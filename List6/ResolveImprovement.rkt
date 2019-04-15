#lang racket

(require rackunit)
(require rackunit/text-ui)

;; procedury pomocnicze
(define (tagged-tuple? tag len x)
  (and (list? x)
       (=   len (length x))
       (eq? tag (car x))))

(define (tagged-list? tag x)
  (and (pair? x)
       (eq? tag (car x))
       (list? (cdr x))))

;; reprezentacja formuł w CNFie
;; zmienne
(define (var? x)
  symbol? x)

(define (var x)
  x)

(define (var-name x)
  x)

(define (var<? x y)
  (symbol<? x y))

;; literały
(define (lit pol var)
  (list 'lit pol var))

(define (pos x)
  (lit true (var x)))

(define (neg x)
  (lit false (var x)))

(define (lit? x)
  (and (tagged-tuple? 'lit 3 x)
       (boolean? (second x))
       (var? (third x))))

(define (lit-pol l)
  (second l))

(define (lit-var l)
  (third l))

(define (lits-eq? lit-1 lit-2)
  (and (lit? lit-1)
       (lit? lit-2)
       (eq? (lit-pol lit-1) (lit-pol lit-2))
       (eq? (lit-var lit-1) (lit-var lit-2))))

;; klauzule
(define (clause? c)
  (and (tagged-list? 'clause c)
       (andmap lit? (cdr c))))

(define (clause . lits)
  (cons 'clause lits))

(define (clause-lits c)
  (cdr c))

(define (cnf? f)
  (and (tagged-list? 'cnf f)
       (andmap clause? (cdr f))))

(define (cnf . clauses)
  (cons 'cnf clauses))

(define (cnf-clauses f)
  (cdr f))

;; definicja rezolucyjnych drzew wyprowadzenia
(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))

(define (axiom c)
  (list 'axiom c))

(define (axiom-clause a)
  (second a))

(define (res? p)
  (tagged-tuple? 'resolve 4 p))

(define (res x pf-pos pf-neg)
  (list 'resolve x pf-pos pf-neg))

(define (res-var p)
  (second p))
(define (res-proof-pos p)
  (third p))
(define (res-proof-neg p)
  (fourth p))

(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))

;; Wewnętrzna reprezentacja klauzul
(define (sorted? ord? xs)
  (or (null? xs)
      (null? (cdr xs))
      (and (ord? (car xs)
                (cadr xs))
           (sorted? ord? (cdr xs)))))

(define (sorted-varlist? xs)
  (and (andmap var? xs)
       (sorted? var<? xs)))

(define (res-clause pos neg pf)
  (list 'res-clause pos neg pf))

(define (res-clause-pos rc)
  (second rc))
(define (res-clause-neg rc)
  (third rc))
(define (res-clause-proof rc)
  (fourth rc))

(define (res-clause? p)
  (and (tagged-tuple? 'res-clause 4 p)
       (sorted-varlist? (second p))
       (sorted-varlist? (third  p))
       (proof? (fourth p))))

;; implementacja zbiorów / kolejek klauzul do przetworzenia
(define clause-set-empty
  '(stop () ()))

(define (clause-set-add rc rc-set)
  (define (eq-cl? sc)
    (and (equal? (res-clause-pos rc)
                 (res-clause-pos sc))
         (equal? (res-clause-neg rc)
                 (res-clause-neg sc))))

  (define (add-to-stopped sset)
    (let ((procd  (cadr  sset))
          (toproc (caddr sset)))
      (cond
       [(null? procd) (list 'stop (list rc) '())]
       [(or (memf eq-cl? procd)
            (memf eq-cl? toproc))
        sset]
       [else (list 'stop procd (cons rc toproc))])))

  (define (add-to-running rset)
    (let ((pd  (second rset))
          (tp  (third  rset))
          (cc  (fourth rset))
          (rst (fifth  rset)))
      (if (or (memf eq-cl? pd)
              (memf eq-cl? tp)
              (eq-cl? cc)
              (memf eq-cl? rst))
          rset
          (list 'run pd tp cc (cons rc rst)))))
  (if (eq? 'stop (car rc-set))
      (add-to-stopped rc-set)
      (add-to-running rc-set)))

(define (clause-set-done? rc-set)
  (and (eq? 'stop (car rc-set))
       (null? (caddr rc-set))))

(define (clause-set-next-pair rc-set)
  (define (aux rset)
    (let* ((pd  (second rset))
           (tp  (third  rset))
           (nc  (car tp))
           (rtp (cdr tp))
           (cc  (fourth rset))
           (rst (fifth  rset))
           (ns  (if (null? rtp)
                    (list 'stop (cons cc (cons nc pd)) rst)
                    (list 'run  (cons nc pd) rtp cc rst))))
      (cons cc (cons nc ns))))
  (if (eq? 'stop (car rc-set))
      (let ((pd (second rc-set))
            (tp (third  rc-set)))
        (aux (list 'run '() pd (car tp) (cdr tp))))
      (aux rc-set)))

(define (clause-set-done->clause-list rc-set)
  (and (clause-set-done? rc-set)
       (cadr rc-set)))

;;; Improvements
(define (lit-in? lit lits)
  (cond [(null? lits) #f]
        [(lits-eq? lit (car lits)) #t]
        [else (lit-in? lit (cdr lits))]))

(define (subsumes? cl-hard cl)
  (let ([cl-hard-lits (clause-lits cl-hard)]
        [cl-lits (clause-lits cl)])

        (andmap (lambda (lit) (lit-in? lit cl-lits))
                cl-hard-lits)))

;;; Tests
;; Init vars
(define cl-hard (clause (pos 'p) (neg 'q)))
(define cl-easy (clause (pos 'p) (neg 'q) (neg 'p)))
(define cl-not-subsuming-hard (clause (pos 'p) (pos 'r)))

;; Define tests
(define tests
  (test-suite
    ""
    (check-true (subsumes? cl-hard cl-easy)
                 "Subsumes should pass if hard is a subset of easy")

    (check-false (subsumes? cl-hard cl-not-subsuming-hard)
                 "Subsumes should fail if hard is not a subset of the second cl")))

(run-tests tests)