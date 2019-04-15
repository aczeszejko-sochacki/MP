#lang racket

(require rackunit)
(require rackunit/text-ui)

(define (tagged-tuple? tag len x)
  (and (list? x)
       (=   len (length x))
       (eq? tag (car x))))

(define (tagged-list? tag x)
  (and (pair? x)
       (eq? tag (car x))
       (list? (cdr x))))

;; Vars
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)

(define (var<? x y)
  (symbol<? x y))

;; Literals
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

;; Clauses
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

;; Resolves
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

;;; Clause helpers
(define (bool-var-in-n-times bool var clause)
    (let* ([l (lit bool var)]
           [literals (clause-lits clause)]
           [occ (filter (lambda (x) (lits-eq? l x)) literals)])

           (length occ)))

(define (positive-var-in-once? var clause)
  (= 1 (bool-var-in-n-times #t var clause)))

(define (negative-var-in? var clause)
  (<= 1 (bool-var-in-n-times #f var clause)))

(define (positive-var-not-in? var clause)
  (= 0 (bool-var-in-n-times #t var clause)))

(define (literals-not-equal-to-lit bool var clause)
  (let* ([l (lit bool var)]
         [literals (clause-lits clause)])
           
        (filter-not (lambda (x) (lits-eq? l x)) literals)))

;; Returns an axiom or #f if the proof is incorrect
(define (eval-proof pf)

  ;; Evaluate the resolve consisting of two axioms
  ;; Returns an axiom or #f if res is incorrect
  (define (eval r)
    (let* ([var (res-var r)]
           [r-pos (res-proof-pos r)]
           [r-neg (res-proof-neg r)]
           [clause-pos (axiom-clause r-pos)]
           [clause-neg (axiom-clause r-neg)]
           [literals-pos (clause-lits clause-pos)]
           [literals-neg (clause-lits clause-neg)])
           
           (if (and (positive-var-in-once? var clause-pos)
                    (negative-var-in? var clause-neg)
                    (positive-var-not-in? var clause-neg))

                ; Resolve is correct
                (let ([rest-pos (literals-not-equal-to-lit #t var clause-pos)]
                      [rest-neg (literals-not-equal-to-lit #f var clause-neg)])

                      (if (equal? rest-pos rest-neg)
                          (if (and (null? rest-pos)
                                   (null? rest-neg))

                                   ; Empty clause
                                   (axiom (clause))

                                   ; p or p is p
                                   (axiom (clause (flatten rest-pos))))

                          (axiom (clause (flatten (append rest-pos rest-neg))))))

                ; Resolve is incorrect
                #f)))

  (let ([var (res-var pf)]
        [res-pos (res-proof-pos pf)]
        [res-neg (res-proof-neg pf)])

        ; End if any axiom is empty (no vars satisfies)
        (define (end? pos)
          (and (axiom? pos)
               (null? (clause-lits (axiom-clause res-pos)))))

        (if (or (end? res-pos)
                (end? res-neg))

            ; Return an empty axiom
            (axiom (clause))

            ; Rec
            (cond [(and (axiom? res-pos)
                        (axiom? res-neg)) (eval pf)]
                  [(axiom? res-pos)       (eval (res var
                                                    res-pos
                                                    (eval-proof res-neg)))]
                  [(axiom? res-neg)       (eval (res var
                                                    (eval-proof res-pos)
                                                    res-neg))]
                  [(and (res? res-pos)
                        (res? res-neg))   (eval (res var
                                                    (eval-proof res-pos)
                                                    (eval-proof res-neg)))]
                  [else #f]))))

;;; CNF helpers 
(define (clause-in-cnf? cl p)
  (let ([clauses (cnf-clauses p)])
       (ormap (lambda (x) (eq? x cl)) clauses)))

(define (leaves-in-cnf? pf prop)
   (if (axiom? pf)
       (clause-in-cnf? (axiom-clause pf) prop)
       (and (leaves-in-cnf? (res-proof-pos pf) prop)
            (leaves-in-cnf? (res-proof-neg pf) prop))))

;; Main
(define (proof-result pf prop)
  (let ([result (eval-proof pf)])
       (if (and (axiom? result)
                (leaves-in-cnf? pf prop))
           
           ; Everything ok
           (axiom-clause result)

           ; Evaluation failed
           #f)))

(define (check-proof? pf prop)
  (let ((c (proof-result pf prop)))
    (and (clause? c)
         (null? (clause-lits c)))))

;;; Tests
;; Init vars
(define cl-1 (clause (pos 'p) (pos 'q)))
(define cl-2 (clause (neg 'p) (pos 'q)))
(define cl-3 (clause (neg 'q) (pos 'r)))
(define cl-4 (clause (neg 'q) (neg 'r)))
(define cl-5 (clause (neg 'p)))

(define pf-false-2-depth (res 'q (res 'p (axiom cl-1)
                                         (axiom cl-2))
                                 (res 'r (axiom cl-3)
                                         (axiom cl-4))))

(define pf-not-false-2-depth (res 'q (axiom cl-1)
                                     (res 'r (axiom cl-3)
                                             (axiom cl-4))))

(define prop-cnf-false-2-depth (cnf cl-1 cl-2 cl-3 cl-4))
(define prop-cnf-not-false-2-depth (cnf cl-1 cl-3 cl-4))
(define prop-cnf-clause-not-in (cnf cl-1 cl-2 cl-3))

;; Define tests
(define tests

  (test-suite
    ""
    (check-true (positive-var-in-once? 'p cl-1)
                "positive var inluded exactly once")

    (check-false (positive-var-in-once? 'r cl-1)
                 "positive var not included exactly once")

    (check-true (negative-var-in? 'p cl-2)
                "negative var inluded at least once")

    (check-false (negative-var-in? 'r cl-1)
                 "negative var not included at least once")

    (check-true (positive-var-not-in? 'p cl-2)
                "positive var not included")

    (check-false (positive-var-not-in? 'p cl-1)
                 "positive var included (unexpected)")

    (check-true (clause-in-cnf? cl-1 prop-cnf-false-2-depth)
                "clause included in cnf")

    (check-false (clause-in-cnf? cl-5 prop-cnf-false-2-depth)
                 "clause not included in cnf")

    (check-equal? (literals-not-equal-to-lit #t 'p cl-1 ) (list (lit #t 'q))
                  "literals not equal to a literal")

    (check-true (check-proof? pf-false-2-depth prop-cnf-false-2-depth)
                "test case defined in task consisting 4 clauses should pass")

    (check-false (check-proof? pf-not-false-2-depth prop-cnf-not-false-2-depth)
                 "test case defined in task consisting 3 clauses should not pass")

    (check-false (check-proof? pf-false-2-depth prop-cnf-clause-not-in)
                 "test case where not all clauses in cnf should not pass")))

;; Invoke all tests
(run-tests tests)