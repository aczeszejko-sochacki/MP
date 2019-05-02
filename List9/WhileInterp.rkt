#lang racket

(require rackunit)
(require rackunit/text-ui)

(provide (all-defined-out))

(struct variable (x)         #:transparent)
(struct const    (val)       #:transparent)
(struct op       (symb l r)  #:transparent)
(struct let-expr (x e1 e2)   #:transparent)
(struct if-expr  (b t e)     #:transparent)

(define (expr? e)
  (match e
    [(variable s)       (symbol? s)]
    [(const n)          (or (number? n)
                            (boolean? n))]
    [(op s l r)         (and (member s '(+ *))
                             (expr? l)
                             (expr? r))]
    [(let-expr x e1 e2) (and (symbol? x)
                             (expr? e1)
                             (expr? e2))]
    [(if-expr b t e)    (andmap expr? (list b t e))]
    [_                  false]))

(struct skip      ()       #:transparent) ; skip
(struct comp      (s1 s2)  #:transparent) ; s1; s2
(struct assign    (x e)    #:transparent) ; x := e
(struct while     (b s)    #:transparent) ; while (b) s
(struct if-stm    (b t e)  #:transparent) ; if (b) t else e
(struct var-block (x e s)  #:transparent) ; var x := e in s

(define (stm? e)
  (match e
    [(skip) true]
    [(comp s1 s2)   (and (stm? s1) (stm? s2))]
    [(assign x e)   (and (symbol? x) (expr? e))]
    [(while b s)    (and (expr? b) (stm? s))]
    [(if-stm b t e) (and (expr? b) (stm? t) (stm? e))]
    [_ false]))

;;; The new, imperative list schema:
;;; (mcons (mcons a1 a1) (mcons (mcons b1 b2) (mcons ... (mcons (mcons x1 x2) null) ... )) 

;; Equivalent to caar
(define mcaar (lambda (x) (mcar (mcar x))))

;; Equivalent to cdar  
(define mcdar (lambda (x) (mcdr (mcar x))))
  
;; Imperative lookup
(define (imp-lookup x xs)
  (cond [(null? xs)         (error x "unknown identifier :(")]
        [(eq? (mcaar xs) x) (mcdar xs)]
        [else               (imp-lookup x (mcdr xs))]))

;; Imperative update
(define (imp-update x v xs)
  (define (iter tmp)
    (cond [(null? tmp)        (error x "unknown identifier :(")]
          [(eq? (mcaar tmp) x) (set-mcdr! (mcar tmp) v)]
          [else               (iter (mcdr tmp))]))

  (begin (iter xs)
         xs))

;; Need to convert list of operands to rec mcons
(define (op-to-proc x)
  (env-lookup x 
              (foldr mcons 
                     null
                     (map (lambda (x) (mcons (car x) (cadr x)))
                          `((+ ,+)
                            (* ,*)
                            (- ,-)
                            (/ ,/)
                            (%, modulo)
                            (> ,>)
                            (>= ,>=)
                            (< ,<)
                            (<= ,<=)
                            (= ,=)
                            (!= ,(lambda (x y) (not (= x y)))) 
                            (&& ,(lambda (x y) (and x y)))
                            (|| ,(lambda (x y) (or x y))))))))

;; Env with imperative update and lookup
(define (env-empty) null)
(define env-lookup imp-lookup)
(define (env-add x v env) (mcons (mcons x v) env))
(define env-update imp-update)
(define env-discard mcdr)
(define (env-from-assoc-list xs) xs)

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r) ((op-to-proc s) (eval l env)
                                (eval r env))]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]))

(define (interp p m)
  (match p
    [(skip) m]
    [(comp s1 s2) (interp s2 (interp s1 m))]
    [(assign x e)
     (env-update x (eval e m) m)]
    [(while b s)
     (if (eval b m)
         (interp p (interp s m))
         m)]
    [(var-block x e s)
     (env-discard
       (interp s (env-add x (eval e m) m)))]
    [(if-stm b t e) (if (eval b m)
                        (interp t m)
                        (interp e m))]))

;;; Tests
;; Init environment
(define env (env-from-assoc-list (mcons (mcons 'i 4)
                                        (mcons (mcons 'j 3)
                                               (mcons (mcons 'k 2) null)))))

;; Some code in WHILE
(define fact-in-WHILE
  (var-block 'x (const 0)                                           ; var x := 0 in
     (comp (assign 'x (const 1))                                    ;   x := 1
     (comp (while (op '> (variable 'i) (const 0))                   ;   while (i > 0)
              (comp (assign 'x (op '* (variable 'x) (variable 'i))) ;     x := x * i
                    (assign 'i (op '- (variable 'i) (const 1)))))   ;     i := i - 1
           (assign 'i (variable 'x))))))                            ;   i := x

(define (factorial n)
  (env-lookup 'i (interp fact-in-WHILE
                         (env-from-assoc-list (mcons (mcons 'i n) null)))))
                                              
(define tests
  (test-suite
    "Testing correctenss of imperative implementation of the environment"

    (check-equal? (env-lookup 'i env) 4 "Lookup of the first element")

    (check-equal? (env-lookup 'k env) 2 "Lookup of the third element")

    (check-equal? (begin (env-update 'j 10 env)
                         (env-lookup 'j env)) 10 "Lookup after update")
    
    (check-equal? (let ([extended-env (env-add 'p 0 env)])
                        (env-lookup 'p extended-env)) 0 "Lookup after addition")

    (check-equal? (env-discard env) (mcons (mcons 'j 10)
                                           (mcons (mcons 'k 2) null)))

    (check-equal? (factorial 5) 120 "Test evaluation of some code in while")))

;; Invoke all tests
(run-tests tests)