#lang racket

(require rackunit)
(require rackunit/text-ui)

(struct variable     (x)        #:transparent)
(struct const        (val)      #:transparent)
(struct op           (symb l r) #:transparent)
(struct let-expr     (x e1 e2)  #:transparent)
(struct if-expr      (b t e)    #:transparent)
(struct cons-expr    (l r)      #:transparent)
(struct car-expr     (p)        #:transparent)
(struct cdr-expr     (p)        #:transparent)
(struct pair?-expr   (p)        #:transparent)
(struct null-expr    ()         #:transparent)
(struct null?-expr   (e)        #:transparent)
(struct symbol-expr  (v)        #:transparent)
(struct symbol?-expr (e)        #:transparent)

(struct lambda-expr  (xs b)     #:transparent)
(struct closure      (xs b e))
(struct app-expr     (f es)     #:transparent)
(struct apply-expr   (f e)      #:transparent)
(struct list-expr    (xs)       #:transparent)

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
    [(cons-expr l r)    (andmap expr? (list l r))]
    [(car-expr p)       (expr? p)]
    [(cdr-expr p)       (expr? p)]
    [(pair?-expr p)     (expr? p)]
    [(null-expr)        true]
    [(null?-expr p)     (expr? p)]
    [(symbol-expr v)    (symbol? v)]
    [(symbol?-expr p)   (expr? p)]
    [(lambda-expr xs b) (and (andmap symbol? xs)
                             (expr? b)
                             (not (check-duplicates xs)))]
    [(app-expr f es)    (and (expr? f)
                             (andmap expr? es))]
    [(apply-expr f e)   (and (expr? f)
                             (expr? e))]
    [(list-expr xs)     (andmap expr? xs)]
    [_                  false]))

(struct val-symbol (s)   #:transparent)

(define (my-value? v)
  (or (number? v)
      (boolean? v)
      (and (pair? v)
           (my-value? (car v))
           (my-value? (cdr v)))
      (and (symbol? v) (eq? v 'null))
      (and ((val-symbol? v) (symbol? (val-symbol-s v))))
      (and (closure? v)
           (list? closure-xs)
           (expr? closure-b)
           (expr? closure-e))
      (and (list? v)
           (andmap expr? v))))

(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "unknown identifier :(")]
    [(eq? (caar xs) x) (cadar xs)]
    [else (lookup x (cdr xs))]))

(define (op-to-proc x)
  (lookup x `(
              (+ ,+)
              (* ,*)
              (- ,-)
              (/ ,/)
              (> ,>)
              (>= ,>=)
              (< ,<)
              (<= ,<=)
              (= ,=)
              (eq? ,(lambda (x y) (eq? (val-symbol-s x)
                                       (val-symbol-s y))))
              )))

(define (env-empty) null)
(define env-lookup lookup)
(define (env-add x v env) (cons (list x v) env))

(define (env? e)
  (and (list? e)
       (andmap (lambda (xs) (and (list? e)
                                 (= (length e) 2)
                                 (symbol? (first e)))))))

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r)
     ((op-to-proc s) (eval l env)
                     (eval r env))]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]
    [(cons-expr l r)
     (let ((vl (eval l env))
           (vr (eval r env)))
       (cons vl vr))]
    [(car-expr p)      (car (eval p env))]
    [(cdr-expr p)      (cdr (eval p env))]
    [(pair?-expr p)    (pair? (eval p env))]
    [(null-expr)       'null]
    [(null?-expr e)    (eq? (eval e env) 'null)]
    [(symbol-expr v)   (val-symbol v)]
    [(lambda-expr xs b) (closure xs b env)]
    [(app-expr f es) (let ([vf (eval f env)])
                          (match vf
                            [(closure xs b c-env)
                             (if (= (length xs) (length es))
                                 (let ([pairs (map cons xs es)])
                                      (eval b (foldr (lambda (pair env-acc)
                                                             (env-add (car pair)
                                                                      (eval (cdr pair) env)
                                                                      env-acc))
                                                     env
                                                     pairs))
                                      )
                                 (error "Wrong number of arguments :/"))] 
                            [_ (error "Application: not a function :/")]))]
    [(list-expr xs) xs]
    [(apply-expr f e) (eval (app-expr f (eval e env)) env)]))

(define (run e)
  (eval e (env-empty)))

;;; Tests

;; Define test expressions
(define e1 (lambda-expr '(x y z) (const 1)))
(define e2 (app-expr (lambda-expr '(x y)
                                  (op '+ (variable 'x)
                                         (variable 'y)))
                     (list (const 1) (const 2))))
(define e3 (let-expr 'z (const 10)
                     (app-expr (lambda-expr '(x y z)
                                            (op '* (variable 'x)
                                                   (op '+ (variable 'y)
                                                          (variable 'z))))
                               (list (const 2) (const 3) (variable 'z)))))
(define e4 (apply-expr (lambda-expr '(x y z)
                                    (op '+ (variable 'x)
                                           (variable 'z)))
                       (list-expr (list (const 1) (const 2) (const 3)))))

(define e5 (apply-expr (lambda-expr '()
                                    (const 10))
                       (list-expr '())))

(define e6 (apply-expr (lambda-expr '(x)
                                    (cdr-expr (variable 'x)))
                       (list-expr (list (cons-expr (const 1) (const 2))))))

(define tests
  (test-suite
    ""
    (check-true (andmap expr? (list e1 e1 e3 e4 e5 e6))
                "Check if all the expressions with custom methods satisfies expr? pred")

    (check-true (match (run e1)
                  [(closure '(x y z) (const 1) '()) #t]
                  [_ #f])
                "Check if lambda-expr returns expected closure")

    (check-equal? (run e2) 3 "Test app-expr on two constants passed to lambda-expr")

    (check-equal? (run e3) 26 "Test app-expr inside let;
                              check if arguments are passed in expected order")

    (check-equal? (run e4) 4 "Test apply-expr when passed correct (3)
                             number of args to lambda-expr")

    (check-equal? (run e5) 10 "Test apply-expr when no argument required")

    (check-equal? (run e6) 2 "Test apply-expr requiring exactly one argument")))

(run-tests tests)