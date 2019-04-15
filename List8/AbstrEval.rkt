#lang racket

(require rackunit)
(require rackunit/text-ui)

;;; Define abstract expresions
(struct const (a) #:transparent)
(struct var () #:transparent)
(struct op (symb l r) #:transparent)
(struct deriv (expr) #:transparent)

(define (expr? e)
  (match e
    [(var) true]
    [(const a)    (number? a)]
    [(op s l r)   (and (member s '(+ *))
                       (expr? l)
                       (expr? r))]
    [(deriv expr) (expr? expr)]
    [_            false]))

;;; Evaluate the expression in the abstract notation
(define (eval e val)
  (match e
    [(const a)           a]
    [(var)               val]
    [(op '+ l r)         (+ (eval l val) (eval r val))]
    [(op '* l r)         (* (eval l val) (eval r val))]
    [(deriv (const a))   0]
    [(deriv (var))       1]
    [(deriv (op '+ l r)) (+ (eval (deriv l) val) (eval (deriv r) val))]
    [(deriv (op '* l r)) (+ (* (eval (deriv l) val) (eval r val))
                            (* (eval l val) (eval (deriv r) val)))]
    [_                   (error "Please pass correct expression")]))

;;; Tests
;; Define expressions
(define task-exp-1 (op '+ (const 2) (var)))
(define task-exp-2 (op '+ (op '* (const 2)
                                    (var))
                             (deriv (op '+ (op '* (var)
                                                  (var))
                                           (var)))))

;; d/dx x^4
(define deriv-high-pow-exp (deriv (op '* (op '* (var) (var))
                                         (op '* (var) (var)))))

;; x + x + 2
(define sum (op '+ (op '+ (var) (var))
                   (const 2)))

;; (x + x + 2)^2
(define power-of-sum (op '* sum sum))

;; d/dx (x + x + 2)^2
(define deriv-power-of-sum (deriv power-of-sum))

(define tests
  (test-suite
    ""
    ;;; Checking correctness of expression
    (check-pred expr? task-exp-1
      "First expr defined in the task")

    (check-pred expr? task-exp-2
      "Second expr defined in the task")

    (check-pred expr? deriv-high-pow-exp
      "Custom test: expr d/dx x^4")

    (check-pred expr? deriv-power-of-sum
      "Custom test: expr d/dx (x + x + 2)^2")

    ;;; Eval tests
    (check-equal? (eval task-exp-1 5)  7
      "Eval of the first expr")

    (check-equal? (eval task-exp-2 3) 13
      "Eval of the second expr")

    (check-equal? (eval deriv-high-pow-exp 3) 108
      "Evaluate deriv of x^4")

    (check-equal? (eval sum 10) 22
      "Evaluate x + x + 2")

    (check-equal? (eval power-of-sum 5) 144
      "Evaluate (x + x + 2)^2")

    (check-equal? (eval deriv-power-of-sum 10) 88
      "Evaluate d/dx (x + x + 2)^2 = 2(2x + 2)*2")))

;; Invoke all tests
(run-tests tests)