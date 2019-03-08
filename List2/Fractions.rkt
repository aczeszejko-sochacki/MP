#lang racket

;;; The specification forces to keep all in one file
;;; Math func
(define (abs x)
  (if (< 0 x)
    x
    (- x)))

(define (euclid-dist x y)
  (abs (- x y)))

(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (square x) (* x x))

;; Constant
(define epsilon 0.0001)

;; Main function
(define (fraction-iter an-1 an-2
                       bn-1 bn-2
                       num-gen denom-gen
                       n)

  ;; No need to pass it each iteration
  (define last-frac (/ an-1 bn-1))
  
  (define (converged? current-frac)
    (< (euclid-dist last-frac current-frac) epsilon))

  ;; Helper for calculating A_n and B_n
  (define (gen-new s-1 s-2)
    (+ (* (denom-gen n) s-1)
       (* (num-gen n) s-2)))

  (let ([new-a (gen-new an-1 an-2)]
        [new-b (gen-new bn-1 bn-2)])
       (let ([new-frac (/ new-a new-b)])
            (if (converged? new-frac)
                new-frac
                (fraction-iter new-a an-1
                               new-b bn-1
                               num-gen denom-gen
                               (inc n))))))


;;; Tests for arctg x
;; Core test procedure
(define (make-test x)

  (define (arctg-denom-gen n)
    (- (* 2 n) 1))

  (define (arctg-num-gen n)

    ;; Helper to avoid passing x to fraction-iter
    (define (arctg-num-gen-x x n)
      (if (= n 1)
          x
          (square (* (dec n) 
                  x))))

    (arctg-num-gen-x x n))

  (fraction-iter 0. 1. 1. 0. arctg-num-gen arctg-denom-gen 1))

;;; Invoke the tests
(writeln 'Results:)
(make-test 1)
(make-test 2)
(make-test 3)

;;; Real values
(writeln 'Real:)
(atan 1)
(atan 2)
(atan 3)