#lang racket

;;; The specification forces to keep all in one file
;;; Math func
(define (abs x)
  (if (< 0 x)
    x
    (- x)))

(define (euclid-dist x y)
  (abs (- x y)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

;; To avoid hardcoding
(define min-epsilon 0.00001)

;; Main func
(define (cube-root x epsilon)
  
  (define check-epsilon
    (if (> epsilon min-epsilon) #t #f))
  
  (define (good-precision? approx)
    (if (< (euclid-dist (cube approx) x)
           epsilon)
      true
      false))

  (define (better-approx approx)
     (iter (/ (+ (/ x (square approx))
                  (* 2 approx))
               3)))

  (define (iter approx)
    (if (good-precision? approx)
      approx
      (better-approx approx)))

  ;; Find the result if epsilon is correct
  (if check-epsilon
      (iter x)
      "too small epsilon"))

;;; Tests
;; Asserting for good epsilon
(define (assert-with-good-epsilon x epsilon)
  (< (euclid-dist (cube (cube-root x epsilon)) x) epsilon))

(assert-with-good-epsilon 8 0.01)
(assert-with-good-epsilon 27 0.0001)
(assert-with-good-epsilon 132 1)

;; Asserting for bad epsilon
(define (assert-with-bad-epsilon x epsilon)
  (equal? (cube-root x epsilon) "too small epsilon"))

(assert-with-bad-epsilon 10 -2)
(assert-with-bad-epsilon 10 0)
(assert-with-bad-epsilon 10 0.000001)
