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

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? x y)
  (< (euclid-dist x y) epsilon))

(define (square x) (* x x))

(define (power x n)
  (cond ((= n 0) 1)
        ((even? n) (square (power x (/ n 2))))
        (else (* x (power x (dec n))))))

;;; Constants
(define epsilon 0.000001)

(define max-iter 1000000)

;;; Miscelanous helpers
;; Fixed point of f
(define (fixed-point f s)

  (define (iter k iter-number)
    (let ((new-k (f k)))
      (if (good-enough? k new-k)
          k

          ;; Check if the method diverges
          (if (< iter-number max-iter)
              (iter new-k (inc iter-number))
              (writeln 'Max-iteration-limit-reached)))))

  (iter s 1))

;; Composing two functions
(define (compose f g)
  (lambda (x) (f (g x))))

;; Repeating func f n times
(define (repeated f n)
  (if (= n 0)
    identity
    (compose f (repeated f (dec n)))))

;; Mean of x and f(x)
(define (average-damp f)
  (lambda (x) (average x (f x))))

;;; Main
(define (nth-root n-times-damp x nth)
  (fixed-point ((repeated average-damp n-times-damp)
                (lambda (y) (/ x (power y (dec nth)))))
                1.0))

;;; Tests
;; It turns out, that the method (probably)
;;converges for n-times-damp = ceil(lg nth)
(nth-root 2 81 (power 2 2))
(nth-root 2 81 (power 2 3))
(nth-root 3 81 (power 2 3))
(nth-root 3 81 (power 2 4))
(nth-root 4 81 (power 2 4))
(nth-root 9 81 (power 2 10))
(nth-root 10 81 (power 2 10))