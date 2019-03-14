#lang racket

(require rackunit)
(require rackunit/text-ui)

;;; Math
(define (inc x) (+ x 1))

(define (dec x) (- x 1))

(define (lg x)
  (/ (log x) (log 2)))

;;; List helpers
;; Calculate the length of a list
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (append lst-1 lst-2)
  (if (null? lst-1)
      lst-2
      (cons (car lst-1)
            (append (cdr lst-1) lst-2))))

(define (fold-right op nval lst)
  (if (null? lst)
      nval
      (op (car lst)
          (fold-right op nval (cdr lst)))))

(define (flatten lst)
  (fold-right append null lst))

;; Composing two functions
(define (compose f g)
  (lambda (x) (f (g x))))

;; Repeating func f n times
(define (repeated f n)
  (if (= n 0)
    identity
    (compose f (repeated f (dec n)))))

;;; Core
;; Merge two lists
(define (merge lst-1 lst-2)
  
  (define (comp-heads)
    (< (car lst-1) (car lst-2)))

  (cond ((null? lst-1) lst-2)
        ((null? lst-2) lst-1)
        (else (if (comp-heads)
                  (cons (car lst-1) (merge (cdr lst-1)
                                           lst-2))
                  (cons (car lst-2) (merge (cdr lst-2)
                                           lst-1))))))

;; Split a list
;; The first half has its items in reverse order (not important)
(define (split lst)

  ;; Half of the length of a list
  (define half-len
    (floor (/ (length lst) 2)))

  (define (split-iter pref-lst suff-lst iter-no)
    (if (= iter-no half-len)
        (cons pref-lst (list suff-lst))  ; Return a pair of lists after split
        (split-iter (cons (car suff-lst)
                          pref-lst)
                    (cdr suff-lst)
                    (inc iter-no))))

  (split-iter null lst 0))

;;; ;; Main
(define (mergesort lst)

  (define rec-depth
    (ceiling (lg (length lst))))

  (define (split-once tmp-lst)
    (flatten (map split tmp-lst)))

  ;; Works correctly only for even lengths
  (define (merge-once tmp-lst)
    (if (null? tmp-lst)
        null
        (cons (merge (car tmp-lst)
                     (cadr tmp-lst))
              (merge-once (cddr tmp-lst)))))

  ;; Invoke the methods
  (let ([splitted ((repeated split-once rec-depth) (list lst))]) ; Nest list for map)
    (let ([merged ((repeated merge-once rec-depth) splitted)])
      (car merged))))   ; Merged is of type '('(...))

;;; Tests
;; Split
(define tests
  (test-suite
    "Merge, split and mergesort tests"

    ;;; Split tests
    (check-equal? (split (list 1))
                  (list (list) (list 1))
                  "Split a one-element list")

    (check-equal? (split (list 1 2))
                  (list (list 1) (list 2))
                  "Split a two-element list")
    
    (check-equal? (split (list 1 2 3 4 5 6 7 8 9))
                  (list (list 4 3 2 1) (list 5 6 7 8 9))
                  "Split a common list")

    

  ;; Merge tests
  (check-equal? (merge (list) (list 2))
                (list 2)
                "Merge with an empty list")

  (check-equal? (merge (list 6 7 8) (list 1 2 3 5))
                (list 1 2 3 5 6 7 8)
                "Merge equivalent to append one list to another")

  (check-equal? (merge (list 1 3 5 7) (list 2 4 6 8))
                (list 1 2 3 4 5 6 7 8)
                "Common merge")

  ;; Mergesort
  (check-equal? (mergesort (list 1))
                (list 1)
                "Merge a singleton")

  (check-equal? (mergesort (list 5 8 1 3 9))
                (list 1 3 5 8 9)
                "Merge a list of cardinality not being a power of 2")

  (check-equal? (mergesort (list 1 4 3 235 253 64236 9 -2 3 1980 -3 3))
                (list -3 -2 1 3 3 3 4 9 235 253 1980 64236)
                "Common mergesort with some negative numbers")))

;; Invoke all tests
(run-tests tests)