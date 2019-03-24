#lang racket

(require rackunit)
(require rackunit/text-ui)

(define (inc n)
  (+ n 1))

;;; tagged lists
(define (tagged-list? len-xs tag xs)
  (and (list? xs)
       (= len-xs (length xs))
       (eq? (first xs) tag)))

;;; ordered elements
(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

;;; leftist heaps (after Okasaki)

;; data representation
(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (tagged-list? 5 'hnode h)
       (natural? (caddr h))))

(define (make-hnode elem heap-a heap-b)
  (cond [(and (leaf? heap-a) (leaf? heap-b)) (list 'hnode elem 1 leaf leaf)]
        [(leaf? heap-a) (list 'hnode elem 1 heap-b leaf)]
        [(leaf? heap-b) (list 'hnode elem 1 heap-a leaf)]
        [else (let ([a-rank (hnode-rank heap-a)]
                    [b-rank (hnode-rank heap-b)])
                   (if (> a-rank b-rank)
                       (list 'hnode elem (inc b-rank) heap-a heap-b)
                       (list 'hnode elem (inc a-rank) heap-b heap-a)))]))

(define (hnode-elem h)
  (second h))

(define (hnode-left h)
  (fourth h))

(define (hnode-right h)
  (fifth h))

(define (hnode-rank h)
  (third h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (hnode-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (hnode-left h))
           (heap? (hnode-right h))
           (<= (rank (hnode-right h))
               (rank (hnode-left h)))
           (= (rank h) (inc (rank (hnode-right h))))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-left h))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-right h)))))

(define (rank h)
  (if (leaf? h)
      0
      (hnode-rank h)))

;; operations

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-hnode elt leaf leaf)))

(define (heap-min heap)
  (hnode-elem heap))

(define (heap-pop heap)
  (heap-merge (hnode-left heap) (hnode-right heap)))

(define (heap-merge h1 h2)
  (cond [(leaf? h1) h2]
        [(leaf? h2) h1]
        [else (let ([min-h1 (heap-min h1)]
                    [min-h2 (heap-min h2)])
                   (if (< (elem-priority min-h1)
                          (elem-priority min-h2))
                       (make-hnode min-h1
                                   (hnode-left h1)
                                   (heap-merge (hnode-right h1) h2))
                       (make-hnode min-h2
                                   (hnode-left h2)
                                   (heap-merge (hnode-right h2) h1))))]))


;;; heapsort. sorts a list of numbers.
(define (heapsort xs)
  (define (pop-all h)
    (if (heap-empty? h)
        null
        (cons (elem-val (heap-min h)) (pop-all (heap-pop h)))))
  (let ((h (foldl (lambda (x h)
                    (heap-insert (make-elem x x) h))
            empty-heap xs)))
    (pop-all h)))

;;; check that a list is sorted (useful for longish lists)
(define (sorted? xs)
  (cond [(null? xs)              true]
        [(null? (cdr xs))        true]
        [(<= (car xs) (cadr xs)) (sorted? (cdr xs))]
        [else                    false]))

;;; generate a list of random numbers of a given length
(define (randlist len max)
  (define (aux len lst)
    (if (= len 0)
        lst
        (aux (- len 1) (cons (random max) lst))))
  (aux len null))

;;; Tests
(define tests
  (test-suite
    "a"
    ;;; Split tests
    (check-pred sorted? (heapsort (randlist 100 10))
      "is sorted")

    (check-pred heap? (foldl (lambda (x h) (heap-insert (make-elem x x) h)) empty-heap (randlist 20 100))
      "is heap")))

;; Invoke all tests
(run-tests tests)