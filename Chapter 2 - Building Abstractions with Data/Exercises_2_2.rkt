#lang scheme
; Exercise 2.17
(define (last-pair in-list)
  (let ((but-first (cdr in-list)))
    (if (null? but-first)
      in-list
      (last-pair but-first))))
(last-pair (list 23 72 149 34))
(last-pair (list 1 2 3 4 5 5 6 7 8 9))



; Exercise 2.18
(define (reverse in-list)
  (let ((but-first (cdr in-list)))
    (if (null? but-first)
      in-list
      (append (reverse but-first)
              (list (car in-list))))))
(reverse (list 1 4 9 16 25))
(reverse (list 1 2 3 4 5 6 7 8 9 10 11 12))



; Exercise 2.19
; Previous change-counting procedure here for reference
#|
(define (count-change amount) (cc amount 5))
(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))
(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
|#
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))
(define (first-denomination coin-values)
  (car coin-values))
(define (except-first-denomination coin-values)
  (cdr coin-values))
(define (no-more? coin-values)
  (null? coin-values))
; The order does not matterbut having the biggest denomination first is more efficient.
; For example, for 100 cents where 50 comes first, we know right from the start that if fifty is used once, the only reasonable numbers to calculate now are 50 cents and lower.
;     If the order was different, and 10 (or any other coin smaller than 50) came first, our process would form a tree that will eventually verify if 50 divides evenly into 90,
;     and then 80, 70, ... and so on. This produces extra steps that were skipped when we removed 50 first, ensuring that we're only going to be iterating through instances where
;     50 can be used to actually make change.
; Example below demonstrates how order doesn't matter


(define us-coins (list 50 25 10 5 1))
(define random-us-coins (list 10 1 25 50 5))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(define random-uk-coins (list 2 5 10 0.5 100 1 20 50))

(cc 100 us-coins)  ; 292
(cc 100 random-us-coins)  ; 292
(cc 100 (reverse us-coins))  ; 292
(cc 100 (reverse random-us-coins))  ; 292
(cc 100 uk-coins)  ; 104561
(cc 100 random-uk-coins)  ; 104561
(cc 100 (reverse uk-coins))  ; 104561
(cc 100 (reverse random-uk-coins))  ; 104561