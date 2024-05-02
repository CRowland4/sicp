#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them
(define nil '())

(define (square x)
  (* x x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Exercise 2.17
(define (last-pair in-list)
  (let ((but-first (cdr in-list)))
    (if (null? but-first)
      in-list
      (last-pair but-first))))



; Exercise 2.18
(define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))



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

#|  Commenting out so it doesn't slow down the rest of the runs
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
|#



; Exercise 2.20
(define (same-parity x . ints)
  (let ((parity (remainder x 2)))
    (define (iter result items)
      (cond ((null? items) result)
            ((= parity (remainder (car items) 2))
             (iter (append result (list (car items))) (cdr items)))
            (else (iter result (cdr items)))))
    (iter '() ints)))



; Exercise 2.21
(define (square-list1 items)
  (if (null? items)
      nil
      (cons (square (car items)) (square-list1 (cdr items)))))
(define (square-list2 items)
  (map (lambda (x) (* x x)) items))



; Exercise 2.22
(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))  ; <answer> is initially empty. These two lines are combining the first element if items with the answer in a "right to left" way,
                    answer))))             ;    so the list gets reversed
  (iter items nil))

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer  ; This line is using cons on a list and a number, so it will produce a nest of lists rather than a single list. It should use append instead
                    (square (car things))))))
  (iter items nil))



; Exercise 2.23
(define (for-each-alt proc items)
  (if (null? items)
      true
      ((lambda (x) (proc (car x)) (for-each-alt proc (cdr x))) items)))  ; Lambdas are



; Exercise 2.24 - Done in LiquidText



; Exercise 2.25
(define first (list 1 3 (list 5 7) 9))
(define second (list (list 7)))
(define third (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

#|
(car (cdr (car (cdr (cdr first))))) -> 7
(car (car second)) -> 7
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr third)))))))))))) -> 7
|#


; Exercise 2.26
#|
(define list-x (list 1 2 3))
(define list-y (list 4 5 6))
(append list-x list-y) -> (1 2 3 4 5 6)
(cons list-x list-y) -> ((1 2 3) 4 5 6)
(list list-x list-y) -> ((1 2 3) (4 5 6))
|#



; Exercise 2.27
#|
  (define (reverse items)
  (if (null? items)
      items
      (append (reverse (cdr items)) (list (car items)))))
  |#

(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else (append (deep-reverse (cdr items)) (list (car items))))))
(define x (list 1 2 (list 1 (list 1 2 3 4 5) 2) 3 4 (list 3 4) 3))
(deep-reverse x)

  