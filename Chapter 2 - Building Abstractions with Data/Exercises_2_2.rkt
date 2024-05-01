#lang scheme
; Exercise 2.17
(define (last-pair in-list)
  (if (null? (cdr in-list))
      in-list
      (last-pair (cdr in-list))))
(last-pair (list 23 72 149 34))
(last-pair (list 1 2 3 4 5 5 6 7 8 9))



; Exercise 2.18
(define (reverse in-list)
  (if (null? (cdr in-list))
      in-list
      (append (reverse (cdr in-list))
              (list (car in-list)))))
(reverse (list 1 4 9 16 25))
(reverse (list 1 2 3 4 5 6 7 8 9 10 11 12))