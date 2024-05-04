#lang scheme
; Exercise 2.53
(list 'a 'b 'c) ; (a b c)
(list (list 'george)) ; ((george))
(cdr '((x1 x2) (y1 y2))) ; ((y1 y2))
(cadr '((x1 x2) (y1 y2))) ; (y1 y2)
(pair? (car '(a short list))) ; #f
(memq 'red '((red shoes) (blue socks))) ; #f
(memq 'red '(red shoes blue socks)) ; (red shoes bue socks)



; Exercise 2.54
(define (equal? list1 list2)
  (cond ((or (and (not (pair? list1)) (pair? list2))
             (and (not (pair? list2)) (pair? list1)))
         false)
        ((and (not (pair? list1)) (not (pair? list2)) (eq? list1 list2))
         true)
        (else (and (equal? (car list1) (car list2))
                   (equal? (cdr list1) (cdr list2))))))
(equal? '(this is a list) '(this is a list)) ; #t
(equal? '(this is a list) '(this (is a) list)) ; #f



