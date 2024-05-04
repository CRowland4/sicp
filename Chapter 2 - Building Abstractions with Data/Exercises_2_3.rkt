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



; Exercise 2.55
(car ''abracadabra)
#|
The innter >'abracadabra< is the same as (quote abracadabra).
Then you have '(quote abracadabra), which is a list of symbols, the first of which is 'quote,
   and (car <a list of symbols starting with 'quote>) is going to return 'quote, which is displayed as > quote.
|#