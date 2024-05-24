#lang sicp
; Exercise 3.12
(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (append x y))
z  ; (a b c d)
(cdr x)  ; response 1 - (b)
(define w (append! x y))
w  ; (a b c d)
(cdr x)  ; response 2 - (b c d)

; Box and pointer diagrams in LiquidText



; Exercise 3.13
; (last-pair z), where z is a cycle, will cause an infiniute loop since the break condition
;   of (cdr z) never being null will never be reached.



; Exercise 3.14 - Box and Pointer diagrams in LiquidText
; The mystery procedure is a reverse procedre that reverses a list "in place".
; So w is the reverse of v, and v is the a single-element list whose only element
;   is the first element of the original v.



; Exercise 3.15 in LiquidText



; Exercise 3.16 in LiquidText



; Exercise 3.17
(define (count-pairs x)
  (define (iter pair counted)
    (cond ((or (memq pair counted) (not (pair? pair))) 0)
          (else
           (begin
             (set! counted (if (null? counted)
                               (list pair)
                               (append! counted (list pair))))
             (let ((car-count (iter (car pair) counted)))
               (+ car-count
                  (iter (cdr pair) counted)
                  1))))))
  (iter x '()))



; Exercise 3.18
(define (contains-cycle? items)
  (define (iter pair visited)
    (cond ((null? pair) false)
          ((not (pair? pair)) false)
          ((memq pair visited) true)
          (else
           (begin
             (set! visited (cons pair visited))
             (iter (cdr pair) visited)))))
  (iter items '()))



; Exercise 3.19
          



