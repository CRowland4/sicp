#lang sicp
; Everything between here and the row of semi-colons is defined for access, and isn't in the order in which it's presented in the book

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (display-line x) (newline) (display x))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (show x)
  (display-line x)
  x)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 3.50
(define (stream-map-generalized proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map-generalized
              (cons proc (map stream-cdr argstreams))))))



; Exercise 3.51
; a.)
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))  ;  0
#|
After printing 0, because (show 0) is called during the defining of x, my initial answer is that the following is x:

              (0 . (delayed stream-map show (stream-cdr (delayed stream-enumerate-interval 1 10))))

I was right, basically, but the interpreter simplye displays '(0 . #<promise>)'
|#

; b.)
(stream-ref x 5)
#|
My initial solution. Since x is defined as (0 . #<promise>), and we're not explicitly showing x, it shouldn't be
printed here. (Well, it shouldn't be printed again after the initial printing of 0 when x is defined).
(1 . #<promise>)
(2 . #<promise>)
(3 . #<promise>)
(4 . #<promise>)
(5 . #<promise>)

I forgot that show was an argument of stream-map, and isn't mapping the whole promise. So this actually prints

1
2
3
4
55

The extra 5 is the actual returned value of the (stream-ref x 5).
|#

; c.)
(stream-ref x 7)
#|
Based on the previous exercise, this should display

1
2
3
4
5
6
77

So this is right when evaluated alone, but when evaluated AFTER (stream-ref x 5), this only outputs

6
77

which makes since because x is a stream, and has already gone through the first 5 numbers with (stream-ref x 5).
|#












