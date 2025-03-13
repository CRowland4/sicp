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

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (add-streams s1 s2) (stream-map-generalized + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

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



; Exercise 3.52
(define sum 0)
(define (accum x)
  (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))

(stream-ref y 7)
(display-stream z)

#| a)

; 1
(stream-ref y 7)

; 2
(stream-ref (stream-filter even? seq)
            7)

; 3
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 1 20)))
            7)

; 4
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (cons-stream
                                        1
                                        (stream-enumerate-interval 2 20))))
            7)

; 5
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (1 . (delay (stream-enumerate-interval 2 20)))))
            7)

; 6
(stream-ref (stream-filter even?
                           ((accum 1) . (delay (stream-map accum
                                                           (stream-cdr
                                                            (1 . (delay (stream-enumerate-interval 2 20))))))))
            7)

; 7 -- after this step, sum is 1
(stream-ref (stream-filter even?
                           ((accum 1) . (delay (stream-map accum
                                                           (stream-enumerate-interval 2 20)))))
            7)

; 8
(stream-ref (stream-filter even?
                           (1 . (delay (stream-map accum
                                                   (stream-enumerate-interval 2 20)))))
            7)

; 9
(stream-ref (stream-filter even? (stream-cdr (1 . (delay (stream-map accum
                                                   (stream-enumerate-interval 2 20))))))
            7)

; 10 - This is identical to step 3, but now our enumerate-interval has been incremented by 1
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 2 20)))
            7)

; 11
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (cons-stream
                                        2
                                        (stream-enumerate-interval 3 20))))
            7)

; 12
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (2 . (delay (stream-enumerate-interval 3 20)))))
            7)

; 13
(stream-ref (stream-filter even?
                           (cons-stream (accum 2)
                                        (stream-map accum
                                                    (stream-cdr (2 . (delay (stream-enumerate-interval 3 20)))))))
            7)

; 14 - after this step, sum is 3
(stream-ref (stream-filter even?
                           ((accum 2) . (delay (stream-map accum
                                                           (stream-eneumerate-interval 3 20)))))
            7)

; 15
(stream-ref (stream-filter even?
                           (3 . (delay (stream-map accum
                                                   (stream-enumerate-interval 3 20)))))
            7)

; 16
(stream-ref (stream-filter even?
                           (stream-cdr (delay (stream-map accum
                                                   (stream-enumerate-interval 3 20)))))
            7)

; 17
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 3 20)))
            7)

; Now I see the pattern and we can start to generalize:
; <sum> -- <low part of enumerate-interval>
; 6 -- 4   This is actually the first even number, as the previous 2 were 1 and 3
; 10 -- 5
; 15 -- 6
; 21 -- 7
; 28 -- 8
; 36 -- 9
; 45 -- 10
; 55 -- 11
; 66 -- 12
; 78 -- 13
; 91 -- 14
; 105 -- 15
; 120 -- 16
; 136 -- 17
; 153 -- 18
; 171 -- 19
; 190 -- 20
; 210 -- 21    Finished, because 21 > 20

; So the value of sum is 210, and the 7th indexed element of the filter process is 136

; Turns out I was right about the 7th index being 136, but from step 1 I started simplifying wrong because
;  I started evaluating y befor substituting y and 7 into stream-ref, so I calculated sum incorrectly. Corrected version below.

; 1
(stream-ref y 7)

; 2
(stream-ref (stream-cdr y)
            6)

; 3
(stream-ref (stream-cdr (stream-cdr y))
            5)

; 4
(stream-ref (stream-cdr (stream-cdr (stream-cdr y)))
            4)

; 5
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))
            3)

; 6
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y)))))
            2)

; 7
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))))
            1)

; 8
(stream-ref (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y)))))))
            0)

; 9
(stream-car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))))))

; 10
(car (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr (stream-cdr y))))))))

; And now I see my mistake - in my initial set of steps, I didn't actually begin incorrectly. Steps 1-17 are correct,
;   as are this most recent set of steps 1-11. The mistake was in my comment that I see the pattern. What I had seen
;   was the pattern for odd numbers going through the filter check, but if I had gone through one more iteration,
;   I would have hit the first even number: 6. This would have resulted in a pair being returned from the call to
;   filter, rather than just the result of another call to filter. So below I pic up where I left off from the
;   initial set of steps.

; 17
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (stream-enumerate-interval 3 20)))
            7)

; ... 18
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (cons-stream
                                        3
                                        (stream-enumerate-interval 4 20))))
            7)

; 19
(stream-ref (stream-filter even?
                           (stream-map accum
                                       (3 . (delay (stream-enumerate-interval 4 20)))))
            7)

; 20 - sum is 6 after this step
(stream-ref (stream-filter even?
                           (cons-stream (accum 3)
                                        (stream-map accum
                                                    (stream-enumerate-interval 4 20))))
            7)

; 21
(stream-ref (stream-filter even?
                           (6 . (stream-map accum
                                            (stream-enumerate-interval 4 20))))
            7)

; 22 - and this is where things change, because the even? predicate will return true now
(stream-ref (cons-stream 6
                         (stream-filter
                          even?
                          (delay (stream-map accum
                                            (stream-enumerate-interval 4 20)))))
            7)

; 23 - There is not a delay behind the next call to stream-filter, so instead of doing another round of the filter,
;        we back up even further and do a round of stream-ref, which I didn't do before incorrectly generalizing in
;        in my first set of steps.
(stream-ref (6 . (delay (stream-filter
                         even?
                         (delay (stream-map accum
                                            (stream-enumerate-interval 5 20))))))
            7)

; 24
(stream-ref (stream-cdr (6 . (delay (stream-filter
                         even?
                         (delay (stream-map accum
                                            (stream-enumerate-interval 5 20)))))))
            6)

; Now it's clear to see that ever even number passing throug filter will decrement the ref index by one, meaning the
;  process will be stopped after the 7th indexed even number, which will subsequently stop the accumulation. So it makes
;  sense now that sum will only be 136 after this process, not 210.


b)

; 1
(display-stream z)

; 2
(stream-for-each display-line z)

; 3
(stream-for-each display-line
                 (stream-filter (lambda (x) (= (remainder x 5) 0))
                                seq))

; 4
(stream-for-each display-line
                 (stream-filter (lambda (x) (= (remaiinder x 5) 0))
                                (stream-map accum
                                            (stream-enumerate-interval 1 20))))

; Now there's no 7th index (or any index) that will stop the process at any point, so the enumeration will actually happen through 20.
;  But since in the problem this is called after (stream-ref y 7), the sum will start at 136, rather than at 0. So once again the numbers
;  will be as follows:
; <sum> -- <low part of enumerate-interval>
; 136 - 1
; 137 - 2
; 139 - 3
; 142 - 4
; 146 - 5
; 151 - 6
; 157 - 7
; 164 - 8
; 172 - 9
; 181 - 10
; 191 - 11
; 202 - 12
; 214 - 13
; 227 - 14
; 241 - 15
; 256 - 16
; 272 - 17
; 289 - 18
; 307 - 19
; 326 - 20
; 346 - 21

; No numbers here are divisible by 5, so nothing will be printed to the screen, and the sum will be 346;

Note that these numbers assume we are NOT using the memoization. With memoization, the two calls would not both add to sum,
  because the values generated for the stream by the first call would be stored, and when called, those values would be returned
  without executing the delayed procedure and adding to sum. With memoization, 10, 15, 45, 55, 105, 120, 190, and 210 would be
  printed to the screen, and sum would be 210.

I believe I also could have missed something here since I did not consider the calculation steps that would be taken upon
  defining the structures used for the problem. While that will change the value of the outcome, it won't change the mechanics
  of the problem, so I don't feel the need to redo the whole thing to correct that bit
|#



; Exercise 3.53
(define s (cons-stream 1 (add-streams s s)))
; This stream will generate: 1, 2, 4, 8, 16 ...



; Exercise 3.54
(define (mul-streams s1 s2) (stream-map-generalized * s1 s2))
(define factorials
  (cons-stream 1 (mul-streams factorials (add-streams integers ones))))



; Exercise 3.55
(define (partial-sums s)
  (add-streams (cons-stream 0 (partial-sums s)) s))
  