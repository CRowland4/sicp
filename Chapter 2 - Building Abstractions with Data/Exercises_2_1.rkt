#lang scheme
; Procedures defined between here and the separator are here so that the exercises can access them
(define (numer x) (car x))

(define (denom x) (cdr x))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (square x) (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (= (remainder x 2) 1))
;------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Exercise 2.1
(define (make-rat n d)
  (let ((g (abs (gcd n d))))
         (if (or (and (< n 0) (< d 0))
          (and (> n 0) (> d 0)))
      (cons (abs (/ n g)) (abs (/ d g)))
      (cons (- (abs (/ n g))) (abs (/ d g))))))



; Exercise 2.2
(define (midpoint-segment s)
  (let ((avg-x (/ (+ (x-point (start-segment s)) (x-point (end-segment s))) 2))
        (avg-y (/ (+ (y-point (start-segment s)) (y-point (end-segment s))) 2)))
    (make-point avg-x avg-y)))
(define (make-segment p1 p2) (cons p1 p2))  ; Segment constructor, takes two points
(define (start-segment s) (car s))  ; Segment selector, takes a segment and returns the start point
(define (end-segment s) (cdr s))  ; Segment selector, takes a segment and returns the end point
(define (make-point x y) (cons x y))  ; Point constructor, takes two integers representing the x and y coordinates
(define (x-point p) (car p))  ; Point selector, takes a point and returns the x coordinate of the point
(define (y-point p) (cdr p))  ; Point selector, takes a point and retrns the y cooridnate of the point
(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ", ")
  (display y-point p)
  (display ")"))



; Exercise 2.3
; Add some helper procedures with points, line segments, and rational numbers
; Distance between two points
(define (point-distance p1 p2)
  (sqrt (+ (square (- (x-point p1)
                      (x-point p2)))
           (square (- (y-point p1)
                      (y-point p2))))))
; Length of a line segment
(define (segment-length s)
  (point-distance (end-segment s) (start-segment s)))
; "Rise" of a line segment
(define (segment-rise s)
  (- (y-point (end-segment s)) (y-point (start-segment s))))
; "Run" of a line segment
(define (segment-run s)
  (- (x-point (end-segment s)) (x-point (start-segment s))))
; Slope of a line segment
(define (segment-slope s)
  (if (= (segment-run s) 0)
      +inf.0
      (/ (segment-rise s) (segment-run s))))
; Reciprocol of a number
(define (reciprocal x)
  (/ 1 x))
; Are line segments perpendicular
(define (segments-perpendicular? s1 s2)
  (or (and (= +inf.0 (segment-slope s1))
           (= 0 (segment-slope s2)))
      (and (= +inf.0 (segment-slope s2))
           (= 0 (segment-slope s1)))
      (= (* -1 (reciprocal (segment-slope s1))) (segment-slope s2))))
; Selectors
(define (side1 r)
  (car r))
(define (side2 r)
  (make-segment (end-segment (car r)) (end-segment (cdr r))))
(define (side3 r)
  (cdr r))
(define (side4 r)
  (make-segment (start-segment (cdr r)) (start-segment (car r))))
(define (length r)
  (let ((distance1 (segment-length (side1 r)))
        (distance2 (segment-length (side2 r))))
    (if (> distance1 distance2)
        distance1
        distance2)))
(define (width r)
  (let ((distance1 (segment-length (side1 r)))
        (distance2 (segment-length (side2 r))))
    (if (< distance1 distance2)
        distance1
        distance2)))
; Rectangle validation - sides must form four 90 degree angles
(define (is-valid-rect? r)
  (and (segments-perpendicular? (side1 r) (side2 r))
       (segments-perpendicular? (side2 r) (side3 r))
       (segments-perpendicular? (side3 r) (side4 r))
       (segments-perpendicular? (side4 r) (side1 r))))
(define (perimeter r)
  (+ (* 2 (length r)) (* 2 (width r))))
(define (area r)
  (* (length r) (width r)))
  
; Constructor of first rectangle implementation - takes 2 line segments that are parallel sides of a rectangle,
;     and returns the pair of segments (s1, s2)
(define (make-rectangle1 s1 s2)
  (let ((rect (cons s1 s2)))
    (if (is-valid-rect? rect)
        rect
        (error "Invalid rectangle"))))
; Test first implementation
(define rec1 (make-rectangle1
               (make-segment (make-point 0 5) (make-point 4 5))
               (make-segment (make-point 0 0) (make-point 4 0))))
(width rec1)
(length rec1)
(perimeter rec1)
(area rec1)

; Constructor of second rectangle implementation - takes 4 points, with the expectation that the connection order is
;     1 -> 2 -> 3 -> 4 -> 1. Returns an the same data shape so that the abstractions barriers are clear.
(define (make-rectangle2 p1 p2 p3 p4)
  (let ((rect (cons (make-segment p1 p2) (make-segment p4 p3))))
    (if (is-valid-rect? rect)
        rect
        (error "Invalid rectangle"))))
; Test second implementation
(define rec2 (make-rectangle2
               (make-point 0 0)
               (make-point 0 5)
               (make-point 4 5)
               (make-point 4 0)))
(width rec2)
(length rec2)
(perimeter rec2)
(area rec2)

; Take away is that there's more to it than is initially obvious to create good abstraction barriers



; Exercise 2.4
#|
(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))

(car (cons x y))
(car (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
x
|#
; With this procedural implementation of cons and car, (car (cons x y)) ultimately reduces down to a procedure that always returns x

; Corresponding cdr

#|
(define (cdr z)
  (z (lambda (p q) q)))

(cdr (cons x y))
(cdr (lambda (m) (m x y)))
((lambda (m) (m x y)) (lambda (p q) q))
((lambda (p q) q) x y)
y
|#



; Exercise 2.5
(define (cons-alt a b)
  (* (expt 2 a) (expt 3 b)))
(define (car-alt p)
  (define (two-dividor current count)
    (if (even? current)
        (two-dividor (/ current 2) (+ count 1))
        count))
  (two-dividor p 0))
(define (cdr-alt p)
  (define (three-dividor current count)
    (if (= (remainder current 3) 0)
        (three-dividor (/ current 3) (+ count 1))
        count))
  (three-dividor p 0))


(define test (cons-alt 1496 429))
(car-alt test)
(cdr-alt test)



; Exercise 2.6
; Zero is a procedure that receives as input a procedure f and applies f 0 times to f's own input.
(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; Simplification as suggested by the exercise
#|
(add-1 zero)
(lambda (f) (lambda (x) (f ((zero f) x))))
(lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x))))
(lambda (f) (lambda (x) (f ((lambda (x) x) x))))
(lambda (f) (lambda (x) (f x)))  ; So this is one
|#
; One is a procedure that receives as input a procedure f and applies f 1 time to f's own input
(define one (lambda (f) (lambda (x) (f x))))

; Therefore it follows that two is a procedure that receives as input a procedure f and applies f 2 times to f's own input
(define two (lambda (f) (lambda (x) (f (f x)))))

; Addition
(define (church+ f g)
  (lambda (h) (lambda (x) ((g h) ((f h) x)))))



; Extended Exercise (2.1.4) - Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound interval)
  (cdr interval))
(define (lower-bound interval)
  (car interval))

; Exercise 2.8
; What we're actually thinking about is the lower bound on the resistance difference of two intervals. The smallest this could be is if the first
;     resistor's resistance value was equal to its lower bound, and if the second resistor's resistance value was equal to its upper bound.
;     Similarly, the max this could be is if the first value was at its maximum and the second value was at its minimum
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

; Exercise 2.9
; Procedure for the width of an interval
(define (interval-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

#|
Process for calculating the width of a sum of two intervals

(interval-width (add-interval x y))
(interval-width (make-interval (+ (lower-bound x) (lower-bound y))
                      (+ (upper-bound x) (upper-bound y))))
(/ (- (+ (upper-bound x) (upper-bound y))
      (+ (lower-bound x) (lower-bound y)))
   2)

Switching to mathematical notation, the above is equivalent to
(up-x + up-y - low-x - low-y) / 2
((up-x - low-x) / 2) + ((up-y - low-y) / 2)
(width-x + width-y)

So the width of the sum of two intervals is the sum of the widths of the two intervals
|#

#|
Process for calculating the width of a difference of two intervals

(interval-width (sub-interval x y)
(interval-width (make-interval (- (lower-bound x) (upper-bound y))
                      (- (upper-bound x) (lower-bound y))))
(/ (- (- (upper-bound x) (lower-bound y))
      (- (lower-bound x) (upper-bound y)))
   2)

And again switching to mathematical notation
(up-x - low-y - low-x + up-y) / 2
((up-x - low-x) / 2) + ((up-y - low-y) / 2)
(width-x + width-y)


So the width of the difference of two intervals is also the sum of the widths of the two intervals
|#

; For multiplication and division, we can't just divide or multiply the widths of the individual intervals.
; Here are two intervals, with widths of 3 and 18 respectively. If these followed the similar patterns, we would expect a width of 54 for the product,
;     and 6 for the solution.
(interval-width (mul-interval (make-interval 4 7) (make-interval 5 23)))  ; 70.5
(interval-width (div-interval (make-interval 5 23) (make-interval 4 7)))  ; 2.518

; Exercise 2.10
; We also check for one of the bounds of y being zero, because that is also not able to be calculated
(define (div-interval-updated x y)
  (cond ((or (= (upper-bound y) 0)
             (= (lower-bound y) 0))
         (error "Invalid division - one of the bounds of interval y is 0"))
        ((or (and (> (upper-bound x) 0) (< (lower-bound x) 0))
             (and (> (upper-bound y) 0) (< (lower-bound y) 0)))
         (error "Invalid division - one of the intervals spans 0"))
         (mul-interval
          x
          (make-interval (/ 1.0 (upper-bound y))
                         (/ 1.0 (lower-bound y))))))


