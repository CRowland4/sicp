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
