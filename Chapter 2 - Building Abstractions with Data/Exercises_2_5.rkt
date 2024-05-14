#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.77
#|
Object in question
  > ('complex 'rectangular 3 4)

Procedures to add to the complex package:

(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

This works because it provides a "bridge" from the complex package into the rectangular package for the
   magnitude procedure. Without the put additions, the call to (magnitude z) looks for the procedure magnitude
   inside the first package that is tagged, which is complex. But the complex package doesn't have a procedure
   called magnitude, so an error is thrown. The code above points any call to "magnitude in the complex package"
   to the generic magnitude procedure.

So (magnitude z), where z is the object defined above, does the following:
   1) First it sees the first tag is 'complex, so it passes ('rectangular 3 4) to the procedure
         at the "magnitude" row and "complex" column in our abstraction table. Thanks to the
         put statements above, this procedure is the generic procedure <magnitude>, that
         works on either rectangular or polar complex numbers.
   2) This generic magnitude procedure sees that its argument tag is 'rectangular, so it passes
         (3 4) to the procedure at the "magnitude" row and "rectangular" column in the
         abstraction table. This is the magnitude procedure defined in the rectangular package.
   3) This procedure isn't generic, and so it returns the magnitude of the complex number
         whose real and imaginary parts are 3 and 4, respectively.

This is a recursive process - (magnitude ('complex 'rectangular 3 4)) ends up passing
   ('rectangular 3 4) back to itself.
|#



; Exercise 2.78
; This returns 'scheme-number as the type of a normal number - the number doesn't actually have to
;   be represented as a pair
(define (type-tag datum)
  (cond ((number? datum) 'scheme-number)
        ((pair? datum) (car datum))
        (else (error "Bad datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((number? datum) datum)
        ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum: CONTENTS" datum))))

(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))

