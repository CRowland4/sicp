#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else (error "No method for these types"
                                     (list op type-tags))))))
              (error "No method for these types"
                     (list op type-tags)))))))

(define (put op type item)  ; Placeholder for put operation, discussed more in chapter 3
  "foo")

(define (get op type item)  ; Placeholder for get operation, discussed more in chapter 3
  "bar")

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (real-part-ri z)
  (car z))

(define (imag-part-ri z)
  (cdr z))

(define (get-coercion type1 type2)  ; Placeholder for get-coercion operation, discussed more in chapter 3
  ("baz"))

(define (put-coercion type1 type2 item)  ; Placeholder for put-coercion operation, discussed more in chapter 3
  ("bam?"))

(define (tag x)  ; Defined here as a placeholder since the real <tag> procedures are internal to installation-packages
  ("tags an object"))

; Package for handling rational numbers
(define (install-rational-package)
  ; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)
; Creating "tagged" rational numbers
(define (make-rational n d)
  ((get 'make 'rational) n d))

; Creating "tagged" complex numbers
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (add x y)
  (apply-generic 'add x y))
(define (sub x y)
  (apply-generic 'sub x y))
(define (mul x y)
  (apply-generic 'mul x y))
(define (div x y)
  (apply-generic 'div x y))

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



; Exercise 2.79
; These two procedures would be added to the scheme-number package (without the suffix after "equ?")
(define (equ?-scheme x y)
  (= x y))
(put 'equ? ('scheme-number 'scheme-number) equ?-scheme)

; These two procedures would be added to the rational-number package (without the suffix after "equ?")
(define (equ?-rational x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))
(put 'equ? ('rational 'rational) equ?-rational)

; These two procedures would be added to the complex-number package (without the suffix after "equ?")
(define (equ?-complex z1 z2)
  (and (= (real-part z1) (real-part z2))
       (= (imag-part z1) (imag-part z2))))
(put 'equ? ('complex 'complex) equ?-complex)

; Full generic equ? procedure
(define (equ? x y)
  (apply-generic 'equ? x y))



; Exercise 2.80
; These two procedures would be added to the scheme-number package (without the suffix after "=zero?")
(define (=zero?-scheme x)
  (= x 0))
(put '=zero? '(scheme-number) =zero?-scheme)

; These two procedures would be added to the rational-number package (without the suffix after "=zero?")
(define (=zero?-rational x)
  (= (numer x) 0))
(put '=zero? '(rational) =zero?-rational)

; These two procedures would be added to the complex-number package (without the suffix after "=zero?")
(define (=zero?-complex z)
  (and (= (real-part z) 0)
       (= (imag-part z) 0)))
(put '=zero? '(complex) =zero?-complex)

; Full generic =zero? procedure
(define (=zero? x)
  (apply-generic '=zero? x))



; Exercise 2.81
; Suggested additions to the coercion table
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number
              'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

; Example situation - we have this generic procedure
(define (exp x y) (apply-generic 'exp x y))
; and this is added to the Scheme-number package, and called with two complex numbers
(put 'exp '(schem-number scheme-number)
     (lambda (x y) (tag (expt x y))))
#|
a) This would cause an infinite recursive loop, where the first complex number is constantly being "converted" to a complex number,
     and passed back to apply-generic.

b) Yes, he is correct. If the object-type table doesn't contain a procedure for a pair of the particular type, an error will be thrown.
|#

; c)  
(define (apply-generic-same-type-coercion op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags)))
                (if (= type1 type2)
                    (error "No method for these types"
                           (list op type-tags))
                    (let ((a1 (car args))
                          (a2 (cadr args))
                          (t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else (error "No method for these types"
                                     (list op type-tags)))))))
          (error "No method for these types" (list op type-tags)))))))



; Exercise 2.82
(define (apply-generic-generalized op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((possible-conversions (map
                                       (lambda (x) (get-coercion-list x type-tags (list (lambda (x) x))))
                                       type-tags)))  ; This will return a list of conversion procedure lists
            (let ((possible-args (arg-possibilites args possible-conversions)))  ; This retrns a list of lists, where each list is potential objects for that arg
              (let ((procedure-and-args (proc-finder op possible-args '())))
                (if (not (null? procedure-and-args))
                    (let ((solution-proc (car procedure-and-args))
                          (solution-args (cadr procedure-and-args)))
                      (apply solution-proc (map contents solution-args)))
                    (error "No method for these types" (list op type-tags))))))))))

; Takes a type and a list of types  (possible-to-types) and returns a list of the conversions that can be used on <type>
(define (get-coercion-list type possible-to-types conversions)
  (if (null? possible-to-types)
      conversions
      (let ((conversion (get-coercion type (car possible-to-types))))
        (if conversion
            (get-coercion-list type (cdr possible-to-types) (append conversions (list conversion)))
            (get-coercion-list type (cdr possible-to-types) conversions)))))

; args is a list of objects, and conversions is a list of conversion lists.
; The procedure returns a list, where every element is a list of possible objects the corresponding argument could be after applying possible conversions
(define (arg-possibilites args conversions) 
  (define (combo-iter args conversions result)
    (if (null? args)
        result
        (combo-iter (cdr args) (cdr conversions) (append result (list (map (lambda (proc) (proc (car args))) (car conversions)))))))
  (combo-iter args conversions '()))

; This procedure returns (procedure args) if procedure is found, false otherwise
(define (proc-finder op potential-args args)
  (if (null? potential-args)
      (let ((solution (get op (map type-tag args))))
        (if solution
            (list solution args)
            false))
      (if (not (null? (car potential-args)))
          (let ((result (proc-finder (cdr potential-args) (append (args) (list (car (car potential-args)))))))
            (if result
               result
               (proc-finder (append (list (cdr (car potential-args))) (cdr potential-args)) args)))
         false)))

#|
As an example of when the proposed book strategy wouln't work, consider a "tower" of objects A->B->C, and there exists
  an operation that accepts 3 arguments of types A, B, and C, respectively.

If I pass arguments into this procedure of types B, B, and C, we won't ever end up at the A, B, C combination that the
  proceure would accept. We would only try A A A, B B B, and C C C

For the two argument version, we could have a tower of the form A->B, and an operation that takes 2 arguments of types A and B,
  respectively. Passing arguments of type A and A into this procedure will never attempt the combination A B. Only A A and B B
  woul be tried.
|#



; Exercise 2.83
; Creating the coercion procedures and adding them to the coercion table
(define (integer->rational num)
  (make-rational num 1))
(put-coercion 'scheme-number 'rational integer->rational)

(define (rational->real rat)
  (list 'real (/ (numer rat) (denom rat))))
(put-coercion 'rational 'real rational->real)

(define (real->complex num)
  (make-complex-from-real-imag num 0))
(put-coercion 'real 'complex real->complex)

; To install a generic raise procedure, we would add the below procedures to the corresponding package of the argument type
; Suffixes would be removed, added here only to show distinction
; We also have to add each one to the 'raise operation in the op-type table
(define (raise-integer num)
  ((get-coercion 'scheme-number 'rational) num))
(put 'raise '(scheme-number) raise-integer)

(define (raise-rational rat)
  ((get-coercion 'rational 'real) rat))
(put 'raise '(rational) raise-rational)
(define (raise-real real)
  ((get-coercion 'real 'complex) real))
(put 'raise '(real) raise-real)

; Final generic raise operation
(define (raise num)
  (apply-generic 'raise num))



; Exercise 2.84
; Since this problem wants the arguments to be coerced from the same type, we'll use the non-generalized apply-generic as a starting point
(define (type-table type)
  (cond ((eq? type 'scheme-number) 0)
        ((eq? type 'real) 1)
        ((eq? type 'rational) 2)
        ((eq? type 'complex) 3)
        ((eq? type 'rectangular) 3)
        ((eq? type 'polar) 3)
        (else (error "Type not recognized" type))))

(define (apply-generic-raise op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond (= type1 type2)
                      (error "No method for these types" (list op type-tags))
                      ((>  (type-table type1) (type-table type2))
                       (apply-generic-raise op a1 (raise a2)))
                      (else (apply-generic-raise op (raise a1) a2))))
              (error "No method for these types"
                     (list op type-tags)))))))



; Exercise 2.85
; To install a generic project procedure, we would add the below procedures to the corresponding package of the argument type
; Suffixes would be removed, added here only to show distinction
; We also have to add each one to the 'project operation in the op-type table
(define (project-complex num)
  (list 'real (real-part (contents num))))
(put 'project 'complex project-complex)

(define (project-real num)
  (make-rational (round (contents num)) 1))
(put 'project 'real project-real)

(define (project-rational rat)
  (round (/ (numer rat) (denom rat))))
(put 'project 'rational project-rational)

; To install a generic drop procedure, we would add the below procedures to the corresponding package of the argument type
; Suffixes would be removed, added here only to show distinction
; We also have to add each one to the 'drop operation in the op-type table
(define (drop-complex num)
  (list 'real (* 1.0 (real-part num))))
(put 'drop 'complex drop-complex)

(define (drop-real num)
  (make-rational (round (contents num)) 1))
(put 'drop 'real drop-real)

(define (drop-rat rat)
  (numer rat))
(put 'drop 'rational drop-rat)

; Generic project and drop
(define (project num)
  (apply-generic 'project num))
(define (drop num)
  (cond ((eq? (type-tag num) 'scheme-number) num)
        ((equ? (project (raise num)) num)
         (drop (apply-generic 'drop num)))
        (else num)))
; Add the equ? procedure to the reals
(define (equ?-real num1 num2)
  (= (car (contents num1)) (car (contents num2))))
(put 'equ? ('real 'real) equ?-real)

; I'll use my fully generalized apply-generic procedure here
(define (apply-generic-generalized-simplify op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc         
          (drop (apply proc (map contents args)))
          (let ((possible-conversions (map
                                       (lambda (x) (get-coercion-list x type-tags (list (lambda (x) x))))
                                       type-tags)))  ; This will return a list of conversion procedure lists
            (let ((possible-args (arg-possibilites args possible-conversions)))  ; This retrns a list of lists, where each list is potential objects for that arg
              (let ((procedure-and-args (proc-finder op possible-args '())))
                (if (not (null? procedure-and-args))
                    (let ((solution-proc (car procedure-and-args))
                          (solution-args (cadr procedure-and-args)))
                      (drop (apply solution-proc (map contents solution-args))))
                    (error "No method for these types" (list op type-tags))))))))))



; Exercise 2.86
; To implement this change, we update all the operations done the different types of numbers inside their packages to be generic,
;   and also create new generic operations (over ordinary and rational numbers) for sqrt, sin, cos, and atan
; Change square to use generic multiplication
(define (square x)
  (mul x x))

; To install a generic sin, cos, atan, and sqrt procedures, we would add the below procedures to the corresponding package of the argument type
; Suffixes would be removed, added here only to show distinction
; We also have to add each one to the correct operation in the op-type table

; Generic sine
(define (sine-real num)
  ((sin (contents num))))
(define (sine-rat rat)
  (sin (/ (numer rat) (denom rat))))
(put 'sine 'real sine-real)
(put 'sine 'rational sine-rat)
(define (sine num)
  (if (eq? (type-tag num) 'scheme-number)
      (sin num)
      (apply-generic 'sine num)))

; Generic cosine
(define (cosine-real num)
  ((cos (contents num))))
(define (cosine-rat rat)
  (cos (/ (numer rat) (denom rat))))
(put 'cosine 'real cosine-real)
(put 'cosine 'rational cosine-rat)
(define (cosine num)
  (if (eq? (type-tag num) 'scheme-number)
      (cos num)
      (apply-generic 'cosine num)))

; Generic atang
(define (atang-real num)
  ((atan (contents num))))
(define (atang-rat rat)
  (atan (/ (numer rat) (denom rat))))
(put 'atang 'real atang-rat)
(put 'atang 'rational atang-rat)
(define (atang num)
  (if (eq? (type-tag num) 'scheme-number)
      (atan num)
      (apply-generic 'atang num)))

; Generic sqroot
(define (sqroot-real num)
  ((sqrt (contents num))))
(define (sqroot-rat rat)
  (sqrt (/ (numer rat) (denom rat))))
(put 'sqroot 'real sqroot-rat)
(put 'sqroot 'rational sqroot-rat)
(define (sqroot num)
  (if (eq? (type-tag num) 'scheme-number)
      (sqrt num)
      (apply-generic 'sqroot num)))

; Changes to operations used in packages
(define (install-rectangular-package)
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (add (square (real-part z))  ; change the addition to the generic selector add
               (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cos a)) (mul r (sin a))))  ; Change to generic multiplication
  ; Interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ; Internal procedures
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (mul (magnitude z) (cos (angle z))))  ; Change to generic multiplication
  (define (imag-part z)
    (mul (magnitude z) (sin (angle z))))  ; Change to generic multiplication
  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))  ; Change to generic addition
          (atan y x)))
  ; Interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  ; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2)) ; Change to generic addition
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))  ; Change to generic subtraction
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2)) ; Change to generic multiplication
                       (add (angle z1) (angle z2))))  ; Change to generic addition
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))  ; Change to generic division
                       (sub (angle z1) (angle z2))))  ; Change to generic subtraction
  ; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)
  


              


