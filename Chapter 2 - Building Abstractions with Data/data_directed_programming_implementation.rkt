#lang sicp
#|
In 2.4.3, we work as though we have two operations "put" and "get" that store types and operations inside of a 2-dimensional table.
In chapter 3.3.3, we actually get the tools to make that happen. It was difficult to test everything properly from the chapter 2 exercises
   without having a fully functional program, so this is what that is.

Every variation asked for in the exercises won't be present, but anytime someting new is added to the overall "system", it will show up
   here as a working program.
|#

; Below are the procedures needed to make this sytem work
(define (square x)
  (* x x))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)  ; Exercise 2.78
      contents
      (cons type-tag contents)))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      'scheme-number))  ; Exercise 2.78

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      datum))  ; Exercise 2.78


; Here is the machinery that we are given in 3.3.3 for making a local table, with get and put procedures
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record (cdr record) false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable
             (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record
                   (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
        (set-cdr! local-table
                  (cons (list key-1 (cons key-2 value))
                        (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The rectangular complex package
(define (install-rectangular-package)
  ; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  "Rectangular package installed")

; The polar complex package
(define (install-polar-package)
  ; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) (cos (angle z))))
  (define (imag-part z) (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  "Polar package installed")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The scheme-number package
(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)  ; Exercise 2.79
       (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number)  ; Exercise 2.80
       (lambda (x) (= 0 x))) 
  (put 'make 'scheme-number (lambda (x) (tag x)))
  "Scheme-number package installed")

; The rational package
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
  (put 'equ? '(rational rational)  ; Exercise 2.79
       (lambda (x y) (= (* (numer x) (denom y))
                        (* (numer y) (denom x)))))
  (put '=zero? '(rational)  ; Exercise 2.80
       (lambda (x) (= (numer x) 0)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  "Rational number package installed")

; The complex package
(define (install-complex-package)
  ; imported procedures from rectangulr complex and polar complex packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                        (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
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
  (put 'equ? '(complex complex)  ; Exercise 2.79
       (lambda (z1 z2) (and (= (real-part z1) (real-part z2))
                            (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)  ; Exercise 2.80
       (lambda (z) (or (and (= (real-part z) 0)
                            (= (imag-part z) 0))
                       (= (magnitude z) 0))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  "Complex package installed")








; Package installations
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

; Rectangular and polar complex number generics
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
; Adding complex number selectors to complex package (Exercise 2.77)
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)

; Artihmetic generics
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))  ; Exercise 2.79
(define (=zero? x) (apply-generic '=zero? x))  ; Exercise 2.80
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


; Running/Testing everything
; Rectangular complex operations
(define z1 (make-from-real-imag 1 2))
(real-part z1)
(imag-part z1)
(magnitude z1)
(angle z1)
; Polar complex operations
(define z2 (make-from-mag-ang 4 3.14))
(real-part z2)
(imag-part z2)
(magnitude z2)
(angle z2)

; Scheme-number operations
(define a (make-scheme-number 3))
(define b (make-scheme-number 4))
(add a b)
(sub a b)
(mul a b)
(div a b)
(equ? a b)
(equ? a a)
(equ? b b)
(=zero? a)
(=zero? (make-scheme-number 0))

; Rational number operations
(define r (make-rational 3 4))
(define s (make-rational 5 7))
(add r s)
(sub r s)
(mul r s)
(div r s)
(equ? r s)
(equ? r r)
(equ? s s)
(equ? r (make-rational 6 8))
(=zero? r)
(=zero? (make-rational 0 4))

; Complex number operations
(define z3 (make-complex-from-real-imag 1 2))
(define z4 (make-complex-from-mag-ang 4 3.14))
(add z3 z4)
(sub z3 z4)
(mul z3 z4)
(div z3 z4)
(real-part z3)
(real-part z4)
(imag-part z3)
(imag-part z4)
(magnitude z3)
(magnitude z4)
(angle z3)
(angle z4)
(equ? z3 z4)
(eq? z3 z3)
(eq? z4 z4)
(=zero? z4)
(=zero? (make-complex-from-real-imag 0 0))
(=zero? (make-complex-from-mag-ang 0 6.28))
