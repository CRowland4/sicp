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
  (mul x x))  ; Exercise 2.86 - change "*" to "mul"

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)  ; Exercise 2.78
      contents
      (cons type-tag contents)))

(define (apply-generic op . args)  ; Generalized for Exercise 2.82
  ; Takes a type and a list of types (possible-to-types) and returns a list of the conversions that can be used on <type>
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
            (let ((result (proc-finder op (cdr potential-args) (append args (list (car (car potential-args)))))))
              (if result
                 result
                 (proc-finder op (append (list (cdr (car potential-args))) (cdr potential-args)) args)))
           false)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((possible-conversions (map
                                       (lambda (x) (get-coercion-list x type-tags (list (lambda (x) x))))
                                       type-tags)))  ; This will return a list of conversion procedure lists
            (let ((possible-args (arg-possibilites args possible-conversions)))  ; This retrns a list of lists, where each list is potential objects for that arg
              (let ((procedure-and-args (proc-finder op possible-args '())))
                (if procedure-and-args
                    (let ((solution-proc (car procedure-and-args))
                          (solution-args (cadr procedure-and-args)))
                      (apply solution-proc (map contents solution-args)))
                    (error "No method for these types" (list op type-tags))))))))))


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

; And now we make the coercion table from 2.5.2
; This table index on two types, and tries to turn the first type into the second
(define coercion-table (make-table))
(define put-coercion (coercion-table 'insert-proc!))
(define get-coercion (coercion-table 'lookup-proc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The rectangular complex package
(define (install-rectangular-package)
  ; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqroot (add (square (real-part z))  ; Exercise 2.86 - replace "+" with "add" and "sqrt" with "sqroot"
                 (square (imag-part z)))))
  (define (angle z)
    (atang (imag-part z) (real-part z)))  ; Exercise 2.86 - replace "atan" with "atang" and "/" with "div"
  (define (make-from-mag-ang r a)
    (cons (mul r (cos a)) (mul r (sin a))))  ; Exercise 2.86 - replace "*" with "mul"
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
  (define (real-part z) (mul (magnitude z) (cosine (angle z))))  ; Exercise 2.86 - replace "*" with "mul", and cos with cosine
  (define (imag-part z) (mul (magnitude z) (sine (angle z))))  ; Exercise 2.86 - replace "*" with "mul", and sin with sine
  (define (make-from-real-imag x y)
    (cons (sqrt (add (square x) (square y)))  ; Exercise 2.86 - replace "+" with "add"
          (atang y x)))  ; Exercise 2.86 - replace "atan" with "atang"
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
  (put 'sine '(scheme-number)  ; Exercise 2.86
       (lambda (x) (sin (contents x))))
  (put 'cosine '(scheme-number)  ; Exercise 2.86
       (lambda (x) (cos (contents x))))
  (put 'atang '(scheme-number scheme-number)  ; Exercise 2.86
       (lambda (x y) (atan (contents x) (contents y))))
  (put 'sqroot '(scheme-number)  ; Exercise 2.86
       (lambda (x) (sqrt (contents x))))
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
  (put 'sine '(rational)  ; Exercise 2.86
       (lambda (x) (sin (/ (numer x) (denom x)))))
  (put 'cosine '(rational)  ; Exercise 2.86
       (lambda (x) (cos (/ (numer x) (denom x)))))
  (put 'atang '(rational rational)  ; Exercise 2.86
       (lambda (x y) (atan (/ (numer x) (denom x))
                           (/ (numer y) (denom y)))))
  (put 'sqroot '(rational)  ; Exercise 2.86
       (lambda (x) (sqrt (/ (numer x) (denom x)))))
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
    (make-from-real-imag (add (real-part z1) (real-part z2))  ; Exercise 2.86 - replace "+" with "add"
                         (add (imag-part z1) (imag-part z2))))  ; Exercise 2.86 - replace "+" with "add"
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))  ; Exercise 2.86 - replace "-" with "sub"
                         (sub (imag-part z1) (imag-part z2))))  ; Exercise 2.86 - replace "-" with "sub"
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))  ; Exercise 2.86 - replace "*" with "mul"
                       (add (angle z1) (angle z2))))  ; Exercise 2.86 - replace "+" with "add"
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))  ; Exercise 2.86 - replace "/" with "div"
                       (sub (angle z1) (angle z2))))  ; Exercise 2.86 - replace "-" with "sub"
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

;The polynomial package
(define (install-polynomial-package)
  ; internal procedures
  ; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  (define (variable? x)
    (symbol? x))
  ; representation of terms and term lists
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY"(list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in the same var: MUL-POLY" (list p1 p2))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  (define (zero-poly? p)  ; Exercise 2.87
    (define (iter terms)
      (cond ((empty-termlist? terms) true)
            ((not (= (coeff (first-term terms)) 0)) false)
            (else (iter (rest-terms terms)))))
    (iter (term-list p)))
  ; interface to rest of system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zero? '(polynomial) zero-poly?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Package installations
(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-polynomial-package)


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
(define (sine x) (apply-generic 'sine x))  ; Exercise 2.86
(define (cosine x) (apply-generic 'cosine x))  ; Exercise 2.86
(define (atang x y) (apply-generic 'atang x y))  ; Exercise 2.86
(define (sqroot x) (apply-generic 'sqroot x))  ; Exercise 2.86
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))
(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))


; Coercion procedures
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))
(define (scheme-number->rational n)
  (make-rational n 1))
(put-coercion 'scheme-number 'complex scheme-number->complex)
(put-coercion 'scheme-number 'rational scheme-number->rational)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
(sine a)
(cosine b)
(atang a b)
(sqroot (sub a b))

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
(sine r)
(cosine s)
(atang r s)
(sqroot (sub r s))

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
(define zr1 (make-complex-from-real-imag (make-rational 4 5) (make-rational 17 31)))
(define zr2 (make-complex-from-mag-ang (make-rational 8 9) (make-rational 45 11)))
(add zr1 zr2)
(sub zr1 zr2)
(mul zr1 zr2)
(div zr1 zr2)
(real-part zr1)
(real-part zr2)
(imag-part zr1)
(imag-part zr2)
(magnitude zr1)
(magnitude zr2)
(angle zr1)
(angle zr2)

; Polynomial operations
(define px (make-polynomial 'x (list (list 4 1) (list 3 2) (list 2 3) (list 1 4) (list 0 5))))
(define px2 (make-polynomial 'x (list (list 7 12) (list 6 9) (list 5 43) (list 4 1) (list 3 2) (list 2 3) (list 1 4) (list 0 5))))
(define py (make-polynomial 'y (list (list 4 1) (list 3 2) (list 2 3) (list 1 4) (list 0 5))))
(define p-zero (make-polynomial 'z (list (list 2 0) (list 1 0) (list 0 0))))
(add px px2)
(mul px px2)
(=zero? p-zero)
(=zero? px)

; Coercion operations
(scheme-number->complex 5)
(scheme-number->rational 5)