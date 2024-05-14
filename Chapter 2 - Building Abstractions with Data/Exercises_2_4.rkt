#lang scheme
; Everything between here and the row of semi-colons is defined so the exercises have access to them

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)  ; One of the factors is 0
        ((=number? m1 1) m2)  ; One of the factors is 1
        ((=number? m2 1) m1)  ; One of the factors is 1
        ((and (number? m1) (number? m2)) (* m1 m2))  ; Both of the factors are numbers that aren't 0 or 1
        (else (list '* m1 m2))))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)  ; One of the items is 0
        ((=number? a2 0) a1)  ; One of the items is 0
        ((and (number? a1) (number? a2))  ; Both items are numbers
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (multiplicand p)
  (if (= (length p) 3)
      (caddr p)
      (make-product (caddr p) (cadddr p))))

(define (augend s)
  (if (= (length s) 3)
      (caddr s)
      (make-sum (caddr s) (cdddr s))))

(define (multiplier p)
  (cadr p))

(define (addend s)
  (cadr s))

(define (variable? x)
  (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))

(define (exponent expr)
  (caddr expr))

(define (base expr)
  (cadr expr))

(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-exponentiation base expn)
  (cond ((and (number? base) (number? expn))
         (expt base expn))
        ((=number? expn 1) base)
        ((=number? expn 0) 1)
        (else (list '** base expn))))

(define (put op type item)  ; Placeholder for put operation, discussed more in chapter 3
  "foo")

(define (get op type)  ; Placeholder for get operation, discussed more in chapter 3
  ("bar"))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG")))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Exercise 2.73
; Rewriting in data-directed style
(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))


#|
a) In the data-directed rewrite, the operator is being stripped from the expression and used to lookup
      the particular derivative procedure that is associated with that tag. The derivative procedure
      is an operation, whose "type" depends on the operator it receives as an identifier. The number?
      and variable? predicates are procedures whose implementation doesn't depend at all on what data
      they're passed. So they wouldn't fit nicely into the data dispatch table. It would be technically
      possible to have a copy of the same procedure for each "type" in the table, but then you would have
      multiple copies of both procedures - a number? and variable? for +, a number? and variable? for *,
      and so on. This would waste a lot of space, but even more importantly it would be bad design. Each
      new "package" that deals with a different derivative rule would have to add those two procedures,
      which is unwieldy.

|#

; b) and c)
(define (install-derivative+-package)
  (define (deriv+ expr var)
    (make-sum (deriv (addend expr) var)
              (deriv (augend expr) var)))
  (put 'deriv '(+) deriv+)
  'done)

(define (install-derivative*-package)
  (define (deriv* expr var)
    (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
  (put 'deriv '(*) deriv*)
  'done)

(define (install-derivative**-package)
  (define (deriv** expr var)
    (if (number? (exponent expr))
         (make-product (exponent expr)
                       (make-product (make-exponentiation (base expr) (- (exponent expr) 1))
                                     (deriv (base expr) var)))
         (error "This exponential expression has a variable expression as its exponent"))
  (put 'deriv '(**) deriv**))
  'done)

; d)  If the get procedure's argument order changed to ((get (operator exp) 'deriv) (operands exp) var), we would
;        have to change our put procedure calls from (put 'deriv '+ deriv+) to (put '+ 'deriv deriv+)



; Exercise 2.74 - sketch in LiquidText
; Our packages will be dividied up by division. Each division's files should be structured as a list of files,
;   with the first element being the employee name.
(define (install-division<num>-package)
  (define (get-record name)  ; This procedure will need an employee's name as an argument, for the generic, the division number and name will need to be supplied.
    "Search Division <num>'s records for the record with name <name>. Returns #f if the record isn't found.")
  (define (get-salary record)  ; This procedure will need an employee record as an argument, that should be a list with the employee name as the first element.
    "Search the given Division <num> record for the salary information")

  (put 'get-record '(<num>) get-record)
  (put 'get-salary '(<num>) get-salary))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

; a)
(define (get-record-generic division-number name)
  (apply-generic 'get-record (list division-number name)))

; b)
(define (get-salary-generic division-number record)
  (apply-generic 'get-salary (list division-number record)))

; c)
(define (find-employee-record name)
  (define (get-record-iter current-division-number)
    (let ((record (get-record-generic current-division-number name)))
      (if (eq? record false)
          (get-record-iter current-division-number)
          record)))
  (get-record-iter 1))

; d) An item will have to be added to the beginning of the list that is the personnel file, indicating a number.
;     Then each employee file will need to have the name of the employee added as the first element in the record.
;     An installation package will also need to be created for the new division



; Exercise 2.75
(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part )
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op: MAKE-FROM-MAG-ANG" op))))
  dispatch)



; Exercise 2.76
#|
Explicit Data Dispatch
     In order to add a new type, there must be an addition made to all operations to account
        for data of the appropriate type.
     In order to add a new operation, you would simply write the operation with conditions to check each new type.

Data-Directed
     For a new type, you would have to write a new installation package containing definitions for all the existing operations
        as they applied to that new type.
     For a new operation, you would need to define that operation inside each installation package as it applied to that
        package's type.

Message Passing
     For a new type, you would need to write a new procedure that returned a dispatch procedure for that specific type.
     For a new operation, each type's dispatch operation will have to have a new check added for that operation.


If new types were frequently being added to the system, I would opt for a data-directed system. This way,
   constant changes to the existing operations are avoided, and you can immediately begin using the new type with the
   existing generic procedures.

If new operations were frequently being added, I would opt for explicit data dispatching, for the similar reason that there wouldn't
   need to be lots of changes made to the existing system each time an operation was added.
|#

    


    
    
    
             