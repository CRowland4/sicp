#lang sicp
; Below are the procedures needed to make this sytem work
(define (square x)
  (mul x x))  ; Exercise 2.86 - change "*" to "mul"
(define (mul x)
  x)

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




























; The sparse package
(define (install-sparse-package)
  ; internal procedures
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

  ; interface to rest of the system



  "Sparse package installed")




; The dense package
(define (install-dense-package)
  ; internal procedures

  ; interface to rest of the system



  "Dense package installed")



;The polynomial package
(define (install-polynomial-package)
  ; internal procedures
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)  ; TODO make-poly to a more specific make
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)  ; TODO make-poly to a more specific make
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ; interface to rest of the system
  

  
    "Polynomial package installed")





#|

  
  
  ; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make-from-sparse-terms 'polynomial
       (lambda (var terms) (tag (make-from-sparse-terms var terms))))
  (put 'make-from-dense-terms 'polynomial
       (lambda (var terms) (tag (make-from-dense-terms var terms))))
  "Polynomial package installed")
|#
#|



  (define (the-empty-termlist)
    '())
  (define (empty-termlist? L)
    (null? L))
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (make-term order coeff)
    (list order coeff))
  (define (order term)
    (car term))
  (define (coeff term)
    (cadr term))

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
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-from-sparse-terms var sparse-terms)
    (cons var sparse-terms))
  (define (create-sparse-terms-from-dense-terms dense-terms)
    (define (iter result next-order terms)
      (cond ((empty-termlist? terms)
             result)
            ((= (car terms) 0)
             (iter result (- next-order 1) (cdr terms)))
            (else
             (let ((new-term (make-term next-order (car terms))))
               (iter (append result (list new-term)) (- next-order 1) (cdr terms))))))
    (iter '() (length dense-terms) dense-terms))
  (define (make-from-dense-terms var dense-terms)
    (cons var
          (create-sparse-terms-from-dense-terms dense-terms)))
  ; interface to rest of the system

|#



#|
  (define (the-empty-termlist)
    '())
  (define (empty-termlist? L)
    (null? L))
  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))
  (define (add-terms L1 L2)
    (cond ((> (length L1) (length L2))
           (add-terms L1 (adjoin-term 0 L2)))
          ((> (length L2) (length L1))
           (add-terms (adjoin-term 0 L1) L2))
          (else
           (add-equal-length-term-lists L1 L2))))
  (define (add-equal-length-term-lists L1 L2)
    (define (iter result L1 L2)
      (if (null? L1)
          result
          (let ((new-result (+ (first-term L1)
                               (first-term L2))))
            (iter (append result (list new-result))
                  (rest-terms L1)
                  (rest-terms L2)))))
    (iter '() L1 L2))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (map (lambda (t) (mul t t1)) L))
  (define (adjoin-term term term-list)
    (cons term term-list))
  (define (make-from-sparse-terms var sparse-terms)
    (cons var
          (make-dense-terms-from-sparse-terms sparse-terms)))
  (define (make-dense-terms-from-sparse-terms sparse-terms)
    (define (iter result terms)
      (cond ((empty-termlist? terms) result)
            ((empty-termlist? result)
             (iter (append result (caar terms))
      
  ; interface to rest of the system
|#

(define (variable p) (apply-generic 'variable p))