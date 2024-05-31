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






























; The dense package
(define (install-dense-package)
  ; internal procedures
  (define (empty-termlist? L)
    (null? (contents L)))
  (define (first-term L)
    (car L))
  (define (first-term-pair L)
    (list (- (length L) 1) (car L)))
  (define (rest-terms L)
    (cdr L))
  (define (adjoin-term term term-list)
    (if (=zero? (cadr term))
        term-list
        (cons term term-list)))
  (define (add-terms L1 L2)
    (define (iter L1 L2 result)
      (cond ((and (empty-termlist? L1)
                  (empty-termlist? L2))
             result)
            ((> (length L1) (length L2))
             (iter (rest-terms L1) L2 (append result (list (first-term L1)))))
            ((< (length L1) (length L2))
             (iter L1 (rest-terms L2) (append result (list (first-term L2)))))
            (else
             (iter (rest-terms L1) (rest-terms L2) (append result (list (add (first-term L1) (first-term L2))))))))
    (iter L1 L2 '()))
  (define (sub-terms L1 L2)
    (define (iter L1 L2 result)
      (cond ((and (empty-termlist? L1)
                  (empty-termlist? L2))
             result)
            ((> (length L1) (length L2))
             (iter (rest-terms L1) L2 (append result (list (first-term L1)))))
            ((< (length L1) (length L2))
             (iter L1 (rest-terms L2) (append result (list (first-term L2)))))
            (else
             (iter (rest-terms L1) (rest-terms L2) (append result (list (sub (caar L1) (caar L2))))))))
    (iter L1 L2 '()))
  (define (mul-terms L1 L2)
    (apply-generic 'mul-terms
                   (dense-termlist->sparse-termlist L1)
                   (dense-termlist->sparse-termlist L2)))
    
  (define (div-terms L1 L2)
    (if (null? L1)
        (list '() '())
        (let ((t1 (first-term-pair L1))
              (t2 (first-term-pair L2)))
              (if (> (car t2) (car t1))
                  (list '() L1)
                  (let ((new-c (div (cadr t1) (cadr t2)))
                        (new-o (- (car t1) (car t2))))
                    (let ((rest-of-result
                           (div-terms (sub-terms (attach-tag 'dense L1) (mul-terms (attach-tag 'sparse (list new-o new-c))
                                                         (attach-tag 'dense L2))) L2)))
                      (list (adjoin-term (list new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))
  (define (nil-or-zero? L)
    (cond ((empty-termlist? L) true)
          ((not (=zero? (first-term L))) false)
          (else
           (nil-or-zero? (rest-terms L)))))
  (define (remove-leading-zeros L)
    (cond ((null? L) L)
          ((=zero? (first-term L))
           (remove-leading-zeros (rest-terms L)))
           (else(L))))
  ; interface to rest of the system
  (define (tag L) (attach-tag 'dense L))
  (put 'add-terms '(dense dense)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'div-terms '(dense dense)
       (lambda (L1 L2) (tag (div-terms L1 L2))))
  (put 'first-term '(dense) first-term)
  (put 'first-term-pair '(dense) first-term-pair)
  (put 'rest-terms '(dense)
       (lambda (L) (tag (rest-terms L))))
  (put 'mul-terms '(dense dense)
       (lambda (L1 L2)
         (apply-generic 'mul-terms
                        (dense-termlist->sparse-termlist (tag L1))
                        (dense-termlist->sparse-termlist (tag L2)))))
  (put 'neg '(dense)
       (lambda (L) (tag (map
                         (lambda (t) (mul t -1))
                         L))))
  (put '=zero? '(dense) nil-or-zero?)
  (put 'equ? '(dense dense)
       (lambda (L1 L2) (equal? (remove-leading-zeros L1) (remove-leading-zeros L2))))
  (put 'make-dense-termlist 'dense
       (lambda (L) (tag L)))
  "Dense termlist package installed")


; The polynomial package
(define (install-polynomial-package)
  ; import from sparse/dense packages
  (define (make-sparse-termlist L)
    ((get 'make-sparse-termlist 'sparse) L))
  (define (make-dense-termlist L)
    ((get 'make-dense-termlist 'dense) L))
  (define (add-terms L1 L2)
    (apply-generic 'add-terms L1 L2))
  (define (mul-terms L1 L2)
    (apply-generic 'mul-terms L1 L2))
  (define (first-term-pair L)
    (apply-generic 'first-term-pair L))
  (define (div-terms L1 L2)
    (apply-generic 'div-terms L1 L2))
  ; internal procedures
  (define (make-from-existing-termlist var L)
    (cons var L))
  (define (term-list p)
    (cdr p))
  (define (variable p)
    (car p))
  (define (variable? x)
    (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
         (variable? v2)
         (eq? v1 v2)))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-existing-termlist (variable p1)
                               (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-existing-termlist (variable p1)
                                              (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-from-existing-termlist (variable p1)
                                     (div-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))
  (define (neg p)
    (make-from-existing-termlist (variable p)
                                 (apply-generic 'neg (term-list p))))
  (define (add-poly-to-scheme p n)
    (add-poly p (make-from-existing-termlist (variable p) (make-dense-termlist (list n)))))
  ; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (L1 L2) (tag (add-poly L1 L2))))
  (put 'add '(polynomial scheme-number)
       (lambda (p n) (tag (add-poly-to-scheme p n))))
  (put 'add '(scheme-number polynomial)
       (lambda (n p) (tag (add-poly-to-scheme p n))))
  (put 'mul '(polynomial polynomial)
       (lambda (L1 L2) (tag (mul-poly L1 L2))))
  (put 'neg '(polynomial)
       (lambda (p) (tag (neg p))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 (neg p2)))))
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2) (tag (div-poly p1 p2))))
  (put '=zero? '(polynomial)
       (lambda (p) (=zero? (term-list p))))
  (put 'equ? '(polynomial polynomial)
       (lambda (p1 p2) (and (same-variable? (variable p1) (variable p2))
                            (apply-generic 'equ? (term-list p1) (term-list p2)))))
  (put 'make-from-sparse-termlist 'polynomial
       (lambda (var L) (tag (cons var (make-sparse-termlist L)))))
  (put 'make-from-dense-termlist 'polynomial
       (lambda (var L) (tag (cons var (make-dense-termlist L)))))
    "Polynomial package installed")


; The sparse package
(define (install-sparse-package)
  ; internal procedures
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)))
  (define (make-term order coeff)
    (list order coeff))
  (define (coeff term)
    (cadr term))
  (define (order term)
    (car term))
  (define (first-term L)
    (car L))
  (define (rest-terms L)
    (cdr L))
  (define (the-empty-termlist)
    '())
  (define (empty-termlist? L)
    (null? L))
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
  (define (div-terms L1 L2)
    (if (null? L1)
        (list '() '())
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
              (if (> (order t2) (order t1))
                  (list '() L1)
                  (let ((new-c (div (coeff t1) (coeff t2)))
                        (new-o (- (order t1) (order t2))))
                    (let ((rest-of-result
                           (div-terms (sub L1 (mul-terms (list new-o new-c) L2)) L2)))
                      (list (adjoin-term (list new-o new-c) (car rest-of-result))
                      (cadr rest-of-result))))))))
  (define (nil-or-zero? L)
    (cond ((empty-termlist? L) true)
          ((not (=zero? (coeff (first-term L)))) false)
          (else
           (nil-or-zero? (rest-terms L)))))
  (define (remove-zero-terms L)
    (define (iter result terms)
      (cond ((null? terms) result)
            ((=zero? (coeff (first-term terms)))
             (iter result (rest-terms terms)))
            (else
             (iter (append result (list (first-term terms))) (rest-terms terms)))))
    (iter '() L))
          
  ; interface to rest of the system
  (define (tag L) (attach-tag 'sparse L))
  (put 'add-terms '(sparse sparse)
       (lambda (L1 L2) (tag (add-terms L1 L2))))
  (put 'div-terms '(sparse sparse)
       (lambda (L1 L2) (tag (div-terms L1 L2))))
  (put 'mul-terms '(sparse sparse)
       (lambda (L1 L2) (tag (mul-terms L1 L2))))
  (put 'first-term '(sparse) first-term)
  (put 'first-term-pair '(sparse) first-term)
  (put 'rest-terms '(sparse)
       (lambda (L) (tag (rest-terms L))))
  (put '=zero? '(sparse) nil-or-zero?)
  (put 'equ? '(sparse sparse)
       (lambda (L1 L2) (equal? (remove-zero-terms L1)
                               (remove-zero-terms L2))))
  (put 'neg '(sparse)
       (lambda (L) (tag (map
                         (lambda (t) (make-term (order t)
                                                (mul (coeff t) -1)))
                         L))))
  (put 'make-sparse-termlist 'sparse
       (lambda (L) (tag L)))
  "Sparse termlist package installed")



(define (dense-termlist->sparse-termlist L)
  (define (iter L result)
    (cond ((null? (contents L)) result)
          ((=zero? (first-term L))
           (iter (rest-terms L) result))
          (else
           (iter (rest-terms L)
                 (append result (list (list (- (length L) 1) (first-term L))))))))
  (cons 'sparse (iter L '())))
(define (sparse-termlist->dense-termlist L)
  (define (iter L result last-order)
    (cond ((null? (contents L)) result)
          ((= (- last-order 1) (car (first-term L)))
           (iter (rest-terms L)
                 (append result (list (cadr (first-term L))))
                 (- last-order 1)))
          (else
           (iter L
                 (append result (list 0))
                 (- last-order 1)))))
  (if (null? (contents L))
      (cons 'dense '())
      (cons 'dense (iter L '() (+ (caar (contents L)) 1)))))








