#lang sicp
; Exercise 3.50
(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))