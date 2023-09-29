#lang racket

(define (for-each func l)
  (func (car l))
  (if (null? (cdr l))
      (void)
      (for-each func (cdr l))))
      

(for-each 
 (lambda (x) (newline) (display x))
 (list 57 321 88))

