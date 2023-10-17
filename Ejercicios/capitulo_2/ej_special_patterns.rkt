#lang racket

(define record '(job (Hacker Alyssa P) (computer programmer)))

(define (match pattern data)
  (if (null? pattern)
      (null? data)
      (if (equal? (car pattern) '?)
          (match (cdr pattern) (cdr data))
          (if (list? (car pattern))
              (if (match (car pattern) (car data))
                  (match (cdr pattern) (cdr data))
                  #f)
              (if (equal? (car pattern) (car data))
                  (match (cdr pattern) (cdr data))
                  #f)))))
          