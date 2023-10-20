#lang racket

(define (make-monitored f)
  (define func f)
  (define mf 0)
  (lambda (param)
    (cond ((eq? param 'how-many-calls?) mf)
          ((eq? param 'reset-count) (set! mf 0))
          (else (begin (set! mf (+ mf 1)) (func param))))))