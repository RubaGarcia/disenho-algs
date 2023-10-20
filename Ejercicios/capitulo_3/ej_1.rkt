#lang racket

(define (make-accumulator number)
  (define quantity number)
  (lambda (amount)
    (begin (set! quantity 
                     (+ quantity amount))
               quantity)))