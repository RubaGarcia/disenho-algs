#lang racket

(define sumatorio 0)

(define (make-accumulator x)
  (begin
        (set! sumatorio (+ sumatorio x))
	sumatorio
    )
  )

(define (A n) (make-accumulator n))