#lang racket

(define (zero f) (lambda (x) x))

(define (one f) (lambda (x) (f x)))

(define (inc x) (+ x 1))

(define (two f) (lambda (x) (f(f x))))