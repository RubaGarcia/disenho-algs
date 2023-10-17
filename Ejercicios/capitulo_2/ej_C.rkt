#lang racket
(define (width b)
    (cond ((bob-box? b) (bob-width b))
          ((alice-box? b) (alice-width b))))

(define (height b)
    (cond ((bob-box? b) (bob-height b))
          ((alice-box? b) (alice-height b))))

(define (area b)
    (cond ((bob-box? b) (bob-area b))
          ((alice-box? b) (alice-area b))))


