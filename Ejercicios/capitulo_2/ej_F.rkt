#lang racket
(define (make-bob-box x y width height)
  (define (dispatch message)
    (cond ((eq? message 'width) width)
          ((eq? message 'height) height)
          ((eq? message 'type) 'bob-box)
          )
    )
  dispatch
)
(define (make-alice-box x1 y1 x2 y2)
  (define (dispatch message)
    (cond ((eq? message 'width) (- x2 x1))
          ((eq? message 'height) (- y2 y1))
          ((eq? message 'type) 'alice-box)
          )
    )
  dispatch
)