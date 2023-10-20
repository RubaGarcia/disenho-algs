#lang racket

;(define a (alice-make-box 1 2 3 4))
;(define b (bob-make-box 1 2 3 4))

;(alice-area a)
;4
;(bob-area b)
;12

(define (attach-tag tag contents) (cons tag contents))
(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

(define (bob-make-box x y w h)
    (attach-tag 'bob-box
         (cons (cons x y) (cons w h))))

(define (alice-make-box x y x1 y1)
  (attach-tag 'alice-box
              (cons (cons x y) (cons x1 y1))))

(define (area values)
  (cond ((eq? (type-tag values) 'bob-box)
        (* (car(cdr(contents values))) (cdr(cdr(contents values)))))
        ((eq? (type-tag values) 'alice-box)
         (* (- (car (cdr(contents values))) (car (car(contents values)))) (- (cdr(cdr(contents values))) (cdr(car(contents values))))))
  (else (error "error"))))

   
  

     