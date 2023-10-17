#lang racket
(equal? '(this is a list) 
        '(this is a list))
;true
(equal? '(this is a list) 
        '(this (is a) list))
;false

(define (equ? x y)
  (if (null? x) 
      (null? y)
      (if (eq? (car x) (car y))
          (equ? (cdr x) (cdr y))
          #f)))

(equ? '(this is a list) 
        '(this is a list))
;true
(equ? '(this is a list) 
        '(this (is a) list))     
;false