#lang racket
;(list 1 (list 2 (list 3 4)))
(displayln '(list 1 (list 2 (list 3 4))))
(displayln '(list 1 (list 2 (list 3 (cons 4 null)))))
(displayln '(list 1 (list 2 (cons 3 (cons 4 null)))))
(displayln '(list 1 (cons 2 (cons 3 (cons 4 null)))))
(displayln '(cons 1 (cons 2 (cons 3 (cons 4 null)))))

;(list 1 (list 2 (list 3 4)))