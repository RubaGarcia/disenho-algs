#lang racket

(define record '((x 1) (y 2) (z 3)))

(define (assoc key record)
  (if (null? record)
      null
      (if (equal? key (car (car record)))
          (car record)
          (assoc key (cdr record)))))

(define (add-entry key value record)
  (if (null? (assoc key record))
      (union record (list key value))
      (if (equal? (assoc key record) (assoc key (cdr record)))
          (cons (car record) (add-entry key value (cdr record)))
          (cons (list key value) (cdr record)))))
      

(define (del-entry key record)
  (if (null? (assoc key record))
      null
      (if (equal? (assoc key record) (assoc key (cdr record)))
          (cons (car record) (del-entry key (cdr record)))
          (cdr record))))

(define (union lista elem)
  (if (null? lista)
      (cons elem null)
      (cons (car lista) (union (cdr lista) elem))))