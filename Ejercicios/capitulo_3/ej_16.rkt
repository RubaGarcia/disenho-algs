#lang racket


(define (count-pairs x)
  (if (not (mpair? x))
      0
      (+ (count-pairs(mcar x))
         (count-pairs(mcar x))
         1)))


(define n1 (mcons 2 null))
(define n2 (mcons n1 n1))
(define n3 (mcons n2 n2))
(count-pairs n3)


(define n4 (mcons 2 2))
(define n5 (mcons n4 n4))
(set-mcdr! n4 n5)
(set-mcar! n4 n5)

(define n6 (mcons 1 1))
(set-mcar! n6 n6)
;este vale como el primero pero es mas optimo 
