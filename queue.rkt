#lang racket

(define (make-queue)
  (let ((front-ptr null )
        (rear-ptr null ))
    (define (is-empty?)
      (eq? front-ptr rear-ptr))
    (define (pop)
      (if (is-empty?)
          (printf "ERROR")
          (begin (define (var)
                   (mcar front-ptr))
                 (let (front-ptr (mcdr front-ptr)))
                 var))
          )
    (define (insert elem)
      (if (is-empty?)
          (begin(let (front-ptr (mcons elem null)))
                (let (rear-ptr  (mcdr front-ptr)))
                null)
          (begin(let (rear-ptr (mcons elem null)))
                (let (rear-ptr (mcdr rear-ptr)))
                null)))
          
    (define (dispatch m l)
      (cond ((eq? m 'is-empty?) (is-empty?))
            ((eq? m 'pop) (pop))
            ((eq? m 'insert) (insert l))))
    dispatch
))





