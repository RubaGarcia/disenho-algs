#lang racket

(define (make-queue)
  (let ((front-ptr #f)
        (rear-ptr #f))

    (define (enqueue item)
      (let ((new-node (mcons item #f)))
        (if (not rear-ptr)
            (begin
              (set! front-ptr new-node)
              (set! rear-ptr new-node))
            (begin
              (set-mcdr! rear-ptr new-node)
              (set! rear-ptr new-node))))
      'enqueue)

    (define (dequeue)
      (if (not front-ptr)
          (error "La cola está vacía")
          (let ((item (mcar front-ptr)))
            (begin
              (set-mcar! front-ptr (mcdr front-ptr)))
            item)))

    (define (is-empty?)
      (not front-ptr))

    (define (dispatch m)
      (cond ((eq? m 'enqueue) enqueue)
            ((eq? m 'dequeue) dequeue)
            ((eq? m 'is-empty?) is-empty?)
            (else (error "Operación no válida"))))

    dispatch))

(define A (make-queue))

