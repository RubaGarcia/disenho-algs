#lang racket
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (* (car things) (car things))
                    answer))))
  (iter items null))
; se invierte porque se está introduciendo el primer elemento de la lista original en la primera posición de la respuesta
;pero estamos vaciando la primera lista, así que tras la primera iteracion el primer elemento es el segundo, y le ponemos el primero en la solucion.

(define (square-list2 items)
  (define (iter2 things answer)
    (if (null? things)
        answer
        (iter2 (cdr things)
              (cons answer
                    (* 
                     (car things) (car things))))))
  (iter2 items null))
;debido a que estamos introduciendo la respuesta tal cual al establecer la constante, estamos introduciendo múltiples punteros nulos que provienen del final de la respuesta (porque
;todas las listas terminan con un puntero nulo)

(square-list (list 1 2 3 4))
(square-list2 (list 1 2 3 4))