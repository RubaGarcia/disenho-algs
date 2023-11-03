#lang racket

;returns its argument after printing it
(define (show x)
  (display-line x)
  x) ;segunda vez que imprime x

(define (display-line x)
  (newline)    ;nueva linea
  (display x)) ; primera vez que imprime x

(define (stream-enumerate-interval low high)
  (if (> low high)
      empty-stream
      (stream-cons
       low
       (stream-enumerate-interval (+ low 1)
                                  high))))

(define x 
  (stream-map 
   show 
   (stream-enumerate-interval 0 10)))

(stream-ref x 5)  
(stream-ref x 7)

;stream-ref tiene como argumentos x (el string) y 5 (la posicion que se quiere acceder del string), entonces stream-ref llega hasta la posicion indicada y solo lo evalua cuando llega a la posicion deseada.
;finalmente, lo que imprime por pantalla es: va a una linea nueva, y dos veces el resultado. En show se puede aprecir que imprime una primera vez por display-line -> display y una segunda vez en x

