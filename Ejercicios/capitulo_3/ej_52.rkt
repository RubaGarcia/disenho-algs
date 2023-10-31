#lang racket
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

(stream-ref y 7)
(display-stream z)

; El valor de "sum" ha de ser el mismo que el del ultimo elemento de "seq", en este caso 210
; De la ejecucion de "stream-ref" se obtiene el octavo elemento del stream "y", ya que utiliza numeracion similar a los arrays y comienza en 0. El stream "y" contiene todos los elementos pares del stream "seq"
; De la ejecución de "display-stream" se imprime por pantalla todos los valores del stream "z", el cual contiene todos los elementos que son multiplos de 5 del stream seq

; Si en lugar de utilizar "delay <exp>" hubieramos hecho uso de "lambda () <exp>", los valores del stream "seq" se hubieran calculado de nuevo en su segunda iteracion, cambiando los valores del stream "z" ya que la variable
; "sum" se encontraria con valor 210 al comienzo de la segunda ejecucion