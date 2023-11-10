#lang racket
;la relación pk(n) = p(k-1)(n) + pk (n-k) se cumple debido a los siguientes factores:
;
;a la hora de dividir un numero n en k partes se plantean dos casos: el caso en el que k se incluye como una
;de las partes, resultando en n-k en k partes (es decir, pk(n-k), y el caso en el que k no se incluye como
;partición, resultando en pk-1(n) partes. La suma de ambos casos resulta en pk(n), y por tanto ese es el origen
;de la relación
