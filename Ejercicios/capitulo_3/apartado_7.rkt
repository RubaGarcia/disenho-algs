#lang racket
;el algorimo bottom-up es un algortimo que define la solución del problema
;desde el caso base hasta el caso deseado, en este caso, se calculan todos los valores
;intermedios, sean necesarios o no, por lo que se calcularían n² 

;se recurren a 2 valores previamente almacenados, uno por cada acceso de la
;funcion original de las particiones. por cómo es el algoritmo iterativo, los
;valores que necesitamos se encuentran ya hallados

;se hacen n² operaciones aritmeticas por el razonamiento analogo a la primera pregunta
;solo es necesario hacer una suma por cada casilla 