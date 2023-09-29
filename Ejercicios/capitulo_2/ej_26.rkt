#lang racket
(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y) ;append une las listas de manera adecuada
(cons x y); cons crea una constante que tiene como primer elemento la primera lista y un puntero al siguiente elemento que señala a la siguiente lista, por tanto tiene la forma
; (lk = lista numero k, elemLKn = elemento n de la listaK): (L1 elemL21 elemL22 ... elemL2n)
(list x y); list crea una lista con dos elementos, x e y. Al ser ambos listas (y siguiendo la notación dada en cons) la lista resultante tiene la forma: (L1 L2)