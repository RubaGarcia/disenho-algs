#lang racket
(define (grafo nodos conexiones)

  (define referencias (build-list nodos values)
  (define (adyacentes nodo)
    (hash-ref conexiones nodo))
  (define (num-adyacentes nodo)
    (legth hash-ref conexiones nodo))
  (define (quitar-arista nodo1 nodo2)
    (begin (hash-set! conexiones nodo1 (remove nodo2 (hash-ref conexiones nodo1)))
           (hash-set! conexiones nodo2 (remove nodo1 (hash-ref conexiones nodo2)))
           (display "arista eliminada")
           ))
  (define (unir p q)
    (define pid (list-ref referencias p))
    (define qid (list-ref referencias q))
    (for ([i nodos])
        (if  (eq? (list-ref referencias i) qid)
             (set! referencias (list-set referencias i pid))
             (void))))
    
  
  (define (grafo-conexo)
    (set! referencias (build-list
  (define (dispatch message)
    (cond ((eq? message 'adyacentes) adyacentes)
          ((eq? message 'num-adyacentes) num-adyacentes)
          ((eq? message 'quitar-arista) quitar-arista)))

