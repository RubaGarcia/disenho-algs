#lang racket

; meta.rkt
; The metacircular evaluator from section 4.1

(define (seval exp environ)
  ; Evaluate a scheme expression
  (cond ((primitiva? exp) exp)  ; Primitive just "are". Return back
        ((simbolo? exp) (busca-entorno environ exp))  ; Symbols? Look up in the environment.
        ((define? exp) (seval-define exp environ)) ;
        ;si es un define, el car es "define", asi que nos da igual, lo siguiente (car del car del cdr) es la etiqueta, que va
        ; de la mano del contenido (cdr (car (cdr)), y luego ya esta la funcion (car (cdr (cdr)))
        ((if? exp) (seval-if exp environ))
        ((quote? exp) (seval-quote exp environ))
        ; ((cond? exp) ...)
        ; ((let ...))
        ; ((delay...))
        ((begin? exp) (begin-list (begin-expressions exp) environ))
        ((lambda? exp) (make-procedure exp environ))
        ((procedure-application? exp) (seval-procedure exp environ)) ;Evaluar la funcion, evaluar los args y ejecutar la funcion
        (else (error "Error desconocido"))
        )
  )
                           

(define (begin-list exp environ)
  (if (null? (cdr exp)) 
    (seval (car exp) environ)
    (begin (seval (car exp) environ) (begin-list (cdr exp) environ))))

(define (busca-entorno environ exp)
  (if (hash-has-key? environ exp)
  (hash-ref environ exp)
  (busca-entorno (hash-ref environ 'environ) exp))
  )

;defining the environment
(define environ (make-hash))
(hash-set! environ 'true true)
(hash-set! environ 'false false) 
(hash-set! environ '+ +)
(hash-set! environ '- -)
(hash-set! environ '= =)
(hash-set! environ '* *)
(hash-set! environ '= =)
(hash-set! environ '> >)
(hash-set! environ '< <)
(hash-set! environ 'lambda "lambda")


; Función para crear un procedimiento
(define (make-procedure exp environ)
  (lambda args
    (seval (caddr exp) (extend-environment (cadr exp) args environ))
  )
)

(define (extend-environment parameters args env)
  (define new-env (make-hash))
  (hash-set! new-env 'true true)
  (hash-set! new-env 'false false) 
  (hash-set! new-env '+ +)
  (hash-set! new-env '- -)
  (hash-set! new-env '= =)
  (hash-set! new-env '* *)
  (hash-set! new-env '= =)
  (hash-set! new-env '> >)
  (hash-set! new-env '< <)
  (hash-set! new-env 'lambda "lambda")
  (hash-set! new-env 'environ env)
    ; Asociamos los parámetros con los argumentos en el nuevo entorno
  (define n (length args))
  (do ((i 0 (+ i 1)))
    ((= i n) new-env)
    (hash-set! new-env (list-ref parameters i) (list-ref args i))
  )
)

(define (primitiva? exp)
  (or (number? exp) (boolean? exp)))

(define (simbolo? exp) (symbol? exp)) ;si lo primero que salga de la expresion es simbolo retorna true

(define (aplicacion-procedimiento? exp)
  (list? exp)
  )

; (define name value)

; Predicate to test
(define (define? exp)
  (and (list? exp) (eq? (car exp) 'define))
  )

; Selectors to extract information from the expression
(define (define-name exp)
  (cadr exp)
)

(define (define-value exp)
  (caddr exp)
)

; Evaluacion
(define (seval-define exp environ)
  (let ((name (define-name exp))
        (value (define-value exp)))
    (hash-set! environ name (seval value environ))
    )
  )

(define (quote? exp)
  (and (list? exp) (eq? (car exp) 'quote)))

(define (quote-expression exp)
  (cadr exp))

; Como evaluar el operador quote
(define (seval-quote exp environ)
  (quote-expression exp)
  )

; (if test consequence alternative)
(define (if? exp)
  (and (list? exp) (eq? (car exp) 'if)))

; Selectors
(define (if-test exp)
  (cadr exp)
  )

(define (if-consequence exp)
  (caddr exp)
  )

(define (if-alternative exp)
  (cadddr exp)
  )

; como evaluar los if
(define (seval-if exp environ)
  (if (seval (if-test exp) environ) ; Evaluate the test first
      (seval (if-consequence exp) environ)
      (seval (if-alternative exp) environ)
      )
  )

; (begin exp1 ... expn)
; Evaluar todas las expresiones
(define (begin? exp)
  (and (list? exp) (eq? (car exp) 'begin))
)

(define (begin-expressions exp)
  (cdr exp) ; Note: this returns a *list* of the expressions
  )

; (lambda method)
(define (lambda? exp)
  (and (list? exp) (eq? (car exp) 'lambda))
)

(define (procedure-application? exp)
  (list? exp)
)

(define (seval-procedure exp environ)
  (apply (seval (car exp) environ)
    (map (lambda (x) (seval x environ)) (cdr exp))))

(define (check-equal? a b c)
  (if (eq? a b)
      true
      c))

;; Varias pruebas para ver que es lo que tiene que ocurrir
(check-equal? (seval '42 environ) 42 "Primitives failed")
;(check-equal? (seval 'foo environ) 123 "Symbol lookup failed")
(seval '(define x 42) environ)
(check-equal? (seval 'x environ) 42 "Simple define failed")
(seval '(define y (+ 2 3)) environ)
(check-equal? (seval 'y environ) 5 "Expression define failed")
(check-equal? (seval '(quote x) environ) 'x "Quoting failed")

(check-equal? (seval '(if (< 2 3) 1 (/ 1 0)) environ) 1 "if-true failed")
(check-equal? (seval '(if (< 3 2) (/ 1 0) 1) environ) 1 "if-false failed")

; Procedures
(seval '(define square (lambda (x) (* x x))) environ)
(check-equal? (seval '(square 4) environ) 16 "square failed")

(seval '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) environ)
(check-equal? (seval '(fact 5) environ) 120 "fact failed")