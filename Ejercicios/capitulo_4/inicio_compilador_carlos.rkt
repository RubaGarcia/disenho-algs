#lang racket

; meta.rkt
; The metacircular evaluator from section 4.1

(define (seval exp environ)
  ; Evaluate a scheme expression
  (cond ((primitiva? exp) exp)  ; Primitive just "are". Return back
        ((simbolo? exp) (hash-ref environ (list exp)))  ; Symbols? Look up in the environment.
        ((define? exp) (hash-set! environ (list (car (car (cdr exp)))) (lambda (cdr (car (cdr exp))) (car (cdr (cdr exp)))))) ;
        ;si es un define, el car es "define", asi que nos da igual, lo siguiente (car del car del cdr) es la etiqueta, que va
        ; de la mano del contenido (cdr (car (cdr)), y luego ya esta la funcion (car (cdr (cdr)))
        ((if? exp) (if (seval (if-test exp) environ) (seval (if-consequence exp) environ) (seval (if-alternative exp) environ)))
        ((quote? exp) (seval (quote-expression exp) environ))
        ; ((cond? exp) ...)
        ; ((let ...))
        ; ((delay...))
        ((begin? exp) (apply seval (begin-expressions exp) environ))
        ((lambda? exp) (hash-ref environ (list exp)))
        ((procedure-application? exp) (apply (car (car exp)) (seval (cdr (car exp)) environ)))
        (else (error "Error desconocido"))
        )
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
(hash-set! environ 'lambda ((car (cdr exp)) (car (cdr (cdr exp))))) 
(hash-set! environ (list 'begin) begin)



(define (primitiva? exp)
  (or (number? exp) (boolean? exp)))

(define (simbolo? exp) (symbol? (car exp))) ;si lo primero que salga de la expresion es simbolo retorna true

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
    (define-in-environment! name (seval value environ) environ)
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


;; Varias pruebas para ver que es lo que tiene que ocurrir
(check-equal? (seval '42 environ) 42 "Primitives failed")
(check-equal? (seval 'foo environ) 123 "Symbol lookup failed")
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