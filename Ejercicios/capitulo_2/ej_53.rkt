#lang racket
(list 'a 'b 'c) ;lista de los simbolos de a,b,c (a,b,c no son evaluadas)
(list (list 'george)) ;se forma una lista de una lista del simbolo george
(cdr '((x1 x2) (y1 y2))) ; cdr devuelve devuelve el segundo elemento de una pareja, en este caso es el simbolo de la lista y1 e y2
(cadr '((x1 x2) (y1 y2))) ; cadr es el car del cdr, es decir, primero coge el segundo elemento de la pareja de simbolos (x1 x2) (y1 y2) quedando (y1 y2)
                          ;y luego coge el primer elemento entre los simbolos (y1 y2) y null, quedando el simbolo (y1 y2)
(pair? (car '(a short list))) ;primero hace el car de la lista que cogeria el simbolo a y comprueba si el simbolo a es una pareja, como no lo es devuelve false
(memq 'red '((red shoes) (blue socks))) ;devuelve false porque busca el simbolo red en si, no mira si se encuenta en una sublista 
(memq 'red '(red shoes blue socks)); devuelve la lista porque ha encotrado el simbolo red dentro de la lista