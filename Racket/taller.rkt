#lang racket

;------ Factorial ------
(define (factorial num)
  (cond ((zero? num) 1)
  (else
   (* num (factorial (- num 1))))))


;------ Fibonacci ------
(define (fibonacci num)
  (cond ((equal? num 0) 0)
        ((equal? num 1) 1)
        (else (+ (fibonacci (- num 1)) (fibonacci (- num 2)))
    )))


;------ Miembro ------
(define (miembro? item lista)
  (cond ((null? lista) #f)
        ((equal? item (car lista)) #t)
        (else (miembro? item (cdr lista)))))


;------ Eliminar (lista) ------
(define (eliminar item lista)
  (cond ((null? lista ) '())
        ((equal? item (car lista)) (eliminar item (cdr lista)))
        (else (cons (car lista) (eliminar item (cdr lista))))))


;------ Quicksort ------
(define (pivot lista)
  (cond ((null? lista) #f)
        (else
         (pivot-aux (car lista) (cdr lista) '() '()))))

(define (pivot-aux punto lista men may)
  (cond ((null? lista) (list men may))
        ((<= (car lista) punto) (pivot-aux punto (cdr lista) (cons (car lista) men) may))
        (else
         (pivot-aux punto (cdr lista) men (cons (car lista) may)))))

(define (quicksort lista)
  (cond ((null? lista) '())
        (else
         (append (quicksort (car (pivot lista))) (list (car lista)) (quicksort (cadr (pivot lista)))))))


;------ Automovil ------
(define (automovil atributos valores)
  (cond ((or (null? valores) (null? atributos)) '())
        (else (cons (list (car valores) (car atributos)) (automovil (cdr atributos) (cdr valores))))))


;--------- Arbol binario ---------
(define (new_arbol centro izq der)
  (cond ((and (null? izq) (null? der)) centro)
        (else (list centro izq der))))

(define (atom? arbol)
  (not (list? arbol)))

(define (raiz arbol)
  (cond ((atom? arbol) arbol)
        (else
         (car arbol))))

(define (hijo-izq arbol)
  (cond ((atom? arbol) '())
        (else (cadr arbol))))

(define (hijo-der arbol)
  (cond ((atom? arbol) '())
        (else (caddr arbol))))

(define (menor arbol)
  (cond ((null? arbol) #f)
        ((null? (hijo-izq arbol)) (raiz arbol))
        (else
         (menor (hijo-izq arbol)))))


;------ Eliminar (arbol) ------
(define (eliminar_arb item arbol)
  (cond ((null? arbol) '())
        
        ;Busqueda del nodo
        ((< item (raiz arbol)) (new_arbol (raiz arbol) (eliminar_arb item (hijo-izq arbol)) (hijo-der arbol)))
        ((> item (raiz arbol)) (new_arbol (raiz arbol) (hijo-izq arbol) (eliminar_arb item (hijo-der arbol))))

        ;El nodo no tiene hijos
        ((and (null? (hijo-izq arbol)) (null? (hijo-der arbol))) '())

        ;El nodo no tiene hijo izquierdo
        ((null? (hijo-izq arbol)) (hijo-der arbol))

        ;El nodo no tiene hijo derecho
        ((null? (hijo-der arbol)) (hijo-izq arbol))

        ;El nodo tiene dos hijos
        (else
         (new_arbol (menor (hijo-der arbol)) (hijo-izq arbol)
                    (eliminar_arb (menor (hijo-der arbol)) (hijo-der arbol))))))


;------ Grafo ------
(define grafo_nuevo '((i (a b))
                  (a (i c d))
                  (b (i c d))
                  (c (a b x))
                  (d (a b f))
                  (x (c))
                  (f (d))))

(define (sol? final camino)
  (equal? final (car camino)))

(define (vecinos ele grafo)
   (cond ((equal? (assoc ele grafo) #f) #f)
          (else (cadr (assoc ele grafo)))))

(define (extender camino grafo)
  (apply append
         (myMap (lambda(x)
                (cond ((miembro? x camino) '( ))
                      (else (list (cons x camino)))))
              (vecinos (car camino) grafo))))

(define (porAnchura inicio final grafo)
  (porAnchuraAux (list (list inicio)) final grafo '()))
(define (porAnchuraAux caminos final grafo result)
  (cond ((null? caminos)
         (myMap myReverse result))
        ((sol? final (car caminos))
         (porAnchuraAux (cdr caminos) final grafo (cons (car caminos) result)))
        (else
         (porAnchuraAux (append (cdr caminos)
                                (extender (car caminos) grafo))
                        final
                        grafo
                        result))))

;--- Aux ---
(define (myMap funcion lista)
  (cond ((empty? lista)  '())
        (else (cons (funcion (car lista)) (myMap funcion (cdr lista))))))

(define (myReverse lista)
  (cond ((null? lista) '())
        (else
         (append (myReverse (cdr lista)) (list (car lista))))))

;Llamadas
(factorial 13)
(fibonacci 17)
(miembro? 0 '(1 9 8 6 8 7 0 3 5))
(eliminar 5 '(1 2 4 7 5 9 0))
(automovil '(Hatchback Suzuki Forza1 Rojo si Manual) '(Tipo Marca Modelo Color AC Transmision))
(quicksort '(2 3 4 7 9 2 5 3 0))
(eliminar_arb 10 '(10 (5 3 8) 18))



