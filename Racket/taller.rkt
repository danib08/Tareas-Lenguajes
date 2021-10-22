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


;------ Eliminar ------
(define (eliminar item lista)
  (cond ((null? lista ) '())
        ((equal? item (car lista)) (eliminar item (cdr lista)))
        (else (cons (car lista) (eliminar item (cdr lista))))))


;------ Quicksort ------

;------ Automovil ------
(define (automovil atributos valores)
  (cond ((or (null? valores) (null? atributos)) '())
        (else (cons (list (car valores) (car atributos)) (automovil (cdr atributos) (cdr valores))))))


;Llamadas
(factorial 13)
(fibonacci 17)
(miembro? 0 '(1 9 8 6 8 7 0 3 5))
(eliminar 5 '(1 2 4 7 5 9 0))
(automovil '(Hatchback Suzuki Forza1 Rojo si Manual) '(Tipo Marca Modelo Color AC Transmision))





