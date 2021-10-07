#lang racket

; Arbol

(define (arbol centro izq der)
  ( cond ((and (null? izq) (null? der)) 
         centro)  ;Retorna el centro si los hijos son nulls
   
  ( else  
    (list centro izq der))))


; Aux Hoja

(define (hoja? item)
  (not (list? item))
 )


; Raiz

(define (raiz arbol)
  (cond ( (hoja? arbol) ;Si no tiene hijos, retorna la raiz
          arbol)
        
  (else (car arbol)))) ;Retorna primer elemento


; Hijo izquierdo

(define (hijo-izq arbol)
  (cond ( (hoja? arbol) ;Si no tiene hijos, retorna lista vacia
          '())
   ( else
     (cadr arbol)))) ;Retorna primer elemento del segundo elemento del arbol


; Hijo derecho
(define (hijo-der arbol)
  (cond ( (hoja? arbol)
        '())
  (else
    (caddr arbol)))) ;Retorna primer elemento del tercer elemento del arbol

; Menor
(define (menor arbol)
  (cond ( (null? arbol)
         #f)
        
  ( (null? (hijo-izq arbol))
    (raiz arbol))

  ( else
   (menor (hijo-izq arbol)))))


; Eliminar
(define (eliminar item arbol)
  (cond ( (null? arbol) ;Condicion de parada si item no existe
        '())

        ;Buscar el nodo
        ( (< item (raiz arbol))
          (arbol (raiz arbol) (eliminar item (hijo-izq arbol)) (hijo-der arbol)))

        ( (> item (raiz arbol))
          (arbol (raiz arbol) (hijo-izq arbol) (eliminar item (hijo-der arbol))))

        ;Cuando se llega al item como raiz

        ;Item sin hijos
        ( (and (null? (hijo-izq arbol)) (null? (hijo-der arbol)))
         '())

        ;Item con hijo derecho
        ( (null? (hijo-izq arbol))
          (hijo-der arbol))

        ;Item con hijo izquierdo
        ( ( null? (hijo-der arbol))
          (hijo-izq arbol))

        ;Item con dos hijos
        ( else
          (arbol (menor (hijo-der arbol)) (hijo-izq arbol) (eliminar (menor (hijo-der arbol) (hijo-der arbol)))))
   ))


(eliminar 14 '(10 (5 3 8) (15 14 18)))





