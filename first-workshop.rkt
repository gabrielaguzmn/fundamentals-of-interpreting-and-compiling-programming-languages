; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz 2324754-3743
; Juan Pablo Moreno 2372232-3743

#lang eopl

;  point 2

(define down
  (lambda (l)
    (if (null? l)
        (list)
        (cons (list (car l)) (down (cdr l))))))


;; Punto #1 invert :
(define invert
  (lambda (l)
    (if (null? l)
        '()  ; Caso base: lista vacía
        (cons (list (cadr (car l)) (car (car l)))
              (invert (cdr l))))))

;; Pruebas
(display (invert '((a 1) (a 2) (1 b) (2 b)))) (newline)
(display (invert '((5 9) (10 91) (82 7) (a e) ("hola" "Mundo")))) (newline)
(display (invert '(("es" "racket") ("genial" "muy") (17 29) (81 o)))) (newline)

;; Punto #4 filter-in :

(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))

;; Pruebas del profe
(display (filter-in number? '(a 2 (1 3) b 7))) (newline)   ;  (2 7)
(display (filter-in symbol? '(a (b c) 17 foo))) (newline)  ;  (a foo)
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))) (newline)  ; ("univalle" "racket" "flp")

;; Punto #7 cartesian-product :

(define cartesian-product
  (lambda (L1 L2)
    (if (or (null? L1) (null? L2))
        '()
        (append (pair-with (car L1) L2)
                (cartesian-product (cdr L1) L2)))))

;; Función auxiliar pair-with
;; Proposito:
;; x x L -> L' : Procedimiento que combina un valor con cada elemento de la lista.
(define pair-with
  (lambda (x L)
    (if (null? L)
        '()
        (cons (list x (car L)) (pair-with x (cdr L))))))

;; Pruebas
(display (cartesian-product '(a b c) '(x y))) (newline)
(display (cartesian-product '(p q r) '(5 6 7))) (newline)
(display (cartesian-product '(1 2) '(a b c d))) (newline)

;; Punto 10 up :
(define up
  (lambda (L)
    (if (null? L)
        '()
        (append (unpack (car L))
                (up (cdr L))))))

;; Función auxiliar unpack
;; Proposito:
;; x -> L' : Procedimiento que devuelve la lista si x es lista, o lo envuelve en una.
(define unpack
  (lambda (x)
    (if (list? x)
        x
        (list x))))

;; Pruebas
(display (up '((1 2) (3 4)))) (newline)  ; prueba del profesor (1 2 3 4) 
(display (up '((x (y)) z))) (newline)   ; prueba del profesor (x (y) z)
(display (up '(((a b)) (c d) e))) (newline)  ; mi prueba ((a b) c d e)
(display (up '((p) q ((r s))))) (newline)  ; mi prueba (p q (r s))

;; Punto 13 operate :
(define (operate operaciones numeros)
  (if (null? operaciones)
      (car numeros)
      (operate (cdr operaciones)
               (cons ((car operaciones) (car numeros) (cadr numeros))
                     (cddr numeros)))))
;; Pruebas
(display (operate (list + * + - *) '(1 2 8 4 11 6))) (newline) 
(display (operate (list *) '(4 5))) (newline)  
(display (operate (list + - * /) '(10 5 2 4 2))) (newline)

;; Punto 16 Operar-binarias :
;; Definición de  Funcion auxiliar my-length para contar los elementos de una lista
(define my-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (my-length (cdr lst))))))

;; Definición de Operar-binarias
(define Operar-binarias
  (lambda (operacionB)
    (cond
      ((number? operacionB) operacionB)
      ((and (list? operacionB) (= (my-length operacionB) 3))
       (let ((izq (Operar-binarias (car operacionB)))
             (op (cadr operacionB))
             (der (Operar-binarias (caddr operacionB))))
         (cond
           ((eqv? op 'suma) (+ izq der))
           ((eqv? op 'resta) (- izq der))
           ((eqv? op 'multiplica) (* izq der))
           (else 'operador-invalido))))
      (else 'expresion-invalida))))

      ;; Pruebas
(display (Operar-binarias 4)) (newline)  ; -> 4
(display (Operar-binarias '(2 suma 9))) (newline)  ; -> 11
(display (Operar-binarias '(2 resta 9))) (newline)  ; -> -7
(display (Operar-binarias '(2 multiplica 9))) (newline)  ; -> 18
(display (Operar-binarias '((2 multiplica 3) suma (5 resta 1)))) (newline)  ; -> 10
(display (Operar-binarias '((2 multiplica (4 suma 1)) multiplica ((2 multiplica 4) resta 1)))) (newline)  ; -> 70
(display (Operar-binarias '(2 divide 3)))  ; → 'operador-invalido)
