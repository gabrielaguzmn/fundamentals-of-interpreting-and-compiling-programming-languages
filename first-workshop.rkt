; Taller # 1 - Recursion
;
; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz COD
; Juan Pablo Moreno COD

#lang eopl

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

;  point 2

(define down
  (lambda (l)
    (if (null? l)
        (list)
        (cons (list (car l)) (down (cdr l))))))

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
(define pair-with
  (lambda (x L)
    (if (null? L)
        '()  
        (cons (list x (car L)) (pair-with x (cdr L))))))  

;; Pruebas
(display (cartesian-product '(a b c) '(x y))) (newline)
(display (cartesian-product '(p q r) '(5 6 7))) (newline)
(display (cartesian-product '(1 2) '(a b c d))) (newline)
