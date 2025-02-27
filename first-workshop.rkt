; Taller # 1 - Recursion
;
; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz COD
; Juan Pablo Moreno COD

#lang eopl

;  point 2

(define down
  (lambda (l)
    (if (null? l)
        (list)
        (cons (list (car l)) (down (cdr l))))))

;punto 3
(define (list-set L n x)
                  (cond
                   [(null? L) '()] ;si la lista esta vacia, retorna una lista vacia
                   [(zero? n) (cons x (cdr L))] ; si n es 0 reemplaza el primer elemento
                   [(>= n (length L)) 'error]  ; Si n es mayor o igual a la longitud de L, retorna 'error
                   [else (cons (car L) (list-set (cdr L) (- n 1) x))]
                   )
                  )
;punto 6

(define (swapper E1 E2 L)
  (cond
    [(null? L) '()]  ; Caso base: si la lista está vacía, retorna una lista vacía
    [(equal? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]  ; Si el primer elemento es E1, lo cambia por E2
    [(equal? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]  ; Si el primer elemento es E2, lo cambia por E1
    [else (cons (car L) (swapper E1 E2 (cdr L)))]))  ; Si no es ninguno, lo deja igual y sigue con el resto
;punto 9

(define (inversions L)
  (define (count-inversions L rest)
    (cond
      [(null? rest) 0]  ; Si no quedan elementos en la lista, no hay más inversiones
      [else (+ (length (filter (lambda (x) (< x (car L))) (cdr rest))) 
               (count-inversions (cdr L) (cdr rest)))]))  ; Llamada recursiva

  (count-inversions L L))
  
;punto 12

(define (filter-acum a b F acum filter)
  (if (> a b)  ; Caso base: si a supera b, retornar acum
      acum
      (filter-acum (+ a 1) b F  ; Incrementamos a
                   (if (filter a) (F acum a) acum)  ; Aplicamos F si a cumple con filter
                   filter)))  ; Se mantiene el filtro
;punto 15

(define (count-odd-and-even tree)
  (if (null? tree)
      '(0 0)  ; Caso base: árbol vacío, retorna (0,0)
      (let* ([valor (car tree)]
             [izq (if (and (pair? (cdr tree)) (pair? (cadr tree))) (cadr tree) '())]
             [der (if (and (pair? (cdr tree)) (> (length (cdr tree)) 1) (pair? (caddr tree))) (caddr tree) '())]
             [conteo-izq (count-odd-and-even izq)]
             [conteo-der (count-odd-and-even der)]
             [pares (+ (if (even? valor) 1 0) (car conteo-izq) (car conteo-der))]
             [impares (+ (if (odd? valor) 1 0) (cadr conteo-izq) (cadr conteo-der))])
        (list pares impares))))
  
;punto 18

;append
(define juntarListas
  (lambda (l1 l2)
    (if (null? l1) l2
        (cons (car l1)
              (juntarListas (cdr l1) l2)))
    ))


(define (sumar-consecutivos lst)
  (if (or (null? lst) (null? (cdr lst)))
      '()
      (cons (+ (car lst) (cadr lst)) (sumar-consecutivos (cdr lst)))))

(define (pascals-prev-row row)
  (cons 1 (juntarListas (sumar-consecutivos row) '(1))))

(define (pascal n)
  (if (= n 1)
      '(1)
      (pascals-prev-row (pascal (- n 1)))))

