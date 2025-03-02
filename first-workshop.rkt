; Taller # 1 - Recursion
;
; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz COD
; Juan Pablo Moreno 2372232-3743

#lang eopl


;; juntarListas : List List -> List
;; Propósito: Concatena dos listas l1 y l2.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

;append
(define juntarListas
  (lambda (l1 l2)
    (if (null? l1) l2
        (cons (car l1)
              (juntarListas (cdr l1) l2)))
    ))

;;pruebas
(display(juntarListas '(1 2 3) '(4 5 6))) (newline) ; Devuelve '(1 2 3 4 5 6)
(display(juntarListas '() '(a b c))) (newline) ; Devuelve '(a b c)

;; longi : List -> Number
;; Propósito: Devuelve la longitud de la lista L.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

;lenght

(define (longi L)
  (if (null? L)
      0
      (+ 1 (longi (cdr L)))))

;;pruebas
(display(longi '(a b c d))) (newline) ; Devuelve 4
(display(longi '())) (newline) ; Devuelve 0

; filtrar : Number List -> List
;; Propósito: Filtra los elementos de la lista L que son menores que x.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

;filter

(define (filtrar x L)
  (cond
    [(null? L) '()]   ; Caso base: lista vacía, devuelve lista vacía
    [(< (car L) x) (cons (car L) (filtrar x (cdr L)))] ; Si el primer elemento es menor que x, lo conserva
    [else (filtrar x (cdr L))])) ; Si no, lo omite y sigue con el resto

;;pruebas
 (display(filtrar 5 '(1 3 7 2 8))) (newline) ; Devuelve '(1 3 2)
 (display(filtrar 10 '(15 20 25))) (newline) ; Devuelve '()

;  point 2

(define down
  (lambda (l)
    (if (null? l)
        (list)
        (cons (list (car l)) (down (cdr l))))))

;punto 3

;; list-set : List Number Value -> List
;; Propósito: Reemplaza el elemento en la posición n de la lista L con el valor x.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

(define (list-set L n x)
                  (cond
                   [(null? L) '()] 
                   [(zero? n) (cons x (cdr L))] ; si n es 0 reemplaza el primer elemento
                   [(>= n (longi L)) 'error]  ; Si n es mayor o igual a la longitud de L, retorna 'error
                   [else (cons (car L) (list-set (cdr L) (- n 1) x))]
                   )
                  )

;;pruebas
(display(list-set '(a b c d) 2 '(1 2))) (newline)
(display(list-set '(a b c d) 3 'z)) (newline)


;punto 6

;; swapper : Value Value List -> List
;; Propósito: Intercambia todas las ocurrencias de E1 por E2 y viceversa en la lista L.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)
 
(define (swapper E1 E2 L)
  (cond
    [(null? L) '()]  ; Caso base: si la lista está vacía, retorna una lista vacía
    [(equal? (car L) E1) (cons E2 (swapper E1 E2 (cdr L)))]  ; Si el primer elemento es E1, lo cambia por E2
    [(equal? (car L) E2) (cons E1 (swapper E1 E2 (cdr L)))]  ; Si el primer elemento es E2, lo cambia por E1
    [else (cons (car L) (swapper E1 E2 (cdr L)))]))  ; Si no es ninguno, lo deja igual y sigue con el resto

;;pruebas
(display(swapper 'a 'd '(a b c d))) (newline) 
(display(swapper 'x 'y '(y y x y x))) (newline) 

;punto 9

;; inversions : List -> Number
;; Propósito: Cuenta el número de inversiones en la lista L.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

(define (inversions L)
  (if (null? L)
      0
      (+ (longi (filtrar (car L) (cdr L)))
         (inversions (cdr L)))))

;;pruebas
 (display(inversions '(2 3 8 6 1))) (newline) 
 (display(inversions '(1 2 3 4))) (newline) 


  
;punto 12

;; filter-acum : Number Number Function Value Function -> Value
;; Propósito: Acumula los valores de a a b aplicando la función F si el valor cumple con el filtro.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

(define (filter-acum a b F acum filter)
  (if (> a b)  ; Caso base: si a supera b, retornar acum
      acum
      (filter-acum (+ a 1) b F  ; Incrementamos a
                   (if (filter a) (F acum a) acum)  ; Aplicamos F si a cumple con filter
                   filter)))  ; Se mantiene el filtro

;;prueba
(display(filter-acum 1 10 + 0 odd?)) (newline)
(display(filter-acum 1 10 + 0 even?)) (newline)

;punto 15

;; count-odd-and-even : Tree -> List
;; Propósito: Cuenta el número de valores pares e impares en un árbol binario.
;;
;; <arbol> := ()
;;          := (<valor-de-scheme> <arbol> <arbol>)

(define (count-odd-and-even arbol)
  (if (null? arbol)
      '(0 0)  ; Caso base: árbol vacío, retorna (0,0)
      (let* ([valor (car arbol)]
             [izq (if (and (pair? (cdr arbol)) (pair? (cadr arbol))) (cadr arbol) '())]
             [der (if (and (pair? (cdr arbol)) (> (longi (cdr arbol)) 1) (pair? (caddr arbol))) (caddr arbol) '())]
             [conteo-izq (count-odd-and-even izq)]
             [conteo-der (count-odd-and-even der)]
             [pares (+ (if (even? valor) 1 0) (car conteo-izq) (car conteo-der))]
             [impares (+ (if (odd? valor) 1 0) (cadr conteo-izq) (cadr conteo-der))])
        (list pares impares))))

;;prueba
 (display(count-odd-and-even '(14 (7 () (12 () ()))
                          (26 (20 (17 () ()))
                              (31 () ()))))) (newline)

  
;punto 18

;auxiliar 
;; sumar-consecutivos : List -> List
;; Propósito: Suma cada par de elementos consecutivos en la lista L.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

(define (sumar-consecutivos L)
  (if (or (null? L) (null? (cdr L)))
      '()
      (cons (+ (car L) (cadr L)) (sumar-consecutivos (cdr L)))))

;auxiliar
;; pascals-prev-row : List -> List
;; Propósito: Genera la siguiente fila del triángulo de Pascal a partir de la fila dada.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

(define (pascals-prev-row row)
  (cons 1 (juntarListas (sumar-consecutivos row) '(1))))

;; pascal : Number -> List
;; Propósito: Genera la n-ésima fila del triángulo de Pascal.
;;
;; <lista> := ()
;;          := (<valor-de-scheme> <lista>)

(define (pascal n)
  (if (= n 1)
      '(1)
      (pascals-prev-row (pascal (- n 1)))))

;;pruebas

(display(sumar-consecutivos '(1 2 3 4))) (newline) ;; => '(3 5 7)
(display(sumar-consecutivos '(5 10 15))) (newline) ;; => '(15 25)

(display(pascals-prev-row '(1 3 3 1))) (newline)  ;; => '(1 4 6 4 1)
(display(pascals-prev-row '(1 2 1))) (newline)    ;; => '(1 3 3 1)

(display(pascal 5)) (newline) (newline)
(display(pascal 1)) (newline) (newline)

