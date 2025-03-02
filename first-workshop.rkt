; Taller # 1 - Recursion
;
; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz 2324754-3743
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

;; Punto 1 invert:
;; Propósito:
;; L -> L' : Procedimiento que recibe una lista de pares y devuelve una lista con los pares invertidos.
;;
;; <lista> := ()
;;         | (<par> <lista>)
;; <par> := (<valor> <valor>)

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

;; Punto 2 down:
;; Propósito:
;; L -> L' : Procedimiento que recibe una lista y devuelve una lista con cada elemento en una sublista.
;;
;; <lista> := ()
;;         | (<valor> <lista>)

(define down
  (lambda (l)
    (if (null? l)
        (list)
        (cons (list (car l)) (down (cdr l)
              )
            )
        )
    )
  )

;; Pruebas
(display(down '(1 2 3))) (newline) ; -> ((1) (2) (3))
(display(down '((una) (buena) (idea)))) (newline) ; -> (((una)) ((buena)) ((idea)))
(display(down '(un (objeto (mas)) complicado))) (newline) ; -> ((un) ((objeto (mas))) (complicado))

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



;; Punto #4 filter-in:
;; Proposito:
;; P x L -> L' : Procedimiento que filtra los elementos de L que cumplen el predicado P.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

(define filter-in
  (lambda (P L)
    (if (null? L)
        '()
        (if (P (car L))
            (cons (car L) (filter-in P (cdr L)))
            (filter-in P (cdr L))))))

;; Pruebas:
(display (filter-in number? '(a 2 (1 3) b 7))) (newline)
(display (filter-in symbol? '(a (b c) 17 foo))) (newline)
(display (filter-in string? '(a b u "univalle" "racket" "flp" 28 90 (1 2 3)))) (newline)

;; Punto 5 list-index:
;; Propósito:
;; P x L -> num : Procedimiento que devuelve el índice del primer elemento que cumple el predicado P.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

(define list-index
  (lambda (p l)
    (letrec
        (
         (aux
          (lambda (p l acc)
            (cond
              [(null? l) #f]
              [(p (car l)) acc]
              [else (aux p (cdr l) (+ acc 1))]
              )
            )
          )
         )
      (aux p l 0)
      )
    )
  )

;; Pruebas
(display(list-index number? '(a 2 (1 3) b 7))) (newline) ; -> 1
(display(list-index symbol? '(a (b c) 17 foo))) (newline) ; -> 0
(display(list-index symbol? '(1 2 (a b) 3))) (newline) ; -> #f

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


;; Punto #7 cartesian-product:
;; Proposito:
;; L1 x L2 -> L' : Procedimiento que devuelve el producto cartesiano entre dos listas.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

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

;; Punto 8 mapping:
;; Propósito:
;; (X -> Y) x L1 x L2 -> L' : Procedimiento que recibe una función y dos listas, y devuelve una lista de pares donde la función aplicada al primer elemento de cada par es igual al segundo elemento.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

(define mapping
  (lambda (f l1 l2)
    (if (or (null? l1) (null? l2))
        (list)
        (if (= (f (car l1)) (car l2))
            (cons (list (car l1) (car l2)) (mapping f (cdr l1) (cdr l2)))
            (mapping f (cdr l1) (cdr l2))))))

;; Pruebas
(display(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 2 4 6))) (newline) ; -> ((1 2) (2 4) (3 6))
(display(mapping (lambda (d) (* d 3)) (list 1 2 2) (list 2 4 6))) (newline) ; -> ((2 6))
(display(mapping (lambda (d) (* d 2)) (list 1 2 3) (list 3 9 12))) (newline) ; -> ()


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
;; Punto #10 up :
;; Proposito:
;; L -> L' : Procedimiento que remueve un nivel de paréntesis de la lista dada.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

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
(display (up '((1 2) (3 4)))) (newline)
(display (up '((x (y)) z))) (newline)
(display (up '(((a b)) (c d) e))) (newline)
(display (up '((p) q ((r s))))) (newline)

;; Punto 11 zip:
;; Propósito:
;; (X x Y -> Z) x L1 x L2 -> L' : Procedimiento que recibe una función y dos listas, y devuelve una lista resultante de aplicar la función a los elementos correspondientes de las dos listas.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

(define zip
  (lambda (f l1 l2)
    (if (null? l1)
        '()
        (cons (f (car l1) (car l2)) (zip f (cdr l1) (cdr l2))))))

;; Pruebas
(display (zip + '(1 4) '(6 2))) (newline); -> (7 6)
(display (zip * '(11 5 6) '(10 9 8))) (newline) ; -> (110 45 48)

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


;; Punto 13 operate:
;; Proposito:
;; L_ops x L_nums -> num : Procedimiento que aplica operaciones en secuencia a los números.
;;
;; <lista> := ()
;;          := (<valor> <lista>)

(define (operate operaciones numeros)
  (if (null? operaciones)
      (car numeros)
      (operate (cdr operaciones)
               (cons ((car operaciones) (car numeros) (cadr numeros))
                     (cddr numeros)))))
;; Pruebas
(display (operate (list + * + - *) '(1 2 8 4 11 6))) (newline)  ; -> 102
(display (operate (list *) '(4 5))) (newline)  ; -> 20
(display (operate (list + - * /) '(10 5 2 4 2))) (newline)  ; -> 6

;; Punto 14 path:
;; Propósito:
;; num x BST -> L' : Procedimiento que devuelve el camino para encontrar un número en un árbol binario de búsqueda.
;;
;; <BST> := (<valor> <BST> <BST>)
;;       := ()

(define path
  (lambda (n bst)
    (cond
      [(= (car bst) n) '()]
      [(> (car bst) n) (cons 'left (path n (cadr bst)))]
      [else (cons 'right (path n (caddr bst)))]
      )))

;; Pruebas

(display (path 5 '(10 (5 () ()) (15 () ())))) (newline)  ; -> '(left)
(display (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))) (newline) ; -> (right left left)

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


;; Punto #16 Operar-binarias :
;; Proposito:
;; OperacionB -> num : Procedimiento que evalúa expresiones binarias.
;;
;; <OperacionB> := <int>
;;              := (<OperacionB> 'suma <OperacionB>)
;;              := (<OperacionB> 'resta <OperacionB>)
;;              := (<OperacionB> 'multiplica <OperacionB>)

;; Función auxiliar my-length
;; Proposito:
;; L -> num : Procedimiento que devuelve la longitud de una lista.
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
(display (Operar-binarias 4)) (newline)
(display (Operar-binarias '(2 suma 9))) (newline)
(display (Operar-binarias '(2 resta 9))) (newline)
(display (Operar-binarias '(2 multiplica 9))) (newline)
(display (Operar-binarias '((2 multiplica 3) suma (5 resta 1)))) (newline)
(display (Operar-binarias '((2 multiplica (4 suma 1)) multiplica ((2 multiplica 4) resta 1)))) (newline)
(display (Operar-binarias '(2 divide 3))) (newline)  ; → 'operador-invalido)

;; Punto 17 prod-scalar-matriz:
;; Propósito:
;; Mat x Vec -> Mat' : Procedimiento que recibe una matriz y un vector, y devuelve una nueva matriz resultante de multiplicar cada fila de la matriz por el vector.
;;
;; <Mat> := ()
;;       := (<Vec> <Mat>)
;; <Vec> := ()
;;       := (<num> <Vec>)

(define prod-scalar-matriz
  (lambda (mat vec)
    (if (null? mat)
        '()
        (cons (zip * (car mat) vec) (prod-scalar-matriz (cdr mat) vec))
        )
    ))

;; Pruebas
(display (prod-scalar-matriz '((1 1) (2 2)) '(2 3))) (newline) ; -> ((2 3) (4 6))
(display (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))) (newline) ; -> ((2 3) (4 6) (6 9))
  
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

