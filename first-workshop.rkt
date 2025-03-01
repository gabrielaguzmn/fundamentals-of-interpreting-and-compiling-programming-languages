; Taller # 1 - Recursion
;
; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz 2324754-3743
; Juan Pablo Moreno 2372232-3743

#lang eopl

;append
(define juntarListas
  (lambda (l1 l2)
    (if (null? l1) l2
        (cons (car l1)
              (juntarListas (cdr l1) l2)))
    ))

;lenght
(define (longi L)
  (if (null? L)
      0
      (+ 1 (longi (cdr L)))))

;filter


(define (filtrar x L)
  (cond
    [(null? L) '()]   ; Caso base: lista vacía, devuelve lista vacía
    [(< (car L) x) (cons (car L) (filtrar x (cdr L)))] ; Si el primer elemento es menor que x, lo conserva
    [else (filtrar x (cdr L))])) ; Si no, lo omite y sigue con el resto

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
(display (prod-scalar-matriz '((1 1) (2 2) (3 3)) '(2 3))) (newline) ; -> ((2 3) (4 6) (6 9));punto 3
(define (list-set L n x)
                  (cond
                   [(null? L) '()] ;si la lista esta vacia, retorna una lista vacia
                   [(zero? n) (cons x (cdr L))] ; si n es 0 reemplaza el primer elemento
                   [(>= n (longi L)) 'error]  ; Si n es mayor o igual a la longitud de L, retorna 'error
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
  
  (cond
  [(equal? (longi(filtrar (car L) L)) 0) (inversions(cdr L))]
  [(> (longi(filtrar (car L) L)) 1) (inversions (juntarListas ((cdr L) ((longi(filtrar (car L) L))))) )]
  

  [else (car L)]
  )  
   
  
  )



  
;punto 12

(define (filter-acum a b F acum filter)
  (if (> a b)  ; Caso base: si a supera b, retornar acum
      acum
      (filter-acum (+ a 1) b F  ; Incrementamos a
                   (if (filter a) (F acum a) acum)  ; Aplicamos F si a cumple con filter
                   filter)))  ; Se mantiene el filtro
;punto 15

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
  
;punto 18


(define (sumar-consecutivos L)
  (if (or (null? L) (null? (cdr L)))
      '()
      (cons (+ (car L) (cadr L)) (sumar-consecutivos (cdr L)))))

(define (pascals-prev-row row)
  (cons 1 (juntarListas (sumar-consecutivos row) '(1))))

(define (pascal n)
  (if (= n 1)
      '(1)
      (pascals-prev-row (pascal (- n 1)))))

