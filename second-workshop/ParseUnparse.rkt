#lang eopl

; Gabriela Guzmán 2326772-3743
; Valentina Sanchéz 2324754-3743
; Juan Pablo Moreno 2372232-3743

; -------------------------------------------------------------------------- ;
;                                 BNF GRAMMAR                                ;
; -------------------------------------------------------------------------- ;

; <circuit> ::= '(circuit <gatelist>)
; <gatelist> ::= empty | <gate> <gatelist>
; <gate> ::= '(gate <gateid> <type> <inputlist>)
; <gateid> ::= identificador de la compuerta
; <type> ::= and | or | not | xor
; <inputlist> ::= empty | <bool> <inputlist>
;               | <gateref> <inputlist>
; <gateref> ::= identificador de otra compuerta

(define-datatype circuit circuit?
  (a-circuit
   (gate_list gate_list?)))

(define-datatype gate_list gate_list?
  (empty-gate_list)
  (nonempty-gate_list
   (gate gate?)
   (rest gate_list?)))

(define-datatype gate gate?
  (a-gate (gate_id symbol?)
          (type type?)
          (input_list input_list?)))

(define-datatype type type?
  (and-type)
  (or-type)
  (not-type)
  (xor-type))

(define-datatype input_list input_list?
  (empty-input_list)
  (bool-input_list
   (bool boolean?)
   (rest input_list?))
  (gateref-input_list
   (gateref symbol?)
   (rest input_list?)))


; -------------------------------------------------------------------------- ;
;                               PARSEADOR BNF                                ;
; -------------------------------------------------------------------------- ;

(define (PARSEBNF datum)
  (let ((gate-list (cadr datum)))  ; Extraemos la lista de compuertas
    (a-circuit (parse-gate-list (cdr gate-list)))))  ; Convertimos a `gate_list`

(define (parse-gate-list lst)
  (if (null? lst) 
      (empty-gate_list)  ; Lista vacía -> `empty-gate_list`
      (nonempty-gate_list 
       (parse-gate (car lst))  ; Convertimos el primer `gate`
       (parse-gate-list (cdr lst)))))  ; Recursión para el resto

(define (parse-gate lst)
  (a-gate (cadr lst)  ; `gate_id`
          (parse-type (cadr (caddr lst)))  ; Convertimos el tipo
          (parse-input-list (cadddr lst))))  ; Convertimos `input_list`

(define (parse-type type)
  (case type
    [(and) (and-type)]
    [(or) (or-type)]
    [(not) (not-type)]
    [(xor) (xor-type)]))

(define (parse-input-list lst)
  (if (null? lst) 
      (empty-input_list)  ; `empty-input_list`
      (if (boolean? (car lst))  ; Booleano -> `bool-input_list`
          (bool-input_list (car lst) (parse-input-list (cdr lst)))
          (gateref-input_list (car lst) (parse-input-list (cdr lst))))))  ; Identificador -> `gateref-input_list`

; -------------------------------------------------------------------------- ;
;                               EJEMPLOS DE PRUEBA                           ;
; -------------------------------------------------------------------------- ;

(display " Ejemplo de prueba 1 ---")
(newline)
(display (PARSEBNF '(CIRCUIT (gate_list (gate G1 (type not) (input_list #t))))))
(newline)
(display "Ejemplo de prueba 2 ---")
(newline)
(display (PARSEBNF '(circuit (gate_list (gate G1 (type and) (input_list A B))))))
(newline)

; -------------------------------------------------------------------------- ;
;                               UNPARSEADOR BNF                              ;
; -------------------------------------------------------------------------- ;


;; Convierte un árbol de sintaxis abstracta (circuito) a su representación en listas
(define UNPARSEBNF
  (lambda (arb)
    (cases circuit arb
      (a-circuit (gates) (list 'circuit (cons 'gate_list (UNPARSEBNF-gate-list gates)))))))

;; Convierte una lista de compuertas a su representación en listas
(define UNPARSEBNF-gate-list
  (lambda (arb)
    (cases gate_list arb
      (empty-gate_list () '())  ;; Caso base: lista vacía
      (nonempty-gate_list (head tail) 
        (cons (UNPARSEBNF-gate head) (UNPARSEBNF-gate-list tail)))))) ;; Recursión sobre la lista de compuertas

;; Convierte una compuerta a su representación en listas
(define UNPARSEBNF-gate
  (lambda (arb)
    (cases gate arb
      (a-gate (id type inputs) 
        (list 'gate id (list 'type (UNPARSEBNF-type type)) (cons 'input_list (UNPARSEBNF-inputs inputs)))))))

;; Convierte un tipo de compuerta a su representación como símbolo
(define UNPARSEBNF-type
  (lambda (arb)
    (cases type arb
      (and-type () 'and)  ;; Compuerta AND
      (or-type () 'or)    ;; Compuerta OR
      (not-type () 'not)  ;; Compuerta NOT
      (xor-type () 'xor)))) ;; Compuerta XOR

;; Convierte una lista de entradas a su representación en listas
(define (UNPARSEBNF-inputs arb)
  (cases input_list arb
    (empty-input_list () '())  ;; Caso base: lista vacía
    (bool-input_list (value next) (cons value (UNPARSEBNF-inputs next)))  ;; Entrada booleana
    (gateref-input_list (ref next) (cons ref (UNPARSEBNF-inputs next))))) ;; Referencia a otra compuerta

; -------------------------------------------------------------------------- ;
;                               EJEMPLOS DE PRUEBA UNPARSE                   ;
; -------------------------------------------------------------------------- ;

(define circuit-ejemplo
  (a-circuit
   (nonempty-gate_list
    (a-gate 'G1 (not-type)
            (gateref-input_list 'A (empty-input_list)))
    (empty-gate_list))))

;; Muestra el resultado de la conversión de un circuito a listas
(display "Ejemplo de prueba 1 UNPARSE")
(display (UNPARSEBNF circuit-ejemplo)) (newline)
