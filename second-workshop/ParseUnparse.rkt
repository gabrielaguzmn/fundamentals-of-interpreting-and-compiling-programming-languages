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


