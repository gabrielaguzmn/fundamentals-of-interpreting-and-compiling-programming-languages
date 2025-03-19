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

#lang eopl

; list-based grammar implementation

; constructors 

(define make-input_list (lambda input (cons 'input_list input)))

(define make-type (lambda (op) (list 'type op)))

(define make-gate (lambda (gateid type il) (list 'gate gateid type il)))

(define make-gate_list (lambda gates (cons 'gate_list gates)))

(define make-circuit (lambda (gl) (list 'circuit gl)))

; extractors

(define input_list->first (lambda (il) (if (null? (cdr il)) '() (cadr il))))

(define input_list->rest (lambda (il) (if (null? (cdr il)) '() (cddr il))))

(define gate->gate_id (lambda (gate) (cadr gate)))

(define gate->type (lambda (gate) (caddr gate)))

(define gate->input_list (lambda (gate) (cadddr gate)))

(define gate_list->first (lambda (gl) (if (null? (cdr gl)) '() (cadr gl))))

(define gate_list->rest (lambda (gl) (if (null? (cdr gl)) '() (cddr gl))))

(define circuit->gate_list (lambda (circuit) (cadr circuit)))

; examples of creation

(display(make-circuit (make-gate_list 
        (make-gate 'G1 (make-type 'not) (make-input_list 'A))))) (newline)
        ; -> (circuit (gate_list (gate G1 (type not) (input_list A))))

(display(make-circuit (make-gate_list 
        (make-gate 'G1 (make-type 'and) (make-input_list 'A 'B))))) (newline)
        ; -> (circuit (gate_list (gate G1 (type and) (input_list A B))))

(display(make-circuit (make-gate_list
        (make-gate 'G1 (make-type 'or) (make-input_list 'A 'B))
        (make-gate 'G2 (make-type 'not) (make-input_list 'G1))))) (newline)
        ; -> (circuit (gate_list (gate G1 (type or) (input_list A B)) (gate G2 (type not) (input_list G1))))

(display(make-circuit (make-gate_list
        (make-gate 'G1 (make-type 'or) (make-input_list 'A 'B))
        (make-gate 'G2 (make-type 'and) (make-input_list 'A 'B))
        (make-gate 'G3 (make-type 'not) (make-input_list 'G2))
        (make-gate 'G4 (make-type 'and) (make-input_list 'G1 'G3))))) (newline)
        ; -> (circuit (gate_list (gate G1 (type or) (input_list A B)) (gate G2 (type and) (input_list A B)) 
        ; (gate G3 (type not) (input_list G2)) (gate G4 (type and) (input_list G1 G3))))

; examples of use of the extractors

(define a (make-circuit (make-gate_list
        (make-gate 'G1 (make-type 'or) (make-input_list 'A 'B))
        (make-gate 'G2 (make-type 'not) (make-input_list 'G1))
        (make-gate 'G3 (make-type 'xor) (make-input_list 'C)))))

; Extract the gate list from the circuit
(define gate-list-of-a (circuit->gate_list a))

(display gate-list-of-a) (newline) ; (gate_list (gate G1 (type or) (input_list A B)) (gate G2 (type and) (input_list A B)) 
                                   ; (gate G3 (type xor) (input_list C)))

; Get the first gate from the gate list
(define first-gate (gate_list->first gate-list-of-a))

(display first-gate) (newline) ; -> (gate G1 (type or) (input_list A B))

; Get the rest of the gate list
(define rest-gate (gate_list->rest gate-list-of-a))

(display rest-gate) (newline) ; -> ((gate G2 (type not) (input_list G1)) (gate G3 (type xor) (input_list C)))

; Get the information of the first gate
(display (gate->gate_id first-gate)) (newline); -> G1

(display (gate->type first-gate)) (newline) ; -> (type or)

(display (gate->input_list first-gate)) (newline) ; -> (input_list A B)

; Get the first and last of the input list for the first gate
(define input-list-of-first-gate (gate->input_list first-gate))

(display (input_list->first input-list-of-first-gate)) (newline) ; -> A

(display (input_list->rest input-list-of-first-gate)) (newline) ; -> (B)