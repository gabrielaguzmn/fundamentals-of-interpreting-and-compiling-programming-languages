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

(display "CircuitosDT.rkt cargado correctamente") (newline)

; datatype-based grammar implementation

; constructors

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

; examples of creation

; Example 1
(define input-list1 (gateref-input_list 'A (empty-input_list)))
(define gate1 (a-gate 'G1 (not-type) input-list1))
(define gate-list1 (nonempty-gate_list gate1 (empty-gate_list)))
(define circuit1 (a-circuit gate-list1))

; Display the circuit
(write circuit1) (newline)

; Example 2
(define input-list2 (gateref-input_list 'A (gateref-input_list 'B (empty-input_list))))
(define gate2 (a-gate 'G1 (and-type) input-list2))
(define gate-list2 (nonempty-gate_list gate2 (empty-gate_list)))
(define circuit2 (a-circuit gate-list2))

; Display the circuit
(write circuit2)
(newline)

; Example 3
(define input-list3-1 (gateref-input_list 'A (gateref-input_list 'B (empty-input_list))))
(define input-list3-2 (gateref-input_list 'G1 (empty-input_list)))
(define gate3-1 (a-gate 'G1 (or-type) input-list3-1))
(define gate3-2 (a-gate 'G2 (not-type) input-list3-2))
(define gate-list3 (nonempty-gate_list gate3-1 (nonempty-gate_list gate3-2 (empty-gate_list))))
(define circuit3 (a-circuit gate-list3))

; Display the circuit
(write circuit3)
(newline)

; Example 4
(define input-list4-1 (gateref-input_list 'A (gateref-input_list 'B (empty-input_list))))
(define input-list4-2 (gateref-input_list 'A (gateref-input_list 'B (empty-input_list))))
(define input-list4-3 (gateref-input_list 'G2 (empty-input_list)))
(define input-list4-4 (gateref-input_list 'G1 (gateref-input_list 'G3 (empty-input_list))))
(define gate4-1 (a-gate 'G1 (or-type) input-list4-1))
(define gate4-2 (a-gate 'G2 (and-type) input-list4-2))
(define gate4-3 (a-gate 'G3 (not-type) input-list4-3))
(define gate4-4 (a-gate 'G4 (and-type) input-list4-4))
(define gate-list4 (nonempty-gate_list gate4-1 (nonempty-gate_list gate4-2 (nonempty-gate_list gate4-3 (nonempty-gate_list gate4-4 (empty-gate_list))))))
(define circuit4 (a-circuit gate-list4))

; Display the circuit
(write circuit4)
(newline)

(and-type)