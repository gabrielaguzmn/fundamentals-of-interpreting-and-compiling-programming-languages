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

#lang racket

; datatype-based grammar implementation