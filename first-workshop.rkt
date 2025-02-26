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

