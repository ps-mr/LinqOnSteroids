#lang racket
; Experiment with the essentials of automatic function lifting for Scala expression trees.
; I have to do this in Racket because in Scala the compiler crashes when I try to use them.

; A standard function
(define a (lambda (x) (+ x 1)))
(a 1)
; Expression nodes
(define (buildConst t) (list t))
; Expression nodes built by inject
(define (buildApp f p) (list f p))
(define (evalApp list) ((car list) (caadr list)))

(require racket/control)
; This transforms an expression tree into a plain value.
(define (project t) (shift k (buildApp k t)))
; I would offer a binding of reset as inject, but it seems non-trivial since reset is not a function but a macro.

(define b (reset (+ 1 (project (buildConst 4)))))
; Try evaluating 
(define c (reset (+ (project (buildConst 2)) (project (buildConst 4)))))
(evalApp b)
(evalApp c)
(evalApp (evalApp c))