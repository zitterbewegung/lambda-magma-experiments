#lang racket
(require unstable/match)
; Racket:
(define (filter/match p? lst)
  (match lst
    ['() '()]
    [(cons (? p?) tl)   (cons (car lst) (filter/match p? tl))]
    [(cons hd tl)       (filter/match p? tl)]))
(define (distribute a b c) (cons (cons a b) (cons a c)))

(define (match-true x) 
  (match? x `(cons (cons ,a ,b ) ,c)))

(filter/match match-true '(cons (cons a b) c))