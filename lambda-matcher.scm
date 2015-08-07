#lang racket

(module lambda-matcher racket

(require (for-syntax syntax/parse  unstable/syntax racket/match))
(match '((a b) ( c))
    [(list (list  x y ...) z ...) (list x y x z)])
(define (lambda-matcher x)
  (match x
    [(list (list  x y ...) z ...) (list (list x y) (list x z))]))
(lambda-matcher  '((a b) ( c)))
(lambda-matcher (lambda-matcher '((a b) ( c))))
(define (lambda-matcher2 x)
  (match x [(list (list  x y ...) z ...) (list (list x y) (list x z))]
               [(list (list w (list x) ...) (list y (list (list z)) ...)) (list (list w (list x y))(list w (list y z)))]))   
;(lambda-matcher2 (lambda-matcher2 '((a b) ( c))))
(lambda-matcher2 '((a (b)) (a ((c)))))
(lambda-matcher (lambda-matcher2 (lambda-matcher  '((a b) (c)))))

(define-syntax (lambda-matcher-syntax stx)
  (syntax-parse stx
   [((~literal match) x
      [((~literal list) ((~literal list) a b  (~literal ___)) c (~literal ___)) 
       ((~literal list) ((~literal list) d e) ((~literal list) f g))])
    (match (syntax->datum #'x)
      [(list (list a b ___) c  ___) 
       (list (list (syntax->datum #'d) (syntax->datum #'e)) (list (syntax->datum #'f) (syntax->datum #'g)))])]))

  )