#lang racket/base
 
(require rackunit
         "workingconsdistributor.rkt")

(check-equal? (lambda-matcher '(cons (cons a b) c)) '(cons (cons a c) (cons b c)) "Check for correct basic distribution")

(check-equal? (lambda-matcher '(cons (cons a c) (cons b c))) '(cons (cons a (cons b c)) (cons c (cons b c))) "Check for second level recursive distribution")
(check-equal? (lambda-matcher ' (cons a (cons e f)) (cons g (cons h z)) (cons c (cons b c))) '(cons (cons a (cons g (cons h z))) (cons (cons e f) (cons g (cons h z)))) "Two distribution in the same element")
#|(lambda-matcher (lambda-matcher '(cons (cons a b) c)))
(lambda-matcher (lambda-matcher '(cons (cons a b) c)))
(lambda-matcher (lambda-matcher '(cons (cons a (cons b c)) c)))
(lambda-matcher )
|#