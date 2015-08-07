#lang racket
(require redex)

(require redex/tut-subst)
(define-language lambda-magma
  (e (e (e e ...))
     (distr e ...)
     x
     (x variable-not-otherwise-mentioned)))
;;;Start typing definitions
(define-extended-language L+Γ lambda-magma
  [Γ · (x : t Γ)])
(define-judgment-form
  L+Γ
  #:mode (types I I O)
  #:contract (types Γ e t)
 
  [(types Γ e t)
   -------------------------
   (types Γ e t)])
(define-metafunction lambda-magma
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? (list (term x)) (list (term v)) (term e))])
(define x? (redex-match lambda-magma x))
(define-extended-language  lambda-magma-extended L+Γ
  (p (e ...)) ;Programs
  (P (e ... E e ...))
  (E (e (e e ...) ...)
     (distr E ...)
     (distr (e (e e ...) ...) ...)
     hole
     x
     (x variable-not-otherwise-mentioned)))


(define red
  (reduction-relation
   lambda-magma-extended
   #:domain p
   (--> (in-hole P (distr (e_1 (e_2 e_3))))
                 (in-hole P (distr e_1 e_2 e_1 e_3))
                 "distr")))
((generate-term #:source red) 1)