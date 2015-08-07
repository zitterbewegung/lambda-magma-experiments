#lang racket
(require (planet wmfarr/permutations:1:3/permutations))
(require racket/set)
(define debug-flag #t)
(permutation-identity 10)
(define (left-distribute wff)
  (match wff
    [(list a (list b c)) (list (list a c) (list b c))]))
    
(define (bare-magma element steps)
    (define (left-distribute wff)
      (match wff
        [(cons (cons b c) a)(cons (cons b a) (cons c a))]))
  (if (= steps 0)
        element
      (bare-magma (left-distribute element) (- steps 1))))


(define kons
  (lambda (level wff)
    (define distr
      (match wff
        [(cons a (cons b c)) (cons (cons a c) (cons b c))]  ;123
        [(cons a (cons a a)) (cons (cons a a) (cons a a))]  ;111
        [(cons a (cons b a)) (cons (cons a b) (cons b a))]  
        [(cons c (cons c c)) (cons (cons c c) (cons c c))]
        [(cons c (cons b a)) (cons (cons c a) (cons b a))]
        [(cons c (cons b c)) (cons (cons c c) (cons b c))]
        [(cons a (cons a b)) (cons (cons a a) (cons a b))]
        [(cons a (cons b c)) (cons (cons a c) (cons b c))]
        [(cons a (cons b c)) (cons (cons a c) (cons b c))]))
    (if (= level 0)
        (display "Completed recursion")
        (begin 
          (display wff)
          (display "\n Next element \n")
          (kons (- level 1) distr)))))
  
(define kons-vector
  (lambda (level wff elementnumber)
    (define magma-elements (set ))
    (define distr
      (match wff
        [(cons (cons b c) a)(cons (cons b a) (cons c a))]))
    (if (= level 0)
        (display "Completed recursion")
        (begin 
          (display wff)
          (display "\n Next element \n")
          (kons (- level 1) distr)))))


(define debug 
  (if debug-flag
      (begin 
        (kons  100 '(list a (list b a)))
        ;(bare-magma (list 'a (list 'b 'c)) 5))
        ;(bare-magma (cons (cons 'b 'c )'a) 2)
        )
  #t))

;(kons 100 (kons  100 '(list a (list b a))))
;(kons 100 (kons  100 '(list a (list b c))))