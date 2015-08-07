#lang racket
(require racket/stream)
(require racket/generator)
(require racket/match)
(require (planet samsergey/rewrite:1:0))
;(;;;Axioms
;;;(ab)c = (ab)(ac)
;Z = (lambda f . (lambda f . a) a )
;Za = (Za)a
;(struct Prod (a b c) #:transparent)
;(struct Z (a b) #:transparent))
(define-formal Prod 1- Sum Z a b c)

 (define algebra
   (/.
 ;   (Prod 0 _) --> (display "Stop")
    ; (Prod _ ) --> (begin (Prod a) (display _))
   
    (Z a '()) --> (Z a a) 
;   (Prod (Prod a b) (Prod a c)) -->  (Prod (Prod a b) c)     
    (Prod a b c) --> (Prod (Prod a b) (Prod a c))))
                     
 (define calculator  
   ((generator ()
              (let loop ([x '(Prod (Prod Z a) a)])
                (if (null? x)
                    0
                    (begin
                      (yield (algebra x))
                      (loop (algebra x))))))))

;;;Deterministic reducer 
(define reduce
    (/. ; η-reduction
         ;f --> (display f)
         ;`(λ. ,x (,f ,x)) --> f
         ; β-reduction
         ;`((λ. ,x ,B) ,A) --> (eval `((/. ',x --> ',A) ',B))
         ;(Prod Z a) --> `(Z (λ. ,a (λ. ,a)))
          (Prod Z a) --> (Prod (Prod Z a) a) 
          (Prod (Prod (? symbol? a) (? symbol? b) (? symbol? c))) --> (Prod (Prod a b) (Prod a c))))
 
(define reducer (generator ()
              (let loop ([x '(Prod (Prod Z a) a)])
                (if (null? x)
                    0
                    (begin
                      (display x)
                      (yield (reduce x))
                      (loop (reduce x)))))))

(define leftdistributivity (/. (Prod (Prod (? symbol? a) (? symbol? b) (? symbol? c))) --> (Prod (Prod a b) (Prod a c))))
(define leftdistributivity-match
  (match-lambda '(* a (* b c))
    [(list * a (list * b c)) (* (* a b) (* b c))]))



  
 #|
rule2 = {X[a_ Prod b_] Prod X[c_ ] :> X[X[a Prod c] Prod X[b Prod c]], 
Z[X[a_ Prod b_ Prod c_ ]] :> Z [X[a Prod b Prod c] Prod X[a Prod b Prod c]] } ;
DIT[t_] := Simplify[(t) /. rule2]; |#

  
;   (define (lambdamagma wff steps)
;  
;     (define (reducer wff steps) 
;       (generator ()
;                  (define-formal Z Prod a b c N)
;                  (define reduce-rule
;                    (./ (N 0) --> 0
;                        (N n) --> (N (- n 1))
;                        (Prod a b c (N n)) -->   (Prod (Prod a c (N  n))) (Prod b c (N n))
;                        (Z (Prod a b c (N n))) -->  (Z (Prod (Prod a b c (N n)) (Prod a b c (N n)))))
;                    (let loop ([x 'wff])
;                      (if (= counter 0)
;                          0
;                          (begin
;                            (yield (reduce-rule x)))
;                            (loop (reduce-rule x)))))))
;       (reducer wff steps))

;(define reduce-rule
;                   (replace-all-repeated 
;                     (N 0) --> 0
;                       (N n) --> (N (1- (- n 1)))
;                       (Prod a b c (N n)) -->   (Prod (Prod a c (N  n))) (Prod b c (N n))
;                       (Z (Prod a b c (N n))) -->  (Z (Prod (Prod a b c (N n)) (Prod a b c (N n))))))     
;(reduce-rule (Z (Prod a b c (N 10))))