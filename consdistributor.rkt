#lang racket

(require unstable/match)
#|(define (tree-search function list)
  (print list)
  (cond 
    [(null? list) '()]
    [(or (symbol? (car list)) ) 
                          (tree-search function (cdr list)))] ;;ignore garbage and perform test
    [else (cons
           (tree-search function (car list))
           (tree-search function (cdr list)))]))
|#
(define (match-true x) 
  (match? x `(cons (cons ,a ,b ) ,c)))
(define (distribute a b c)
  (list (list a b)) c)
(define (tree-rewrite function list)
  (print list)
  (cond 
    [(null? list) '()]
    [(match-true list) (cons (distribute (cadadr list) (car (cddadr list)) (caddr list))
                          (tree-rewrite function (cdr list)))];;ignore garbage and perform test
    [else (cons
           (tree-rewrite function (car list))
           (tree-rewrite function (cdr list)))]))
(define (lambda-matcher2 list)
  (tree-rewrite match-true list))
(lambda-matcher2 '(cons (cons a b) c))
#|
(define (lambda-matcher y)
  (begin 
  (define x y)
  (print "Thing to be distributed:")
  (print y)
  
  (tree-search (lambda (x)
                 (match x 
    
    [`(cons (cons ,a ,b ) ,c)  '(cons (cons a c) (cons b c))]
  
    [`,a 'a])) y)))
(car (lambda-matcher '(cons (cons a b) c)))
 (lambda-matcher (car (lambda-matcher '(cons (cons a b) c))))

|#