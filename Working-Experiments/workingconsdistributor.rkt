#lang racket

(require unstable/match)
;;;TODO Match on subexpressions
;;This is where match? comes from. If this changes we will merely use this version of racket
(provide lambda-matcher)
;;We are using an unstable part of match. This may make it so that we can't use any other version other than racket 6.1
(define (distribute-match-true x)
  ;;This function matches true if given a list with a given structure
  (match? x `(cons (cons ,a ,b)  ,c)))
(define (distribute a b c)
  ;;This function performs the distribution
  `(cons (cons ,a ,c) (cons ,b ,c)))
(define (tree-map test function list)
  (cond 
    [(null? list) '()]
    [(test (car list)) (cons (function list) ;;ignore garbage and perform test
                          (tree-map test function (cdr list)))]
    [else (cons
           (tree-map test (car list))
           (tree-map test (cdr list)))]))
(define (map-gen fun l)
   (if (null? l) '()
       (if (list? (car l))
           (cons (map-gen fun (car l)) (map-gen fun (cdr l)))
           (cons (fun (car l)) (map-gen fun (cdr l))))))
(define (tree-rewrite predicate function list)
  (print list)
  (cond 
    [(null? list) '()] 
    [(symbol? list) list] ;; Do nothing if we have encountered a symbol
    [(predicate list) (cons (function (cadadr list) (car (cddadr list)) (caddr list)) ;;Is horrible but we extract the relevant portions of the list if distribut-match-true returns true
                            (tree-rewrite predicate function (cdr list)))];;ignore garbage and perform test))]
                                ;;ignore garbage and perform test)
     [(and (pair? list) (predicate list)) 
                               
                          
                          (cons (function (cadadr list) (car (cddadr list)) (caddr list)) ;;Is horrible but we extract the relevant portions of the list if distribut-match-true returns true
                            (cons                                 
                                     (tree-rewrite predicate function (car list))
                                     (tree-rewrite predicate function (cdr list))))]
    [else (cons
           (tree-rewrite predicate function (car list))
           (tree-rewrite predicate function (cdr list)))]))
(define (lambda-matcher list)
  ;;We get the car of the list since that is the rewritten list
  (tree-rewrite distribute-match-true distribute list)) 
#|(define (recursive-lambda-matcher list)
   (let [(distribute-set (tree-map distribute-match-true list))]
     (define return-true (match distribute-set [#t #f]))))
|#
#|(define (search start target path)
 (letrec ([path (cons path start)]
          [search (lambda (start target path) (if (equal? start target)
                                             path
                                             (search start target (cons path start))))])  

    (begin
      (print path)
      (search (lambda-matcher start) target path))))
     
    
(search '(cons (cons a b) c) '(cons (cons a c) (cons b c)))

(define-simple-macro (distribute-macro (cons (cons a b) c)) (cons (cons a c) (cons a b)))
|#
(define (predicate list)
  (define accumulate
  (let ((acc 0))
    (λ (new-val)
      (set! acc (+ acc new-val))
      acc)))
  (let [(distribute-set (cons (distribute-match-true list) 
                                                     (map-gen distribute-match-true list)))]
             (list-ref distribute-set (- (accumulate 1) 1))))