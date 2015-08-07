#lang scheme

(define (string-to-tree string counter)
    (cond
      
      ((char-alphabetic? (car l)) (cons (car l) (to-tree (cdr l)))))
      ((char=? #\( (car l))) (cons  (list (car l) (to-tree (cdr l))) '()))
      ((char=? #\) (car l)) (cons '() (cons (car l) (to-tree (cdr l))))))
    (to-tree )

(define (string-to-tree string counter)
 (define currentchar (string-ref string counter))
  (cond
   [= counter (string-length string) '()]
   [(char-alphabetic? currentchar) (cons currentchar (string-to-tree string (+ 1 counter)))]
   [(char #\( currentchar)]))
(string-to-tree "")