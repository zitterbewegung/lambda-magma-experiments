#lang racket

#|

This file contains the full lam-v example from the PLT Redex website:

  http://redex.plt-scheme.org/

This file is parsed by the html generation code. The generation code
assumes that there are no blank lines in a definition. It pulls code
out of this file by starting from something that matches "(define ..." where
the ellipsis is replaced by a name, and then continues until it
encounters a blank line.

It also serves as a test suite for the code that shows 
up in the web page.

|#

(require redex)

(define-language λv
  (e (e e ...) (if0 e e e) x v)
  (v (λ (x ...) e) number +)
  (E (v ... E e ...) (if0 E e e) hole)
  (x (variable-except λ + if0)))

(define red
  (reduction-relation
   λv
   (--> (in-hole E (+ number_1 number_2))
        (in-hole E ,(+ (term number_1) 
                       (term number_2)))
        "+")
   (--> (in-hole E (if0 0 e_1 e_2))
        (in-hole E e_1)
        "if0t")
   (--> (in-hole E (if0 number_1 e_1 e_2))
        (in-hole E e_2)
        "if0f"
        (side-condition 
          (not (= 0 (term number_1)))))
   (--> (in-hole E ((λ (x ..._1) e) v ..._1))
        (in-hole E (subst-n (x v) ... e))
        "βv")))

(define-metafunction λv
  subst-n : (x any) ... any -> any
  [(subst-n (x_1 any_1) (x_2 any_2) ... any_3)
   (subst x_1 any_1 (subst-n (x_2 any_2) ... 
                             any_3))]
  [(subst-n any_3) any_3])

(define-metafunction λv
  subst : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2))
   (λ (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1)
                                (term (x_2 ...)))))]
  ;; 2. general purpose capture avoiding case
  [(subst x_1 any_1 (λ (x_2 ...) any_2))
   (λ (x_new ...) 
     (subst x_1 any_1
            (subst-vars (x_2 x_new) ... 
                        any_2)))
   (where (x_new ...)
          ,(variables-not-in
            (term (x_1 any_1 any_2)) 
            (term (x_2 ...))))]
  ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]
  ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction λv
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...)) 
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3) 
   (subst-vars (x_1 any_1) 
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

(define tests 0)
(define failing-tests 0)

(define-syntax (tst stx)
  (syntax-case stx ()
    [(_ x y) 
     (with-syntax ([l (syntax-line stx)])
       #'(tst/proc x y l))]))

(define-syntax (stst stx)
  (syntax-case stx ()
    [(_ x y) 
     (with-syntax ([l (syntax-line stx)])
       #'(stst/proc 'x (λ () x) y l))]))

(define (tst/proc x y l) 
  (set! tests (+ tests 1))
  (let ([got (apply-reduction-relation* red x)])
    (unless (equal? got (list y))
      (set! failing-tests (+ failing-tests 1))
      (fprintf (current-error-port)
               "~s -->* ~s, but expected ~s (line ~s)\n"
               x got y l))))

(define (stst/proc orig get-got expected l)
  (let ([got (with-handlers ((exn? exn-message)) (get-got))])
    (set! tests (+ tests 1))
    (unless (equal? got expected)
      (set! failing-tests (+ failing-tests 1))
      (fprintf (current-error-port)
               "~s =\n(term ~s), but expected:\n(term ~s) (line ~s)\n"
               orig got expected l))))

(define triangle
  '((λ (le) 
      ((λ (f) (le (λ (x) ((f f) x))))
       (λ (f) (le (λ (x) ((f f) x))))))
    (λ (triangle)
      (λ (x)
        (if0 x
             0
             (+ x (triangle (+ x -1))))))))

(traces red
        (term 
         ((λ (n) 
            (if0 
             n
             1
             ((λ (x) (x x)) 
              (λ (x) (x x)))))
          (+ 2 2))))

(begin
  (stst (term (subst x y x)) (term y))
  (stst (term (subst x y z)) (term z))
  (stst (term (subst x y (x (y z)))) (term (y (y z))))
  (stst (term (subst x y ((λ (x) x) ((λ (y1) y1) (λ (x) z)))))
        (term ((λ (x) x) ((λ (y2) y2) (λ (x) z)))))
  (stst (term (subst x y (if0 (+ 1 x) x x)))
        (term (if0 (+ 1 y) y y)))
  (stst (term (subst x (λ (z) y) (λ (y) x)))
        (term (λ (y1) (λ (z) y))))
  (stst (term (subst x 1 (λ (y) x)))
        (term (λ (y) 1)))
  (stst (term (subst x y (λ (y) x)))
        (term (λ (y1) y)))
  (stst (term (subst x (λ (y) y) (λ (z) (z2 z))))
        (term (λ (z1) (z2 z1))))
  (stst (term (subst x (λ (z) z) (λ (z) (z1 z))))
        (term (λ (z2) (z1 z2))))
  (stst (term (subst x z (λ (z) (z1 z))))
        (term (λ (z2) (z1 z2))))
  (stst (term (subst x3 5 (λ (x2) x2)))
        (term (λ (x1) x1)))
  (stst (term (subst z * (λ (z x) 1)))
        (term (λ (z x) 1)))
  (stst (term (subst q (λ (x) z) (λ (z x) q)))
        (term (λ (z1 x1) (λ (x) z))))
  (stst (term (subst x 1 (λ (x x) x)))
        (term (λ (x x) x))))

(begin
  (tst '((λ (x) x) 1) 1)
  (tst '((λ (x y) x) 1 2) 1)
  (tst '((λ (x y) y) 1 2) 2)
  (tst '(((λ (x) (λ (x) x)) 1) 2) 2)
  (tst '(((λ (x) (λ (y) x)) 1) 2) 1)
  (tst '((λ (x) (+ x x)) 2) 4)
  (tst '((λ (x) (if0 x x (+ x 1))) 2) 3)
  (tst '((λ (x) (if0 x x (+ x 1))) 0) 0)
  (tst '(((λ (x) (λ (y z) x)) 1) 2)
       '((λ (y z) 1) 2))
  (tst `(,triangle 5) (+ 5 4 3 2 1 0)))

(if (= 0 failing-tests)
    (printf "~a tests, all passed.\n" tests)
    (printf "~a tests failed.\n" failing-tests))

;; the following program demonstrates the exponential
;; time slowdown discussed on the web page. As is,
;; it runs in linear time (the size of the program is `n' below).

;; But if you change the call to subst-var to be
;; a call to subst-n, the tests will still all pass,
;; but you'll see a considerable slowdown here.

(define (run-timing-tests)
  (let loop ([n 5])
    (unless (= n 30)
      (let-values ([(res cpu real gc) (time-apply (λ () (term (subst x ,(gen-app n) ,(gen-lams n)))) '())])
        (printf "n=~a, ~a msec\n" n (- cpu gc))
        (loop (+ n 1))))))

(define (gen-app n)
  (cond
    [(zero? n) 'x]
    [else
     `(,(ith-id n) ,(gen-app (- n 1)))]))

(define (ith-id n)
  (cond
    [(<= 0 n 25)
     (string->symbol (string (integer->char (+ n (char->integer #\a)))))]
    [else
     (string->symbol (string (integer->char (+ n -26 (char->integer #\A)))))]))

(define (gen-lams n)
  (cond
    [(zero? n) 'x]
    [else
     `(λ (,(ith-id n)) ,(gen-lams (- n 1)))]))

(define value? (redex-match λv v))
(define (single-step? e)
  (= (length (apply-reduction-relation red e))
     1))
(redex-check λv
             e
             (or (value? (term e))
                 (single-step? (term e))))