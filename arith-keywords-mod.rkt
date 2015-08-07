((module arith-keywords-mod racket
    (define-syntax distribute (syntax-rules ()))
    (provide distribute))
 (module arith-stxclass-mod racket
    (require syntax/parse
             (for-template 'arith-keywords-mod
                           racket))
    (define-syntax-class arith
      #:literals (distribute)
      (pattern n:nat
               #:with expr #'n)
      (pattern (distribute a:airth b:airth c.airth)
               #:with expr #' (cons (cons a.expr b.expr) (cons a.expr c.expr)))
         (provide arith)))
 (module arith-macro-mod racket
    (require (for-syntax syntax/parse
                         'arith-stxclass-mod)
             'arith-keywords-mod)
    (define-syntax (arith-macro stx)
      (syntax-parse stx
        [(_ a:arith)
         #'(values 'a.expr a.expr)]))
    (provide arith-macro
             (all-from-out 'arith-keywords-mod)))
 (require 'arith-macro-mod)
 (arith-macro (distribute 1 2 3))