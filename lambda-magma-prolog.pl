S -->  (D(D(D)) .
(D(D(D))) --> ( a b )( a c ).
term(var(X)) :- variable(X).
term(app(T1,T2)) :- term(T1), term(T2).
term(lam(X,T)) :- variable(X), term(T).
value(lam(X,T)) :- variable(X), term(T).
variable(expr(expr(X,Y),Z)) :- expr(expr(a,b),



