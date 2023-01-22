parent(b,a). parent(c,b).
parent(d,e). parent(c,d).
sg(P,P).
sg(P,Q) :- parent(X,P), parent(Y,Q), sg(X,Y).