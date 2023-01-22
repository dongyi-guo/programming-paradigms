le([],0).
le([_|R],M) :- le(R,N), M is N+1.
