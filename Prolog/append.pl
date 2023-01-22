append([],B,B).
append([A|R],B,[A|S]) :- append(R,B,S).