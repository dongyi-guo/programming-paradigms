toy_time(buzz,   5).
toy_time(woody, 10).
toy_time(rex,   20).
toy_time(hamm,  25).

moves(Ms) :- phrase(moves(0,[buzz,woody,rex,hamm],[]), Ms).

moves(T0,Ls0,Rs0) -->
        { select(Toy1, Ls0, Ls1), select(Toy2, Ls1, Ls2),
          Toy1 @< Toy2,
          toy_time(Toy1, Time1), toy_time(Toy2, Time2),
          T1 is T0 + max(Time1,Time2), T1 =< 60 },
        [left_to_right(Toy1,Toy2)],
        moves_(T1,Ls2,[Toy1,Toy2|Rs0]).

moves_(_,[],_)     --> [].
moves_(T0,Ls0,Rs0) -->
        { select(Toy, Rs0, Rs1),
          toy_time(Toy, Time),
          T1 is T0 + Time, T1 =< 60 },
        [right_to_left(Toy)],
        moves(T1,[Toy|Ls0],Rs1).