print_trip( Action, Code, Name, time( Hour, Minute)) :-
   upcase_atom( Code, Upper_code),
   format( "~6s  ~3s  ~s~26|  ~02d:~02d",
           [Action, Upper_code, Name, Hour, Minute]),
   nl.

test :-
   print_trip( depart, nyc, 'New York City', time( 9, 3)),
   print_trip( arrive, lax, 'Los Angeles', time( 14, 22)).

doSomething(nyc,lax) :- test.

main :- read(A),read(B), doSomething(A,B).
