
expr --> term.
expr --> term, [+], expr.
expr --> term, [-], expr.
term --> num.
term --> num, [*], term.
term --> num, [/], term.
num  --> [D], { number(D) }.

parse(E) :- expr(E, []).

/* string_lower/2 , string_upper/2 
 * current_predicate/1
 * atom_string/2 , string_chars/2
 */


whitespace(X) :- member(X,[' ','\n','\t','\r']), !. 

takeWhileChar([X|XS],Y) :- not(whitespace(X)), !, append(Y,[X],R), !, takeWhileChar(XS,R).

pack([],X) :- !.
pack([X|XS], []) :- whitespace(X), !,pack([XS],[]). 
pack([X|XS], []) :- pack([XS],[[X]]).
pack([X,D|XS], [YS|ZS]) :- 
    not(whitespace(X)),
    whitespace(D),  
    append(YS,[X],R),
    append(ZS,[R],P),
    pack(XS,P).
pack([X,D|XS], [Y|YS]) :- 
    not(whitespace(X)),
    not(whitespace(D)), 
    append(Y,[X],R),
    append([R],YS,P),
    pack([D|XS],P).
pack([X,D|XS], ZS) :- 
    whitespace(X),
    whitespace(D),
    pack([XS],ZS).
pack([X],[Y|YS]) :- 
    whitespace(X),
    pack([],[Y|YS]).
pack([X],[Y|YS]) :-
    not(whitespace(x)),
    append(Y,[X],R),
    append(YS,R,P),
    pack([],P).
