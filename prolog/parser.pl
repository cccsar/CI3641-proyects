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
 * split_string/4
 * char_code/2, between/3
 * string_codes/2
 */

name --> [N], { is_propper_noun(N) }.
name --> [N], { is_propper_noun(N) }, name.
remPronouns --> [N], { member(N,['i','you','he','she','it','we','they','I','You','He','She','It','We','They']) } 


my_parse(E) :- name(E,[]).

is_propper_noun(X)　:- 
    atom_codes(X,[Y|YS]),
    check_range([Y],65,90), %initial uppercased
    check_range(YS,97,122). %remaining lowercased

% Checks that a string's characters are on some range
check_range([],_,_).
check_range([X|XS],U,B) :-
    between(U,B,X),
    check_range(XS,U,B).
 
main(X) :- 
    split_string(X," \n\t\r"," \n\t\r",L),
    string_to_atoms(L,[],R),
    my_parse(R),
    write(R), !.
    
string_to_atoms([X|XS],Y,Q) :-
    %string_lower(X,I), no hizo falta porque el atomo de "PRUEBA" seria 'PRUEBA' 
    atom_string(P,X),
    append(Y,[P],R),
    string_to_atoms(XS,R,Q).
string_to_atoms([],Y,Y) .
