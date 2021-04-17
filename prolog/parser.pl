/* string_lower/2 , string_upper/2 
 * current_predicate/1
 * atom_string/2 , string_chars/2
 * split_string/4
 * char_code/2, between/3
 * string_codes/2
 * atom_prefix/2, atom_concat/3, atom_length
 * downcase_atom/2, upcase_atom/2
 * sub_atom_icasechk/3
 */

main(X) :- 
    split_string(X," \n\t\r"," \n\t\r",L),
    string_to_atoms(L,[],R),
    my_parse(R),
    write(R), !.

% reckognized \

describer --> adjective, describer.
describer --> adjective.

name --> [N], { nameCheck(N) }.
name --> [N], { nameCheck(N) }, name.

noun --> [N], { member(N,['book','music','film','project','class','gamer','aternoon']) }. 
noun --> [N], { atom_concat(X,s,N), member(X,['book','music','film','project','class','gamer','aternoon']) }.

%%% pronouns \
tpPronouns --> [N], { member(N,[he,she,it]) }.
tpPronouns --> [N], { nameCheck(N), downcase_atom(N,T), member(T,[he,she,it]) }.

remPronouns --> [N], { member(N,[i,you,we,they]) }.
remPronouns --> [N], { nameCheck(N), downcase_atom(N,T), member(T,[i,you,we,they]) }.
%%% pronouns /

list_stuff --> noun, [and], list_stuff.
list_stuff --> noun, [,], list_stuff.
list_stuff --> noun.

verb --> [V], { member(V,[go,present,like,eat,work,play,went,presented,liked,ate,worked,played]) }.
tpVerb --> [V], { member(V,[goes,presents,likes,eats,works,plays]) }.

preposition --> [P], { member(P,[in,for,at,with,from,to,on,near]) }.

adjective --> [A], { member(A,[great,new,yellow]) }.

determiner --> [D], { member(D,[the,a,an,this,these,that]) }. 

adverb --> [A], { member(A,[today,tomorrow]) }.
% reckognized /


np --> remPronouns.
np --> noun. % this is exceptional
np --> name.
np --> name, determiner, noun. % this is exceptional
np --> determiner, noun.
np --> determiner, describer, np.
np --> describer, np. 
np --> np,[and],np.

pp --> preposition, np.

vp --> verb.
vp --> verb, np,ap.
vp --> verb, np, pp.
vp --> verb, np.
vp --> verb, pp.
vp --> verb, pp.
vp --> vp,listing.

ap --> adverb.
ap --> describer, adverb.

timed --> [will], vp.

claus --> np,timed.

sentence --> claus.
sentence --> claus, [,], claus.

my_parse(E) :- sentence(E,[]). 

% Checks an atom form is like a name: First letter uppercased, remaining lowercased.
nameCheck(NAME) :- 
    atom_concat(PFX,SFX,NAME),
    atom_length(PFX,1),
    upcase_atom(PFX,UCASEPFX),
    downcase_atom(SFX,DCASESFX),
    atom_concat('',SFX,DCASESFX),
    atom_concat('',PFX,UCASEPFX),!.
    

check_range([],_,_).
check_range([X|XS],U,B) :-
    between(U,B,X),
    check_range(XS,U,B).
    
% Turns a list of strings into a list of atoms.
string_to_atoms([X|XS],Y,Q) :-
    %string_lower(X,I), no hizo falta porque el atomo de "PRUEBA" seria 'PRUEBA' 
    atom_string(P,X),
    append(Y,[P],R),
    string_to_atoms(XS,R,Q).
string_to_atoms([],Y,Y) .
