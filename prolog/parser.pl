main(X) :- 
    split_string(X," \n\t\r"," \n\t\r",L),
    string_to_atoms(L,[],R),
    my_parse(AST,R),
    write(AST), !.

% pasando
% Alex watches films
% he likes books , movies and films
% # Carlos and Alex made a great project for class


my_parse(AST,L) :- adverb(AST,L,[]).

% Non terminals

sentence    --> claus.
sentence    --> claus, [,], claus.

claus       --> tpNp, tpVp.
claus       --> remNp,timed.
claus       --> remNp.

timed       --> vp.          
timed       --> [will], vp.

vp          --> verb.
vp          --> verb, conjunctor.
vp          --> verb, pp.
vp          --> verb, both.
%vp          --> verb, both, ap.
%vp          --> verb, both, pp.

tpVp        --> tpVerb.
tpVp        --> tpVerb, both.
tpVp        --> tpVerb, conjunctor.
tpVp        --> tpVerb, pp.
%tpVp        --> tpVerb, both, ap.
%tpVp        --> tpVerb, both, pp.

both        --> remPronouns.
both        --> tpPronouns.
both        --> commonNp.

remNp       --> remPronouns.
remNp       --> commonNp.

tpNp        --> tpPronouns.
tpNp        --> commonNp.

commonNp    --> conjunctor. % this is exceptional
commonNp    --> name.
commonNp    --> name, determiner, noun. % this is exceptional
commonNp    --> determiner, noun.
commonNp    --> determiner, describer, commonNp.
%commonNp    --> describer, commonNp. 

pp          --> preposition, both.

ap          --> describer.
ap          --> adverb, describer.

describer   --> adjective, describer.
describer   --> adjective.

conjunctor  --> nominal, [and], conjunctor.
conjunctor  --> nominal, [,], conjunctor.
conjunctor  --> nominal.

nominal([nominal, Nom])     --> noun(Nom),!.
nominal([nominal, Nom])     --> name(Nom),!.

% Terminals
name([name,Name])                   --> [Name], { downcase_atom(Name,T), not(member(T,[i,she,he,you,we,they])),nameCheck(Name), ! }.
%name([[name,Name],[name,X]])        --> [Name], { downcase_atom(Name,T), not(member(T,[i,she,he,you,we,they])),nameCheck(Name), ! }, name(X).

noun([noun,Noun])        --> [Noun], { member(Noun,[movie,book,music,film,project,class,gamer,afternoon]),! }. 
noun([noun,Noun])        --> [Noun], { atom_concat(X,s,Noun), member(X,[movie,book,music,film,project,class,gamer,afternoon]),!}.

tpPronouns([tpPronoun,PN])  --> [PN], { member(PN,[he,she,it]) , !}.
tpPronouns([tpPronoun,PN])  --> [PN], { nameCheck(PN), downcase_atom(PN,T), member(T,[he,she,it]) , !}.

remPronouns([remPronoun,Rpn]) --> [Rpn], { member(Rpn,[i,you,we,they]) , !}.
remPronouns([remPronoun,Rpn]) --> [Rpn], { nameCheck(Rpn), downcase_atom(Rpn,T), member(T,[i,you,we,they]), !}.

verb([verb,V])       --> [V], { member(V,[go,present,like,eat,work,make,play,watch,went,presented,liked,ate,worked,made,played,watched]) , !}.

tpVerb([tpVerb, V])      --> [V], { member(V,[goes,presents,likes,eats,works,plays,watches]) , !}.

preposition([preposition,Pre]) --> [Pre], { member(Pre,[in,for,at,with,from,to,on,near]) , !}.

adjective([adjective,Adj])   --> [Adj], { member(Adj,[great,new,yellow]) }.

determiner([determiner, Det])  --> [Det], { member(Det,[the,a,an,this,these,that]) , !}. 

adverb([adverb, Adv])   --> [Adv], { member(Adv,[today,tomorrow]), ! }.  

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

/*
 * Zack Snyder and Jared will present the new Justice League film today in HBO
 * Alexs watches films.
 * He likes books , music and films
 * Carlos and Alex made a great project for class
 * We will eat Bamboo Postres tomorrow , we eat at Comedores MYS today
 * Carlos works with Alex while Daniel the gamer plays in the afternoon
 * Carlos and Alex work while they play in the afternoon
 *
 */
