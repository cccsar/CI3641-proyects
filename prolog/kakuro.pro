/*
    Descripciones:
        blank(Col,Row)

        clue(Col, Row, Sum, Blanks)

        kakuro(lista de clues)

        fill(Blank, val)

        Custom structures:
            + cell(i,j)
        Predicados auxiliares
            + cluePositions(clues, cells): Posiciones de una lista de clues representados en forma de cells
            + frec(element, list): frecuencia de un elemento en una lista
            + checkFrec(L1, L2)> revisa que los elementos de L1  no aparezcan más de 2 veces en L2
*/

% Check if all elements are distinct
daff(X) :- 
    list_to_set(X,S),
    length(X,N),
    length(S,M),
    M =:= N.


% clues positions as cells
cluePositions([], []).
cluePositions([clue(Col, Row, _, _) | CluesTail], [blank(Col, Row) | CellsTail ]) :- cluePositions(CluesTail, CellsTail). 

% frecuencia de un elemento en una lista
frec(_, [], 0).
frec(X, [X | T], N) :- frec(X, T, M), N is M + 1.
frec(X, [Y| T], N)  :- frec(X, T, N), X\=Y.

% Revisa que no hayan duplicados
noDuplicates([]).
noDuplicates([X | Xs]) :- frec(X, Xs, 0), noDuplicates(Xs).

% Indica los elementos de la primera lista aparecen menos de dos veces en la lista L
checkFrec([], _).
checkFrec(_, []).
checkFrec([X | T], L) :- frec(X, L, N), N =< 2, checkFrec(T, L).

% obtiene los blanks de una lista de clues
blanks([], []).
blanks([clue(_,_,_, Blanks) | MoreClues], AllBlanks) :- blanks(MoreClues, SomeBlanks), append(Blanks, SomeBlanks, AllBlanks). 

% Obtiene las filas de una lista de blank
rows([], []).
rows([blank(_, Row) | MoreBlanks], [Row | MoreRows]) :- rows(MoreBlanks, MoreRows).

% Obtiene las columnas de una lista de blank
cols([], []).
cols([blank(Col, _) | MoreBlanks], [Col | MoreCols]) :- cols(MoreBlanks, MoreCols).

% Revisa si todos los numeros de una lista estan en un rango
allInRange([], _, _).
allInRange([X | L], Lower, Upper) :- Lower =< X, X =< Upper, allInRange(L,Lower, Upper).

% get sum of numbers in range [l, r]
sumInRange(L, R, Sum) :- S1 is R * (R + 1) / 2, S2 is (L - 1) * (L) / 2, Sum is S1 - S2.

% Dice si todos los elementos de una lista son el mismo elemento
allEq([], _).
allEq([X | T], X) :- allEq(T, X).

% Obtiene el producto cartesiano de dos listas ignorando los reflexivos
blanksInRow(clue(Col,Row,_, Blanks)) :- rows(Blanks, Rows),             % get rows
                                        cols(Blanks, Cols),             % Get cols
                                        write("Everything ok"), nl,
                                        allEq(Rows, Row),      % check that they're all in the same row
                                        length(Blanks, NBlanks),        % get length
                                        Lower is Col + 1,               % get lower bound
                                        Upper is Col + NBlanks,         % get upper bound
                                        allInRange(Cols, Lower, Upper), % check that all elements are in range
                                        sumInRange(Lower, Upper, Sum),  % Calcula suma entre inferior y superior
                                        sum_list(Cols, Sum).            % Revisa que todos los elementos sean distintos
 
blanksInCol(clue(Col,Row,_, Blanks)) :- rows(Blanks, Rows),             % get rows
                                        cols(Blanks, Cols),             % Get cols
                                        allEq(Cols, Col),               % check that they're all in the same Col
                                        length(Blanks, NBlanks),        % get length
                                        Lower is Row + 1,               % get lower bound
                                        Upper is Row + NBlanks,         % get upper bound
                                        allInRange(Rows, Lower, Upper), % check that all elements are in range
                                        sumInRange(Lower, Upper, Sum),  % Calcula suma entre inferior y superior
                                        sum_list(Rows, Sum).            % Revisa que todos los elementos sean distintos

% El producto cruz entre dos listas 

%   crea pares tal que el primer elemento es X, y el segundo pertenece a la lista
mkPairs(_, [], []). 
mkPairs(X, [Y | YS], [pair(X,Y) | MorePairs]) :- mkPairs(X, YS, MorePairs). 

%   calcula producto cartesiano
cross([], _, []).
cross(_, [], []).
cross([X | XS], YS, All) :- mkPairs(X, YS, WX), cross(XS, YS, WT), append(WX, WT, All).

%   elimina los simetricos
noSym([], []).
noSym([pair(X,X) | Ps], NoSymPs) :- noSym(Ps, NoSymPs).
noSym([pair(X,Y) | Ps], [pair(X,Y) | NoSymPs]) :- 
    noSym(Ps, NoSymPs).

%   Revisa si todos los apres de clues en una lista comparten a lo sumo una celda en sus blanks
almostDisjointBlanksHelper(_, []).
almostDisjointBlanksHelper(clue(X,Y,V, B1), [clue(_,_,_, B2) |Clues]) :- intersection(B1, B2, Inter), 
                                                                          length(Inter, N),
                                                                          N =< 1,
                                                                          almostDisjointBlanksHelper(clue(X,Y,V, B1), Clues).


almostDisjointBlanks([]).
almostDisjointBlanks([ Clue | Clues ]) :-   almostDisjointBlanksHelper(Clue, Clues),
                                            almostDisjointBlanks(Clues).

% Revisamos que los fill esten bien escritos
checkFillValues([]).
checkFillValues([fill(blank(_,_), X ) | Fs]) :- member(X, [1,2,3,4,5,6,7,8,9]), checkFillValues(Fs).

% Obtiene los blanks de una solucion
getSolBlanks([], []).
getSolBlanks([fill(B, _) | Fs], [B | MoreBlanks]) :- getSolBlanks(Fs, MoreBlanks).


% revisa que los blanks de una solucion sean todos distintos
fillDiffToAll(_, []).
fillDiffToAll(fill(blank(A,B), V), [ fill(blank(C,D), _) |Fs]) :- write("aaa"),nl, (A \= C ; B \= D), fillDiffToAll(fill(blank(A,B), V), Fs).

checkSolDifferentBlanks([]).
checkSolDifferentBlanks(Fills) :- getSolBlanks(Fills, Blanks), daff(Blanks). 

% revisa que no hayan pares con el mismo numero en la misma blank
checkSolPairNotInSameClue(_, []).
checkSolPairNotInSameClue(pair( fill(Blank1, Val1), fill(Blank2, Val2)), [clue(_,_,_, Blanks) | Clues]) :-
    ( 
        
        not(member(Blank1, Blanks)) ;
        not(member(Blank2, Blanks)) ;
        Val1 \= Val2
    ),
    checkSolPairNotInSameClue(pair( fill(Blank1, Val1), fill(Blank2, Val2)), Clues).

% revisa que los pares de una lista de pares de fills esten todos bien respecto a una lista de clues
checkFillsPairsOk([P1 | Ps], Clues) :- checkSolPairNotInSameClue(P1, Clues), 
                                       checkFillsPairsOk(Ps, Clues).

% Revisa que una lista fills no tenga dos elementos con el mismo valor en la misma clue
checkFillsHelper(_, []).
checkFillsHelper(fill(blank(A,B), V), [fill(_, W) | Fs]) :- V \= W, checkFillsHelper(fill(blank(A,B), V), Fs).

checkFillsHelperClues(_,_,[]).
checkFillsHelperClues(Fill, Fills, [clue(_,_,_, Blanks), Clues]) :- blankJoin(Blanks, Fills, Joined), 
                                                                    checkFillsHelper(Fill, Joined),
                                                                    checkFillsHelperClues(Fill, Fills, Clues).

checkFills([], _).
checkFills(_, []).
checkFills([Fill | Fills], Clues) :- checkFillsHelperClues(Fill, Fills, Clues),
                                     checkFills(Fills, Clues).
    
    


% retorna los fills que pertenecen a la lista de blanks dada
blankJoin([], _, []).
blankJoin(_, [], []).
blankJoin(Blanks, [fill(Blank, V) | Fs], [fill(Blank, V) | Joined]) :- member(Blank, Blanks), blankJoin(Blanks, Fs, Joined).

% returna la suma de una lista de fill
fillSum([], 0).
fillSum([fill(_, V) | Fs], N) :- fillSum(Fs, M), N is M + V.

% indica si las fills de una solución satisfacen a las Clues de un kakuro

allIn([X | Xs], L) :- member(X, L), allIn(Xs, L).
allIn([], _).

solMatch([], _).
solMatch([clue(_,_,V, Blanks) | Clues], Fills) :-   blankJoin(Blanks, Fills, Joined), 
                                                    getSolBlanks(Fills, SolBlanks),
                                                    write("aaaa"), nl,
                                                    allIn(Blanks, SolBlanks),
                                                    fillSum(Joined, V), 
                                                    solMatch(Clues, Fills).

                 


%--- Funciones principales de chequeo ---%

% Indica que cada celda tenga máximo 2 clues
noMoreThanTwoCluesSameCell([]).
noMoreThanTwoCluesSameCell(Clues) :- cluePositions(Clues, Positions), checkFrec(Positions, Positions).

% Indica que no existen clues repetidas en los blanks de otras clues
noClueInSomeBlanks(Clues) :- blanks(Clues, Blanks), cluePositions(Clues, Cells), intersection(Cells, Blanks, []).

% Indica si los blanks de una clue estan en linea recta respecto a la clue 
blanksAligned([]).
blanksAligned([Clue | MoreClues]) :- (blanksInCol(Clue); blanksInRow(Clue)), blanksAligned(MoreClues).

solutionWorks(Clues, Solution) :-   
                                    blanks(Clues, Blanks),
                                    list_to_set(Blanks, UniqueBlanks),
                                    length(UniqueBlanks, N),
                                    length(Solution, N),
                                    checkFillValues(Solution),
                                    checkSolDifferentBlanks(Solution),
                                    checkFills(Solution, Clues),
                                    write(Solution), nl,
                                    solMatch(Clues, Solution)
                                    .

% Revisa si un kakuro es valido y la solucion dada resuelve el kakuro
valid(kakuro(Clues), Solution) :-   
    noMoreThanTwoCluesSameCell(Clues), 
                                    noClueInSomeBlanks(Clues), 
                                    blanksAligned(Clues),
                                    almostDisjointBlanks(Clues), 
                                    solutionWorks(Clues, Solution).

% read some kakuro
openKakuro(Kakuro, FileToOpen) :-   open(FileToOpen, read, Strm),
read(Strm, Kakuro).

readKakuro(Kakuro) :-   write("Dame un archivo para leer: "),
nl,
read(File),
openKakuro(Kakuro, File).
