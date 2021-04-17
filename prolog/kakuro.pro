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

% clues positions as cells
cluePositions([], []).
cluePositions([clue(Col, Row, _, _) | CluesTail], [blank(Col, Row) | CellsTail ]) :- cluePositions(CluesTail, CellsTail). 

% frecuencia de un elemento en una lista
frec(_, [], 0).
frec(X, [X | T], N) :- frec(X, T, M), N is M + 1.
frec(X, [_| T], N)  :- frec(X, T, N).

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
noSym([pair(X,Y) | Ps], [pair(X,Y) | NoSymPs]) :- noSym(Ps, NoSymPs).

%   Revisa si todos los apres de clues en una lista comparten a lo sumo una celda en sus blanks
almostDisjointBlanks([]).
almostDisjointBlanks([ pair(clue(_, _, _, B1), clue(_,_,_, B2)) | Ps]) :-  intersection(B1, B2, Inter),
                                                                            length(Inter, N),
                                                                            N =< 1,
                                                                            almostDisjointBlanks(Ps).
                                                                            


%--- Funciones principales de chequeo ---%

% Indica que cada celda tenga máximo 2 clues
noMoreThanTwoCluesSameCell([]).
noMoreThanTwoCluesSameCell(Clues) :- cluePositions(Clues, Positions), checkFrec(Positions, Positions).

% Indica que no existen clues repetidas en los blanks de otras clues
noClueInSomeBlanks(Clues) :- blanks(Clues, Blanks), cluePositions(Clues, Cells), intersection(Cells, Blanks, []).

% Indica si los blanks de una clue estan en linea recta respecto a la clue 
blanksAligned([]).
blanksAligned([Clue | MoreClues]) :- (blanksInCol(Clue); blanksInRow(Clue)), blanksAligned(MoreClues).

% Indica si para cualesquiera dos pares de clues, comparten a lo sumo una celda en sus blanks
notOverlappingBlanks(Clues) :-  cross(Clues, Clues, Cross), 
                                noSym(Cross, NoSym),
                                write("todo bien"), nl,
                                almostDisjointBlanks(NoSym).

% Revisa si un kakuro es valido y la solucion dada resuelve el kakuro
valid(kakuro(Clues), Solution) :-   noMoreThanTwoCluesSameCell(Clues), 
                                    noClueInSomeBlanks(Clues), 
                                    blanksAligned(Clues),
                                    notOverlappingBlanks(Clues).



% read some kakuro
openKakuro(Kakuro, FileToOpen) :-   open(FileToOpen, read, Strm),
read(Strm, Kakuro).

readKakuro(Kakuro) :-   write("Dame un archivo para leer: "),
nl,
read(File),
openKakuro(Kakuro, File).
