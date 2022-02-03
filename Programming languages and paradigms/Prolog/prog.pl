% RZETELSKI
:- ensure_loaded(library(lists)).

:- op(700, xfx, '<>').
L <> R :- L \= R.

verify :-
   current_prolog_flag(argv, Argv),
   length(Argv, 3),
   nth0(1, Argv, ArgN),
   nth0(2, Argv, File),
   atom_number(ArgN, N),
   verify(N, File).

verify :-
   format('Error: podane argumenty są niepoprawne~n').

% verify(+N, +File)
verify(N, File) :-
   integer(N),
   N > 0,
   set_prolog_flag(fileerrors, off),
   (  see(File) ->
      read(variables(VarIds)),
      read(arrays(ArrIds)),
      read(program(Stmts)),
      seen,
      Prog = program(VarIds, ArrIds, Stmts),
      initState(Prog, N, State),
      traverse(Prog, State, Res),
      output(Res)
   ;  errFile(File)
   ).

verify(N, _) :- errParameter(N).

% errParameter(+N)
errParameter(N) :-
   format('Error: parametr ~w powinien byc liczba > 0~n', N).

% errFile(+File)
errFile(File) :-
   format('Error: brak pliku o nazwie - ~w~n', File).

% initState(+Prog, +N, -InitState)
% Program representation: program(VarIdents, ArrIdents, Stmts)
% State representation: state(Vars, Arrs, PCs), where
% Vars = Map Ident Val
% Arrs = Map Ident [Val]
% PCs (Program Counters) = [Number of currently executed statement]
% E.g. at index 0, PCs contains the number of statement currently
% being executed by the process with PrId equal to 0.
initState(program(VarIds, ArrIds, _), N, state(Vars, Arrs, PCs)) :-
   newMap(VarIds, 0, Vars),
   newArr(N, 0, Arr),
   newMap(ArrIds, Arr, Arrs),
   newArr(N, 1, PCs).

% newMap(+M, +InitV, -M1)
newMap([], _, []).
newMap([K|Ks], InitV, [K-InitV|M]) :-
   newMap(Ks, InitV, M).

% mapAt(+M, +K, -V)
mapAt([K-V|_], K, V).
mapAt([_|M], K, V) :-
   mapAt(M, K, V).

% mapInsertAt(+M, +K, +V, -M1)
mapInsertAt([K-_|T], K, V, [K-V|T]).
mapInsertAt([H|T], K, V, [H|T1]) :-
   mapInsertAt(T, K, V, T1).

% newArr(+Size, +InitV, -Arr)
newArr(0, _, []).
newArr(Size, InitV, [InitV|T]) :-
   Size > 0,
   Size1 is Size - 1,
   newArr(Size1, InitV, T).

% arrayInsertAt(+Arr, +I, +V, -Arr1)
arrayInsertAt([_|T], 0, V, [V|T]).
arrayInsertAt([H|T], I, V, [H|T1]) :-
   I > 0,
   I1 is I - 1,
   arrayInsertAt(T, I1, V, T1).

% step(+Prog, +InState, ?PrId, -OutState)
% Get the next state from the current state by evaluating the 
% instruction that process with PrId is currently pointing at.
step(program(_, _, Stmts), InState, PrId, OutState) :-
   getPC(InState, PrId, PC),
   nth1(PC, Stmts, Stmt),
   evalStmt(Stmt, InState, PrId, OutState).

% evalStmt(+Stmt, +InState, +PrId, -OutState)
evalStmt(assign(Var, ArExpr), InState, PrId, OutState) :-
   evalArExpr(ArExpr, InState, PrId, Val),
   setVar(InState, Var, Val, PrId, MidState),
   advance(MidState, PrId, OutState).

evalStmt(sekcja, InState, PrId, OutState) :-
   advance(InState, PrId, OutState).

evalStmt(goto(Index), InState, PrId, OutState) :-
   setPC(InState, PrId, Index, OutState).

evalStmt(condGoto(BExpr, Index), InState, PrId, OutState) :-
   evalBExpr(BExpr, InState, PrId) ->
      setPC(InState, PrId, Index, OutState)
   ;  advance(InState, PrId, OutState).

% evalExpr(+Expr, +State, +PrId, -Val)
evalExpr(pid, _, PrId, PrId).

evalExpr(Num, _, _, Num) :-
   number(Num).

evalExpr(Var, State, PrId, Val) :-
   evalVar(Var, State, PrId, Val).

evalVar(Ident, State, _, Val) :-
   getVar(State, Ident, Val).

evalVar(array(Ident, ArExpr), State, PrId, Val) :-
   getArr(State, Ident, Arr),
   evalArExpr(ArExpr, State, PrId, Index),
   nth0(Index, Arr, Val).

% evalArExpr(+Expr, +State, +PrId, -Val)
evalArExpr(Expr, State, PrId, Val) :-
   evalExpr(Expr, State, PrId, Val).

evalArExpr(ArExpr, State, PrId, Val) :-
   ArExpr =.. [Op, ExprL, ExprR],
   evalExpr(ExprL, State, PrId, L),
   evalExpr(ExprR, State, PrId, R),
   Eval =.. [Op, L, R],
   Val is Eval.

% evalBExpr(+BExpr, +State, +PrId)
evalBExpr(BExpr, State, PrId) :-
   BExpr =.. [Op, ExprL, ExprR],
   evalExpr(ExprL, State, PrId, L),
   evalExpr(ExprR, State, PrId, R),
   call(Op, L, R).

% advance(+InState, +PrId, -OutState)
% Advance the Program Counter of a given PrId by one. 
advance(InState, PrId, OutState) :-
   getPC(InState, PrId, PC),
   UpPC is PC + 1,
   setPC(InState, PrId, UpPC, OutState).

% getPC(+State, +PrId, -PC)
% Get the Program Counter of the given PrId.
getPC(state(_, _, PCs), PrId, PC) :-
   nth0(PrId, PCs, PC).

% setPC(+State, +PrId, +PC, -OutState)
% Set the Program Counter of the given PrId to PC.
setPC(state(Vars, Arrs, PCs), PrId, PC, state(Vars, Arrs, UpPCs)) :-
   arrayInsertAt(PCs, PrId, PC, UpPCs).

% getVar(+State, +Ident, -Val)
getVar(state(Vars, _, _), Ident, Val) :-
   mapAt(Vars, Ident, Val).

% setVar(+InState, +Ident, +Val, +PrId, -OutState)
setVar(state(Vars, Arrs, PCs), Ident, Val, _, state(UpVars, Arrs, PCs)) :-
   mapInsertAt(Vars, Ident, Val, UpVars).

setVar(InState, array(Ident, ArExpr), Val, PrId, OutState) :-
   getArr(InState, Ident, Arr),
   evalArExpr(ArExpr, InState, PrId, Index),
   arrayInsertAt(Arr, Index, Val, UpArr),
   setArr(InState, Ident, UpArr, OutState).

% getArr(+State, +Ident, -Arr)
getArr(state(_, Arrs, _), Ident, Arr) :-
   mapAt(Arrs, Ident, Arr).

% setArr(+InState, +Ident, +Arr, -OutState)
setArr(state(Vars, Arrs, PCs), Ident, Arr, state(Vars, UpArrs, PCs)) :-
   mapInsertAt(Arrs, Ident, Arr, UpArrs).

% traverse/3(+Prog, +State, -Res)
% Traverse the execution graph of the Program (Prog) starting at State.
% Res describes whether the program is correct (safe).
traverse(Prog, State, Res) :-
   traverse(Prog, State, [], _, Res).

% traverse/5(+Prog, +State +InVisited, -OutVisited, -Res)
% InVisited - a list of visited states before invoking the predicate.
% InVisited - a list of visited states after invoking the predicate.
traverse(Prog, State, InVisited, OutVisited, Res) :-
   (  member(State, InVisited) ->
      OutVisited = InVisited,
      Res = correct
   ;  (  unsafe(Prog, State, Procs) ->
         Res = incorrect([], Procs)
      ;  getNextStates(Prog, State, L),
         reduceStates(Prog, State, L, [State|InVisited], OutVisited, Res)
      )
   ).

% reduceStates(+Prog, +State, +NextStates, +InVisited, ?OutVisited, -Res)
% Invoke all the neighbouring states (NextStates) of given state (State)
% and check whether all are correct (safe).
reduceStates(_, _, [], Visited, Visited, correct).
reduceStates(Prog, State, [PrId-NextState|T], InVisited, OutVisited, Res) :-
   traverse(Prog, NextState, InVisited, MidVisited, MidRes),
   (  MidRes = incorrect(Path, Procs) ->
      getPC(State, PrId, PC),
      Res = incorrect([PrId-PC|Path], Procs)
   ; reduceStates(Prog, State, T, MidVisited, OutVisited, Res)
   ).

% getNextStates(+Prog, +State, -L)
% Get all neighbouring states of a given state (State).
getNextStates(Prog, State, L) :-
   getNextStates(Prog, State, [], L).

getNextStates(Prog, State, Acc, L) :-
   step(Prog, State, PrId, X), % make a step for every PrId
   \+ member(PrId-_, Acc),
   !,
   getNextStates(Prog, State, [PrId-X|Acc], L).
getNextStates(_, _, L, L).

% unsafe(+Program, +State, -Procs) 
% Check whether more than one process is in critical section in
% the given state (State).
% Procs - list of processes in critical section (sekcja).
unsafe(program(_, _, Stmts), state(_, _, PCs), Procs) :-
   elementsAtIndices(Stmts, PCs, CurrentStmts),
   findOccurances(sekcja, CurrentStmts, Procs),
   length(Procs, Count),
   Count > 1.

% elementsAtIndices(+List, +Indices, -Elements)
elementsAtIndices(_, [], []).
elementsAtIndices(List, [I|Indices], [E|Elements]) :-
   nth1(I, List, E),
   elementsAtIndices(List, Indices, Elements).

% findOccurances(+E, +L, -O)
findOccurances(E, L, O) :-
   findOccurances(E, L, 0, O).

findOccurances(_, [], _, []).
findOccurances(E, [E|T], I, [I|Os]) :-
   I1 is I + 1,
   findOccurances(E, T, I1, Os).

findOccurances(E, [_|T], I, O) :-
   I1 is I + 1,
   findOccurances(E, T, I1, O).

% output(+Res)
output(correct) :-
   format('Program jest poprawny (bezpieczny).~n').

output(incorrect(Path, Procs)) :-
   format('Program jest niepoprawny.~nNiepoprawny przeplot:~n'),
   outputPath(Path),
   format('Procesy w sekcji:'),
   outputProcs(Procs).

% outputPath(+Path)
outputPath([]).
outputPath([PrId-PC|T]) :-
   format('    Proces ~d: ~d~n', [PrId, PC]),
   outputPath(T).

% outputProcs(+Procs)
outputProcs([P]) :-
   format(' ~d.~n', P).
outputProcs([P|Ps]) :-
   format(' ~d,', P),
   outputProcs(Ps).
