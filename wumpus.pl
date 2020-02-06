% Student Name: Prafull Kumar Katiyar
% Student Id  : 977224
% Department  : MS CS
% Subjeect    : Declarative Programming COMP90048

:-module(wumpus,[initialState/5, guess/3, updateState/4]).

% Graph technique used for the project
% In the graph every vertex has a label:
% -1 indicates an unkwown position
%  0 is empty
%  1 is either a wall
%  2 is a pit
%  3 is the a place that smells like wumpus
% -2 not a wumpus, but unknown

% we create a empty graph with the current position set to 0 (empty)
initialState(NR, NC, XS, YS, state(search,G,(XS,YS),(XS,YS))):-
	createGraph(NR,NC,G0),
	setVertex(G0,(XS,YS),0,G).

% keep searching, untill the map is explored
guess(state(Search,G,Pos,Pos0), state(Search,G,Pos,Pos0), Orders):-
	member(Search,[search,search2]),
	getUnkwown(G,UnkwownPos), % get an unkown position
	dfs(Pos,UnkwownPos,G,Orders0),
	truncate(G,Pos,Orders0,Orders),
	!.

% all the map has been explored... now kill!
guess(state(Search,g(V,E),Pos,Pos0),
      state(kill,g(V,E),Pos,Pos0,All),
      [shoot]):-
	member(Search,[search,search2]),
	getAllEmpty(V,Empty),
	getAllEmptyAdj(g(V,E),Empty,All),!.

% we are on kill mode!
guess(state(kill,G,P0,P1,[(Pos,[D1|MoreDirs])|More]),
      state(kill,G,P0,P1,[(Pos,MoreDirs)|More]),
      Orders):-
	dfs(P0,Pos,G,Orders0),
	append(Orders0,[D1,shoot],Orders).

% Keep shooting
guess(state(kill,G,P0,P1,[(_,[])|More]),
      state(kill,G,P0,P1,More),
      [shoot]):-!.

% end of feedback
updateState(state(Search,G,_,Pos0), _, [], state(Search,G,Pos0,Pos0)):-!. % graph(G),!. % graph(G).

updateState(S, _, [], S):-!.

updateState(S0, [Order|Orders], [Feedback|Feedbacks], S1):-
	update(S0, Order, Feedback, STmp),
	updateState(STmp, Orders, Feedbacks, S1).

% update predicates for updating one feedback - pit
update(state(Search,G,P,P0),
       Order,
       pit,
       state(Search,GNew,P1,P0)):-
	member(Search, [search,search2]),
	move(P,Order,P1),
	setVertex(G,P1,2,GNew), % update the graph with a pit
	!.

% update predicates for updating one feedback - wall
update(state(Search,G,P,P0),
       Order,
       wall,
       state(Search,GNew,P1,P0)):-
	member(Search, [search,search2]),
	move(P,Order,P1),
	setVertex(G,P1,1,GNew), % update the graph with a wall
	!.


update(state(Search,G,P,P0),
       Order,
       smell,
       state(Search,GNew2,P1,P0)):-
	member(Search, [search,search2]),
	move(P,Order,P1),
	setVertex(G,P1,0,GNew), % update the graph with empty,
	g(Vertices,_)=GNew,
	move(P1,north,N),find(N,Vertices,NL),
	move(N,north,NN),find(NN,Vertices,NNL),
	move(N,east,NE),find(NE,Vertices,NEL),
	move(N,west,NW),find(NW,Vertices,NWL),
	move(P1,east,E),find(E,Vertices,EL),
	move(E,east,EE),find(EE,Vertices,EEL),
	move(P1,south,S),find(S,Vertices,SL),
	move(S,south,SS),find(SS,Vertices,SSL),
	move(S,east,SE),find(SE,Vertices,SEL),
	move(S,west,SW),find(SW,Vertices,SWL),
	move(P1,west,W),find(W,Vertices,WL),
	move(W,west,WW),find(WW,Vertices,WWL),
	appendall([NL,NNL,NEL,NWL,EL,EEL,SL,SSL,SEL,SWL,WL,WWL], AllDirs),
	filterValid(AllDirs,Vertices, AllValidDirs),
	updateVertexLabel(GNew, AllValidDirs, -2, GNew2),
	!.

% update the graph with an empty position
update(state(search,G,P,P0),
       Order,
       stench,
       state(search2,GNew2,P1,P0)):-
	move(P,Order,P1),
	getAdj(G,P1,Adj,[-1,-2]),
	updateVertexLabel(G,Adj,3,GNew),
	setVertex(GNew,P1,0,GNew2), 
	!.

% update the graph with a empty
update(state(search2,G,P,P0),
       Order,
       stench,
       state(search2,GNew,P1,P0)):-
	move(P,Order,P1),
	setVertex(G,P1,0,GNew), 
	!.

% update the graph with a empty space
update(state(Search,G,P,P0),
       Order,
       _,
       state(Search,GNew,P1,P0)):-
	member(Search, [search,search2]),
	move(P,Order,P1),
	setVertex(G,P1,0,GNew), 
	!.

update(state(kill,G,P1,P2,L),_,_,state(kill,G,P1,P2,L)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Functions


% createList(1,H,X), creates graph Y Axis coordinate ListY
% createList(1,H,X), creates graph X Axis coordinate ListX
% product - initial graph
% createEdges - all edges for all available vertices
createGraph(H,W,g(V,E)):-
	createList(1,H,X),
	createList(1,W,Y),
	product(X,Y,-1,V), 
	createEdges(V,V,E). 
						
% createList(Start,End,List) creates a list from Start to End in List
createList(X,X,[X]):-!.
createList(A,B,[A|T]):-
	A1 is A+1,
	createList(A1,B,T).

% product(L1,L2,Value,P): creates the cartesian product between all

product([], _, _, []).
product([H1|T1],L2,V,P):-
	rowProduct(H1, L2, V, PRow),
	product(T1,L2,V,Pr),
	append(PRow,Pr,P).

% Input 1 element from ListX and whole ListY returns tuple

rowProduct(_, [], _, []):-!.
rowProduct(X, [H|T], V, [(X,H,V)|P]):-
	rowProduct(X,T,V,P).

% createEdges(V,E):
% creates a list of adjacent vertices (edges) given the list of vertices
createEdges([],_,[]).
createEdges([(X,Y,_)|OtherVertices],Vertices, [(X,Y,L3)|OtherEdges]):-
	YUp is Y-1,
	YDown is Y+1,
	XLeft is X-1,
	XRight is X+1,
	find((X,YUp,_),Vertices,Up0), addToTuple(Up0, north, Up),
	find((X,YDown,_), Vertices, Down0), addToTuple(Down0, south, Down),
	find((XLeft,Y,_), Vertices, Left0), addToTuple(Left0, west, Left),
	find((XRight,Y,_), Vertices, Right0), addToTuple(Right0, east, Right),
	append(Up,Down,L1),
	append(L1,Left,L2),
	append(L2,Right,L3),
	createEdges(OtherVertices, Vertices, OtherEdges),!.

% Provided list of vertices of graph, and a vertex, returns if vertex exist
find(_,[],[]).
find(H,[H|_],[H]):-!.
find((X,Y),[(X,Y,_)|_],[(X,Y)]):-!.
find(X, [_|T], R):-
	find(X,T,R).

% adds a new value to a tuple
addToTuple([], _, []).
addToTuple([(X,Y,_)], V, [(X,Y,V)]).

% sets a vertex in the graph to a given label
setVertex(g(V,E),(X,Y),Value,g(NewV,E)):-
	replace(V, (X,Y,_), (X,Y,Value), NewV).

% replace(L,X,Y,L2): replaces value X in L with Y
replace([],_,_,[]).
replace([H|T], H, NewH, [NewH|T]):-!.
replace([H|T], X, NewH, [H|NewT]):-
	replace(T,X,NewH,NewT).

% runs a DFS in the graph to find a path from X,Y to the desired vertex
dfs(Pos,Dst,G,Path):-
	dfs(Pos,Dst,G,[Pos],Prev),
	buildPath(Dst,Prev,PathRev),
	reverse(PathRev, Path).

dfs((X,Y),(X,Y), _, _, []):-!. % write('Arrived!'),writeln((X,Y)),!.

dfs((X,Y), Dst, g(V,E), Visited, Prev):-
	member((X,Y,Adj),E), % get adjacency list
	member((X,Y,Label),V),
	member(Label,[-2,-1,0]),  % walk only on empty and unkwown positions
	visitAdjDFS((X,Y),Adj,Dst,g(V,E),Visited,Prev),
	!.

% explores all positions adjacent to the given position in the given
visitAdjDFS(Pos0,[(X,Y,Dir)|_], Dst, G, Visited, [((X,Y),Pos0,Dir)|Prev]):-
	not(member((X,Y),Visited)),
	dfs((X,Y),Dst,G,[(X,Y)|Visited],Prev),
	!.
visitAdjDFS(Pos0,[_|More], Dst, G, Visited, Prev):-
	visitAdjDFS(Pos0, More, Dst, G, Visited, Prev).

buildPath((X,Y), Prev, [PrevDir|Path0]):-
	member(((X,Y),PrevPos,PrevDir), Prev),
	buildPath(PrevPos, Prev, Path0),!.

buildPath(_, _,  []).

% returns an unkwown position in the map which is accesible (next to a
% empty position)
getUnkwown(g(V,E),(X,Y)):-
	member((X,Y,Label),V),    % we want an unkwown position
	member(Label,[-1,-2]),
	member((X0,Y0,0),V),   % and an empty position
	member((X0,Y0,Adj),E), % and we need to move from empty to unkwown
	member((X,Y,_),Adj),
	!.

% returns the adjacent positions to a given position (which are valid)
getAdj(g(V,E), (X,Y), Adj, Valid):-
	member((X,Y,Adj0),E),
	filterValid0(Adj0, V, Adj, Valid).

% filters only valid positions
filterValid([],_,[]).
filterValid([(X,Y,D)|MoreU],V,[(X,Y,D)|Results]):-
	member((X,Y,Status),V),
	member(Status,[-1,-2]),   % keep only unkwown
	filterValid(MoreU,V,Results),!.
filterValid([(X,Y)|MoreU],V,[(X,Y)|Results]):-
	member((X,Y,Status),V),
	member(Status,[-1,-2]),   % keep only unkwown
	filterValid(MoreU,V,Results),!.
filterValid([_|MoreU],V,Result):-
	filterValid(MoreU,V,Result).

filterValid0([],_,[],_).
filterValid0([(X,Y,D)|MoreU],V,[(X,Y,D)|Results],Valid):-
	member((X,Y,Status),V),
	member(Status,Valid),   % keep only unkwown
	filterValid0(MoreU,V,Results,Valid),!.
filterValid0([(X,Y)|MoreU],V,[(X,Y)|Results],Valid):-
	member((X,Y,Status),V),
	member(Status,Valid),   % keep only unkwown
	filterValid0(MoreU,V,Results,Valid),!.
filterValid0([_|MoreU],V,Result,Valid):-
	filterValid0(MoreU,V,Result,Valid).

% updateVertexLabel(G0,L,V,G1): updates the labels of all vertices in
% list L, in the graph G0 to a new label V, in the graph G1
updateVertexLabel(G, [], _, G).
updateVertexLabel(G,[(X,Y,_)|Other],NewLabel,GNew):-
	setVertex(G, (X,Y), NewLabel, GNew0),
	updateVertexLabel(GNew0, Other, NewLabel, GNew).
updateVertexLabel(G,[(X,Y)|Other],NewLabel,GNew):-
	setVertex(G, (X,Y), NewLabel, GNew0),
	updateVertexLabel(GNew0, Other, NewLabel, GNew).


% move(P,D,P0): returns in P0 the result of moving from P in direction D
move((X,Y,_),Dir,(X1,Y1,_)):-
	move((X,Y),Dir,(X1,Y1)).

move((X,Y),north,(X,YN)):-
	YN is Y - 1.
move((X,Y),south,(X,YN)):-
	YN is Y + 1.
move((X,Y),west,(XN,Y)):-
	XN is X - 1.
move((X,Y),east,(XN,Y)):-
	XN is X + 1.
move((X,Y),_,(X,Y)).


% graph(G) prints graph G
graph(G):-graph(1,1,G).

graph(X,Y,g(V,E)):-
	member((X,Y,Ex),V),
	getchar(Ex,C),
	write(C),
	X1 is X+1,
	graph(X1,Y,g(V,E)),!.
graph(_,Y,g(V,E)):-
	writeln(''),
	Y1 is Y+1,
	member((1,Y1,_),V),
	graph(1,Y1,g(V,E)),!.

graph(_,_,_):-writeln('****************').

getchar(-2,'%').
getchar(-1,'?').
getchar(0,'.').
getchar(1,'#').
getchar(2,'P').
getchar(3,'W').

% truncates the orders so it stops in an unkwown position
truncate(_, _, [], []).
truncate(g(V,_), (X,Y), _, []):-
	member((X,Y,Label),V),
	member(Label,[-1,-2]),
	!.
truncate(G, Pos, [Order|Orders], [Order|MoreOrders]):-
	move(Pos, Order, NewPos),
	truncate(G, NewPos, Orders, MoreOrders).

% appendall(LL, L): LL is a list of lists, L will be the result of
% appending all lists of LL
appendall([], []).
appendall([H|T], L):-
	appendall(T,L2),
	append(H,L2,L).

% filters all positions which are empty
getAllEmpty([], []).
getAllEmpty([(X,Y,0)|Other], [(X,Y)|More]):-
	getAllEmpty(Other,More),!.
getAllEmpty([_|Other], More):-
	getAllEmpty(Other,More).

% get all empty positions, adjacent to positions in the given list
getAllEmptyAdj(_, [], []).
getAllEmptyAdj(G, [Pos|More], [(Pos,AdjDir)|Other]):-
	getAdj(G, Pos, Adj, [0]), % get empty neighbors,
	removePos(Adj, AdjDir),
	getAllEmptyAdj(G, More, Other).

% removes the positions from a list of positions/directions so we get
% only the directions
removePos([], []).
removePos([(_,_,D)|Other], [D|Other2]):-
	removePos(Other, Other2).
