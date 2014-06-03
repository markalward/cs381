%-------------------------------------------------------------------------------------
%- Peter Rindal, Mark Alward, Brenn Kucey                                           --
%- CS381 - HW5: prolog                                                               --
%- 6/3/2014                                                                         --
%-------------------------------------------------------------------------------------
%- ex 1.a                                                                           --
%-------------------------------------------------------------------------------------

when(275,10).
when(261,12).
when(381,11).
when(398,12).
when(399,12).

where(275,owen102).
where(261,dear118).
where(381,cov216).
where(398,dear118).
where(399,cov216). 

enroll(mary,275).
enroll(john,275).
enroll(mary,261).
enroll(john,381).
enroll(jim,399).

schedule(S,P,T) :- enroll(S,C),where(C,P),when(C,T).

%-------------------------------------------------------------------
%- ex 1.b                                                         --
%-------------------------------------------------------------------
usage(P,T) :- where(C,P),when(C,T). 

%-------------------------------------------------------------------
%- ex 1.c                                                         --
%-------------------------------------------------------------------
conflict(Y,Z) :- where(Y,P),when(Y,T),where(Z,P),when(Z,T),Y\=Z.

%-------------------------------------------------------------------
%- ex 1.d                                                         --
%-------------------------------------------------------------------
meet(A,B) :- enroll(A,C),enroll(B,C),A\=B. 
meet(A,B) :- enroll(A,C),when(C,T),where(C,P),
             enroll(B,D),when(D,U),where(C,P),
             A\=B, T+1=:=U. 

%-------------------------------------------------------------------
%- ex 2.a                                                         --
%-------------------------------------------------------------------
rdup([],[]).
rdup([A|L],M) :- L = [A|_], rdup(L,M).
rdup([A|L],[A|M]) :- rdup(L,M).

%-------------------------------------------------------------------
%- ex 2.b                                                         --
%-------------------------------------------------------------------
flat([], []).
flat([A|L], F) :- flat(A, B) , flat(L, M) , append(B, M, F).
flat(L, [L]).

flat2([]    , L).
flat2([R|RS], L) :- flat2( R, C)     , flat2(RS, D), append(C,D,L).
flat2(R     , L) :- append([R], L, L).

%-------------------------------------------------------------------
%- ex 2.c                                                         --
%-------------------------------------------------------------------
project_(1, [A|_], L) :- append([A], L, L).
project_(N, [_|AS], L) :- project_(N-1, AS, L).

project([], _, L).
project([N|NS], A, L) :- project_(N, A, L) , project(NS, A, L).
