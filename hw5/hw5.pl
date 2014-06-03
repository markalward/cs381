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

usage(P,T) :- where(C,P),when(C,T). 

conflict(Y,Z) :- where(Y,P),when(Y,T),where(Z,P),when(Z,T),Y\=Z.

meet(A,B) :- enroll(A,C),enroll(B,C),A\=B. 
meet(A,B) :- enroll(A,C),when(C,T),where(C,P),
             enroll(B,D),when(D,U),where(C,P),
             A\=B, T+1=:=U. 
