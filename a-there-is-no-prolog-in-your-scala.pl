son(joe,johann).
son(joe,jill).
male(jill).
male(joe).
male(johann).

father(S,F) :- son(S, F), male(F).
