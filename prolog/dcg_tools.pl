:- module(dcg_tools,
         [
             any//1,
             nl//0,
             space//0,
             space//1,
             spaces//0,
             sp//0,

             optional//1,
             optional//2,
             constant//1,
             '|'//3,
             (//)//2,

             uppercase//1,
             lowercase//1,
             letter//1,
             word//1,
             symbol//1,
             digit//1,
             natural//1,
             integer//1,

             list//1,
             list//2,
             list//3,
             non_greedy//1,
             non_greedy//2
         ]).

:- use_module(library(delay)).

:- multifile delay:mode/1.
delay:mode(lists:append(ground,_)).
delay:mode(lists:append(_,ground)).
delay:mode(dcg_tools:number_codes_(ground,_)).
delay:mode(dcg_tools:number_codes_(_,ground)).


% Higher order predicates
optional(Dcg)      --> optional(Dcg, _).
optional(Dcg, Res) --> call(Dcg, Res) | {Res = ``}.
constant(A) --> seq(A).
'|'(Dcg1, Dcg2, X) --> call(Dcg1, X) | call(Dcg2, X).

//(P1,P2,S1,S2) :- call_dcg(P1,S1,S2), call_dcg(P2,S1,S2).



seq([])     --> [].
seq([E|Es]) --> [E], seq(Es).

any(X)    --> [X].
nl        --> `\n`.
space     --> ` `.
space(_)  --> ` `.
spaces    --> list(space).
sp        --> list(space).

digitCode(C)  --> [C], {code_type(C, digit)}.
symbolCode(C) --> [C], {code_type(C, punct)}.
lowercaseCode(C) --> [C], {code_type(C, lower)}.
uppercaseCode(C) --> [C], {code_type(C, upper)}.
letterCode(C) --> lowercaseCode(C) | uppercaseCode(C).

lowercase(L) --> {delay(atom_codes(L, [C]))}, lowercaseCode(C).
uppercase(L) --> {delay(atom_codes(L, [C]))}, uppercaseCode(C).
letter(L)    --> lowercase(L) | uppercase(L).
word(W)      --> {delay(atom_codes(W, Cs))},
                 list(letter, Cs).
symbol(S)    --> {delay(atom_codes(S, [C]))}, symbolCode(C).


number_codes_(A,B) :- catch(number_codes(A,B), _, false).

digit(D)   --> {delay(number_codes_(D, [C]))},
               digitCode(C).
natural(N) --> {delay(number_codes_(N, Ds))},
               list(digitCode, Ds).
integer(N) --> {delay(number_codes_(N, Codes)),
                delay(append([Sign, Ds], Codes))},
               optional(sign,Sign), list(digitCode, Ds).

sign([0'-]) --> `-`.
sign([0'+]) --> `+`.


% Greedy and non greedy
:- meta_predicate non_greedy(3,*,*).
non_greedy(Goal) --> non_greedy(Goal,_).

:- meta_predicate non_greedy(3,-,*,*).
non_greedy(_Goal,[]) --> [].
non_greedy(Goal,[X|Xs]) --> call(Goal,X), non_greedy(Goal,Xs).

:- meta_predicate list(3,?,?,?).
list(ElemDCG) -->
    list(ElemDCG, _).

:- meta_predicate list(3,?,?,?).
list(ElemDCG, [Elem|Tail]) -->
    call(ElemDCG, Elem),
    list(ElemDCG, Tail).
list(_, []) --> [].

:- meta_predicate list(3,2,?,?,?).
list(ElemDCG, SepDCG, [Elem|Tail]) -->
    call(ElemDCG, Elem),
    (call(SepDCG), list(ElemDCG, SepDCG, Tail) ; {Tail=[]}).
