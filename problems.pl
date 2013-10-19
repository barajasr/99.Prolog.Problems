%=========================================================================
% P01 (*) Find the last element of a list.
%   ?- my_last(X, [a,b,c,d]).
%   X = d
my_last(X, List):-
    last(List, X).

%=========================================================================
% P02 (*) Find the last but one element of a list.
% ???

%=========================================================================
% P03 (*) Find the k'th element of a list.
% The first element in the list is num 1.
%   ?- element_at(X, [a,b,c,d,e], 3).
%   X = c
element_at(X, [X|_], 1):- !.
element_at(Var, [_|Xs], Kth):-
    Kth > 0,
    NewKth is Kth - 1,
    element_at(Var, Xs, NewKth).

%=========================================================================
% P04 (*) Find the number of elements of a list.
number_of_elements([], 0).
number_of_elements([_|Xs], N):-
    number_of_elements(Xs, NewN),
    N is NewN + 1.

%=========================================================================
% P05 (*) Reverse a list.
reverse_list(List, Reverse):-
    reverse_list(List, [], Reverse).

reverse_list([], Acc, Acc).
reverse_list([X|Xs], Acc, Reverse):-
    reverse_list(Xs, [X|Acc], Reverse).

%=========================================================================
% P06 (*) Find out whether a list is a palindrome.
% A palindrome can be read forward or backwards; e.g. [x,a,m,a,x].
is_palidrome(List):-
    is_palidrome(List, []).

is_palidrome(List, List).
is_palidrome([_|Xs], Xs).
is_palidrome([X|Xs], Ys):-
    is_palidrome(Xs, [X|Ys]).

%=========================================================================
% P07 (**) Flatten a nested list structure.
% Transform a list, possibly holding lists as elements into a "flat" list by
% replacing each list with its elements (recursively).
%   ?- my_flatten([a,[b,[c,d],e]],X).
my_flatten(List, Flattened):-
    my_flatten(List, [], Flattened).

my_flatten([], Flattened, Flattened).
my_flatten([X|Xs], Acc, Flattened):-
    my_flatten(X, Acc2, Flattened),
    my_flatten(Xs, Acc, Acc2).
my_flatten(Item, Flattened, [Item|Flattened]):-
    \+ is_list(Item).

%=========================================================================
% P08 (**) Eliminate consecutive duplicates of list elements.
% If a list contains repeated elements they should be replaced with a single
% copy of the element. The order of the elements should not be changed.
%   ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [a,b,c,a,d,e]
compress(List, Compressed):-
    compress(List, [], Compressed), !.

compress(List, Acc, Compressed):-
    compress(List, [], Acc, Compressed).

compress([], _, Compressed, Compressed).
compress([X|Xs], X, Acc, Compressed):-
    compress(Xs, X, Acc, Compressed).
compress([X|Xs], _, Acc, Compressed):-
    append(Acc, [X], NewAcc),
    compress(Xs, X, NewAcc, Compressed).

%=========================================================================
% P09 (**) Pack consecutive duplicates of list elements into sublists.
% If a list contains repeated elements they should be placed in seperate
% sublists.
%   ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([], []).
pack([X|Xs], [Z|Zs]):-
    transfer(X, Xs, Ys, Z), !,
    pack(Ys, Zs).

% Ys contains list remaining from the list Xs when all duplicates of X
% are removed and transfered to Z
transfer(X, [], [], [X]).
transfer(X, [Y|Ys], [Y|Ys], [X]):-
    X \= Y.
transfer(X, [X|Xs], Ys, [X|Zs]):-
    transfer(X, Xs, Ys, Zs).

%=========================================================================
% P10 (**) Run-length encoding of a list.
% Use the result of problem P09 to implement the so-called run-length encoding
% data compression method. Consecutive duplicates of elements are encoded as
% terms [N, E] where N is the number of duplicates of the element E.
%   ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
encode([], []).
encode([[Y|Ys]|Xs], Encoded):-                  % Assuming head ([Y|Ys]) packed
    !, count([Y|Ys], N),
    append([[N,Y]], RestEncoded, Encoded),
    encode(Xs, RestEncoded).
encode([X|Xs], Encoded):-                       % If list not packed
    pack([X|Xs], Packed),
    encode(Packed, Encoded).

encode_tail([], []).
encode_tail([[Y|Ys]|Xs], [[N, Y]|Encoded]):-    % Assuming head ([Y|Ys]) packed
    !, count([Y|Ys], N),
    encode_tail(Xs, Encoded).
encode_tail([X|Xs], Encoded):-                  % If list not packed
    pack([X|Xs], Packed),
    encode(Packed, Encoded).

count([], 0).
count([_|Xs], N):-
    count(Xs, NewN),
    N is NewN + 1.

%=========================================================================
% P11 (*) Modified run-length encoding.
% Modify the result of problem P10 in such a way that if an element has no
% duplicates it is simply copied into the result list. Only elements with
% duplicates are transferred as [N,E] terms.
%   ?- encode_modified(a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],b,[2,c],[2,a],d,[4,e]])
encode_modified(List, Encoded):-
    encode_tail(List, TmpCoding),
    modify(TmpCoding, Encoded), !.

modify([], []).
modify([[1, Y]|Xs], [Y|Encoded]):-
    modify(Xs, Encoded).
modify([[N, Y]|Xs], [[N, Y]|Encoded]):-
    modify(Xs, Encoded).

%=========================================================================
% P12 (**) Decode a run-length encoded list.
% Given a run-length code list generated as specified in problem P11. Construct
% its uncompressed version.
decode([], []).
decode([[N, Y]|Xs], Uncompressed):-
    expand(N, Y, List), !,
    append(List, Rest, Uncompressed),
    decode(Xs, Rest).
decode([X|Xs], [X|Uncompressed]):-
    \+ is_list(X), !, 
    decode(Xs, Uncompressed).

expand(0, _, []).
expand(N, Y, [Y|List]):-
    N > 0,
    NewN is N - 1,
    expand(NewN, Y, List).

%=========================================================================
% P13 (**) Run-length encoding of a list (direct solution).
% Implement the so-called run-length encoding data compression method directly.
% I.e.don't explicitly create the sublists containing the duplicates, as in
% P09, but only count them. As in P11, simplify the result list by replacing
% the singleton terms [1,X] by X.
%   ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%   X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_direct([], []).
encode_direct([X|Xs], [[N,X]|Encoded]):-    % X more than 1
    count_duplicates(X, [X|Xs], N, Rest),
    N > 1,
    encode_direct(Rest, Encoded).
encode_direct([X|Xs], [X|Encoded]):-        % X has no duplicates
    encode_direct(Xs, Encoded).

count_duplicates(_, [], 0, []).
count_duplicates(X, [X|Ys], N, Rest):-
    count_duplicates(X, Ys, NewN, Rest),
    N is NewN + 1.
count_duplicates(X, [Y|Ys], 0, [Y|Ys]):-
    X \= Y.

%=========================================================================
% P14 (*) Duplicate the elements of a list.
%   ?-dupli([a,b,c,d],X).
%   X = [a,a,b,b,c,c,d,d]
dupli([],[]).
dupli([X|Xs], [X,X|Ys]):-
    dupli(Xs, Ys).

%=========================================================================
% P15 (**) Duplicate the elements of a list a given number of times.
%   ?-dupli([a,b,c],3,X).
%   X = [a,a,a,b,b,b,c,c,c]
%
% What are the results of the goal:
%   ?-dupli(X,3,Y).
%   X = Y, Y = [] ;
%   X = [_G334],
%   Y = [_G334, _G334, _G334].
dupli([], _, []).
dupli(List, 1, List):- !.
dupli([X|Xs], N, Duplicates):-
    expand(N, X, Expanded),
    append(Expanded, Rest, Duplicates),
    dupli(Xs, N, Rest), !.

%=========================================================================
% P16 (**) Drop every N'th element from a list.
%   ?-drop([a,b,c,d,e,f,g,h,i,k],3,X).
%   X = [a,b,d,e,g,h,k]
drop(List, Kth, Dropped):-
    drop(List, Kth, 1, Dropped).

drop([], _, _, []).
drop([_|Xs], Kth, Kth, Acc):-
    !, drop(Xs, Kth, 1, Acc).
drop([X|Xs], Kth, Cnt, [X|Acc]):-
    Kth > Cnt,
    NewCnt is Cnt + 1,
    drop(Xs, Kth, NewCnt, Acc).

%=========================================================================
% P17 (*) Split a list into two parts; the length of the first part is given.
% Do not use any predefined predicates.
%   ?-split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%   L1 = [a,b,c]
%   L2 = [d,e,f,g,h,i,k]
split(List, Length, List1, List2):-
    split(List, Length, 1, List1, List2).

split(List, 0, _, [], List).                    % L1 size of 0
split([], _, _, [], []).                        % L1 = List, L2 = []
split([X|Xs], Length, Length, [X], Xs).         % Split list here
split([X|Xs], Length, Cnt, [X|List1], List2):-
    Length > Cnt,
    NewCnt is Cnt + 1,
    split(Xs, Length, NewCnt, List1, List2).

%=========================================================================
% P18 (**) Extract a slice from a list.
% Given two indices, I and K, the slice is the list containing the elements
% between the I'th and K'th element of the original list (both limits included).
% Start counting the elements with 1.
%   ?-slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%   X = [c,d,e,f,g]
slice(List, Start, End, Slice):-
    Start > 0,
    Start =< End,
    slice(List, Start, End, 1, Slice).

slice([X|_], _, End, End, [X]).
slice([_|Xs], Start, End, Cnt, Slice):-         % Not at beginning of splice
    Cnt < Start,
    NewCnt is Cnt + 1,
    slice(Xs, Start, End, NewCnt, Slice).
slice([X|Xs], Start, End, Cnt, [X|Slice]):-     % Build Splice
    Cnt >= Start,
    Cnt < End,
    NewCnt is Cnt + 1,
    slice(Xs, Start, End, NewCnt, Slice).
slice([], Start, End, Cnt, []):-                % End < Cnt, end splice here
    Start < Cnt,
    Cnt < End.

%=========================================================================
% P19 (**) Rotate a list N places to the left.
%   ?- rotate([a,b,c,d,e,f,g,h],3,X).
%   X = [d,e,f,g,h,a,b,c]
%   ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%   X = [g,h,a,b,c,d,e,f]
%
% Hint: Use the predefined predicates length/2 and append/3, as well as
% the result of problem P17.
rotate([], _, []).
rotate(List, N, Rotated):-
    length(List, Length),
    N1 is N mod Length,
    split(List, N1, List1, List2),
    append(List2, List1, Rotated).

%=========================================================================
% P20 (*) Remove the Kth element from a list.
%   ?- remove_at(X,[a,b,c,d],2,R).
%   X = b
%   R = [a,c,d]
remove_at(Removed, List, Kth, Remaining):-
    Kth > 0,
    remove_at(List, Kth, 1, Removed, Remaining), !.

remove_at([X|Xs], Kth, Kth, X, Xs).
remove_at([X|Xs], Kth, Cnt, Removed, [X|Rest]):-
    NewCnt is Cnt + 1,
    remove_at(Xs, Kth, NewCnt, Removed, Rest).

%========================================================================
% P21 (*) Insert an element at a given position into a list.
%   ?- insert_at(alfa,[a,b,c,d],2,R).
%   L = [a,alfa,b,c,d]
insert_at(Inserting, List, Kth, Inserted):-
    Kth > 0,
    insert_at(Inserting, List, Kth, 1, Inserted), !.

insert_at(Y, [], _, _, [Y]).
insert_at(Y, [X|Xs], Kth, Kth, [Y,X|Xs]).
insert_at(Y, [X|Xs], Kth, Cnt, [X|Rest]):-
    NewCnt is Cnt + 1,
    insert_at(Y, Xs, Kth, NewCnt, Rest).

%========================================================================
% P22 (*) Create a list containing all integers within a given range.
%   ?- range(4,9,L).
%   L = [4,5,6,7,8,9]
range(Start, End, List):-
    Start =< End,
    build_range(Start, End, 1, List), !.

build_range(End, End, _, [End]).
build_range(Start, End, Step, [Start|Rest]):-
    Start < End,
    NewStart is Start + Step,
    build_range(NewStart, End, Step, Rest).
build_range(Start, End, _, []):-
    Start > End.

%========================================================================
% P23 (**) Extract a given number of randomly selected elements from a list.
% The selected items shall be put into a result list.
%   ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
%   L = [e,d,a]
%
% Hint: Use the built-in random number generator random/2 and the result of
% problem P20.
rnd_select(List, N, Selected):-
    N > 0,
    length(List, Length),
    rnd_select(List, N, Length, Selected), !.

rnd_select([], _, _, []).
rnd_select(_, 0, _, []).
rnd_select(List, N, Size, [X|Rest]):-
    Pos is random(Size) + 1,
    remove_at(X, List, Pos, _),
    NewN is N - 1,
    rnd_select(List, NewN, Size, Rest).

%========================================================================
% P24 (*) Lotto: Draw N different random numbers from the set 1..M.
% The selected numbers shall be put into a result list.
%   ?- rnd_select(6,49,L).
%   L = [23,1,17,33,21,37]
%
% Hint: Combine the solutions of problems P22 and P23.
lotto(1, End, [X]):-
    X is random(End) + 1, !.
lotto(N, End, [X|Rest]):-
    X is random(End) + 1,
    NewN is N - 1,
    lotto(NewN, End, Rest).

%========================================================================
% P25 (*) Generate a random permutation of the elements of a list.
%   ?- md_select([a,b,c,d,e,f],L).
%   L = [b,a,d,c,e,f]
%
%   Hint: Use the solution of problem P23.
rnd_permu([], []).
rnd_permu([X|Xs], [Y|Rest]):-
    rnd_select([X|Xs], 1, [Y|_]),
    rnd_permu(Xs, Rest).

%========================================================================
% P26 (**) Generate the combinations of K distinct objects chosen from the
% N elements of a list.
% In how many ways can acommitte of 3 be chosen from a group of 12 people?
% We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes
% the well-known binomial coefficients). For pure mathematicians, this
% result may be great. But we want to really generate all the possibilities
% (via backtracing).
%   ?- combination(3,[a,b,c,d,e,f],L).
%   L = [a,b,c] ;
%   L = [a,b,d] ;
%   L = [a,b,e] ;
%   ... 
combination(0, _, []).
combination(N, List, [X|Rest]) :-
    N > 0,
    creep_list(X, List, Xs),
    NewN is N-1,
    combination(NewN, Xs, Rest).
    
creep_list(X, [X|Xs], Xs).
creep_list(X, [_|Ys], Rest):-
    creep_list(X, Ys, Rest).

%========================================================================
% P27 (**) Group the elements of a set into disjoint subsets.
% a) In how many ways can a group of 9 people work in 3 disjoint subgroups of
%    2, 3 and 4 persons? Write a predicate that generates all the possibilities
%    via backtracking.
%   ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
%   G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
%   ...
%
% b) Generalize the above predicate in a way that we can specify a list of
% group sizes and the predicate will return a list of groups.
%   ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
%   Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
%   ...
%
% Note that we do not want permutations of the group members; i.e.
% [[aldo,beat],...] is the same solution as [[beat,aldo],...]. However, we
% make a difference between [[aldo,beat],[carla,david],...] and
% [[carla,david],[aldo,beat],...].
%
% You may find more about this combinatorial problem in a good book on
% discrete mathematics under the term "multinomial coefficients".

