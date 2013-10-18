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
