%% Working with Prolog lists

%=========================================================================
% P1.01 (*) Find the last element of a list.
%   ?- my_last(X, [a,b,c,d]).
%   X = d
my_last(X, List):-
    last(List, X).

%=========================================================================
% P1.02 (*) Find the last but one element of a list.
% ???

%=========================================================================
% P1.03 (*) Find the k'th element of a list.
% The first element in the list is num 1.
%   ?- element_at(X, [a,b,c,d,e], 3).
%   X = c
element_at(X, [X|_], 1):- !.
element_at(Var, [_|Xs], Kth):-
    Kth > 0,
    NewKth is Kth - 1,
    element_at(Var, Xs, NewKth).

%=========================================================================
% P1.04 (*) Find the number of elements of a list.
number_of_elements([], 0).
number_of_elements([_|Xs], N):-
    number_of_elements(Xs, NewN),
    N is NewN + 1.

%=========================================================================
% P1.05 (*) Reverse a list.
reverse_list(List, Reverse):-
    reverse_list(List, [], Reverse).

reverse_list([], Acc, Acc).
reverse_list([X|Xs], Acc, Reverse):-
    reverse_list(Xs, [X|Acc], Reverse).

%=========================================================================
% P1.06 (*) Find out whether a list is a palindrome.
% A palindrome can be read forward or backwards; e.g. [x,a,m,a,x].
is_palidrome(List):-
    is_palidrome(List, []).

is_palidrome(List, List).
is_palidrome([_|Xs], Xs).
is_palidrome([X|Xs], Ys):-
    is_palidrome(Xs, [X|Ys]).

%=========================================================================
% P1.07 (**) Flatten a nested list structure.
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
% P1.08 (**) Eliminate consecutive duplicates of list elements.
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
% P1.09 (**) Pack consecutive duplicates of list elements into sublists.
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
% P1.10 (**) Run-length encoding of a list.
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
% P1.11 (*) Modified run-length encoding.
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
% P1.12 (**) Decode a run-length encoded list.
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
% P1.13 (**) Run-length encoding of a list (direct solution).
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
% P1.14 (*) Duplicate the elements of a list.
%   ?-dupli([a,b,c,d],X).
%   X = [a,a,b,b,c,c,d,d]
dupli([],[]).
dupli([X|Xs], [X,X|Ys]):-
    dupli(Xs, Ys).

%=========================================================================
% P1.15 (**) Duplicate the elements of a list a given number of times.
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
% P1.16 (**) Drop every N'th element from a list.
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
% P1.17 (*) Split a list into two parts; the length of the first part is given.
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
% P1.18 (**) Extract a slice from a list.
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
% P1.19 (**) Rotate a list N places to the left.
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
% P1.20 (*) Remove the Kth element from a list.
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
% P1.21 (*) Insert an element at a given position into a list.
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
% P1.22 (*) Create a list containing all integers within a given range.
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
% P1.23 (**) Extract a given number of randomly selected elements from a list.
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
% P1.24 (*) Lotto: Draw N different random numbers from the set 1..M.
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
% P1.25 (*) Generate a random permutation of the elements of a list.
%   ?- md_select([a,b,c,d,e,f],L).
%   L = [b,a,d,c,e,f]
%
%   Hint: Use the solution of problem P23.
rnd_permu([], []).
rnd_permu([X|Xs], [Y|Rest]):-
    rnd_select([X|Xs], 1, [Y|_]),
    rnd_permu(Xs, Rest).

%========================================================================
% P1.26 (**) Generate the combinations of K distinct objects chosen from the
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
    creap_list(X, List, Xs),
    NewN is N-1,
    combination(NewN, Xs, Rest).
    
creap_list(X, [X|Xs], Xs).
creap_list(X, [_|Ys], Rest):-
    creap_list(X, Ys, Rest).

%========================================================================
% P1.27 (**) Group the elements of a set into disjoint subsets.
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

% P1.27.a Form one group, remove from set, select new group...
group3(List, Group1, Group2, Group3):-
    selectN(2, List, Group1),
    subtract(List, Group1, Remaining),
    selectN(3, Remaining, Group2),
    subtract(Remaining, Group2, Remaining1),
    selectN(4, Remaining1, Group3),
    subtract(Remaining1, Group3, []).

selectN(0, _, []).
selectN(N, List, [X|Xs]):-
    creap_list(X, List, Rest),
    NewN is N - 1,
    selectN(NewN, Rest, Xs).

% P1.27.b
group([],[],[]).
group(List,[N|Ns],[Group|Groups]) :- 
    selectN(N, List, Group),
    subtract(List, Group, Rest),
    group(Rest, Ns, Groups).

%========================================================================
% P1.28 (**) Sorting a list of lists according to length of sublists
% a) We suppose that a list (InList) contains elements that are lists
%    themselves. The objective is to sort the elements of InList according
%    to their length. E.g. short lists first, longer lists later, or vice
%    versa.
%
%   ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%   L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]
%
% b) Again, we suppose that a list (InList) contains elements that are lists
%    themselves. But this time the objective is to sort the elements of 
%    InList according to their length frequency; i.e. in the default, where
%    sorting is done ascendingly, lists with rare lengths are placed first,
%    others with a more frequent length come later.
%
%   ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%   L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
%
% Note that in the above example, the first two lists in the result L have
% length 4 and 1, both lengths appear just once. The third and forth list
% have length 3; there are two list of this length. And finally, the last
% three lists have length 2. This is the most frequent length.


%========================================================================
%========================================================================
%% Arithmetic

%========================================================================
% P2.01 (**) Determine whether a given integer number is prime.
%   ?- is_prime(7).
%   Yes
is_prime(N):-
    prime(N), !.

prime(2).
prime(3).
prime(N):-
    N > 3,
    N mod 2 =\= 0,
    \+ divisible(N, 3).

divisible(N, I):-
    N mod I =:= 0.
divisible(N, I):-
    I*I < N,
    NewI is I + 2,
    divisible(N, NewI).

%========================================================================
% P2.02 (**) Determine the greatest common divisor of two positive integer
% numbers.
% Use Euclid's algorithm.
%   ?- gcd(36, 63, G).
%   G = 9
% Define gcd as an arithmetic function; so you can use it like this:
%   ?- G is gcd(36,63).
%   G = 9
%   note: arithmetic_function(:NameArity) has been deprecated, via SWI Prolog
%   documentation.
%:- arithmetic_function(gcd/2).
gcd(N1, N2, GCD):-
    euclid(N1, N2, GCD), !.

euclid(GCD, 0, GCD).
euclid(N1, N2, GCD):-
    N2 =\= 0,
    N1 > N2,
    NewN2 is N1 mod N2,
    euclid(N2, NewN2, GCD).
euclid(N1, N2, GCD):-           % Flip if N2 > N1
    N2 > N1,
    euclid(N2, N1, GCD).

%========================================================================
% P2.03 (*) Determine whether two positive integer numbers are coprime.
% Two numbers are coprime if their greatest common divisor equals 1.
%   ?- coprime(31,64).
%   Yes
coprime(N1, N2):-
    gcd(N1, N2, 1).

%========================================================================
% P2.04 (**) Calculate Euler's totient function phi(m).
% Euler's so-called totient function φ(m) is defined as the number of 
% positive integers r (1 <= r < m) that are coprime to m.
% Example: m = 10: r = 1,3,7,9; thus φ(m) = 4.
%   Note the special case: phi(1) = 1.
%
%   ?- Phi is totient_phi(10).
%   Phi = 4
%
% Find out what the value of φ(m) is if m is a prime number. Euler's
% totient function plays an important role in one of the most widely used
% public key cryptography methods (RSA). In this exercise you should use
% the most primitive method to calculate this function (there are smarter
% ways that we shall discuss later).
%   φ(prime) ==> prime - 1
%:- arithmetic_function(totient_phi/1).
totient_phi(M, Phi):-
    N is M - 1,
    totient_phi(N, M, Phi1), !,
    Phi is Phi1.

totient_phi(1, _, 1).
totient_phi(N, M, Phi):-
    N > 1, 
    coprime(N, M),
    NewN is N - 1,
    totient_phi(NewN, M, NewPhi),
    Phi is NewPhi + 1.
totient_phi(N, M, Phi):-
    N > 1, 
    \+ coprime(N, M),
    NewN is N - 1,
    totient_phi(NewN, M, Phi).

%========================================================================
% P2.05 (**) Determine the prime factors of a given positive integer.
% Construct a flat list containing the prime factors in ascending order.
%   ?- prime_factors(315,L).
%   L = [3,3,5,7]
prime_factors(N, List):-
    N > 0,
    prime_factors(N, List, 2), !.

prime_factors(1, [], _).
prime_factors(N, [Factor|List], Factor):-   % N multiple of Factor
    Remainder is N // Factor,
    N =:= Remainder * Factor,
    !, prime_factors(Remainder, List, Factor).
prime_factors(N, List, Factor):-            % N not multiple of Factor
    next_factor(N, Factor, Next),
    prime_factors(N, List, Next).

next_factor(_, 2, 3).
next_factor(N, Factor, Next):-
    Factor * Factor < N,
    !, Next is Factor + 2.
next_factor(N, _, N).               % Factor > sqrt(N)

%========================================================================
% P2.06 (**) Determine the prime factors of a given integer (2).
% Construct a list containing the prime factors and their multiplicity.
%   ?- prime_factors_mult(315,L).
%   L = [[3,2],[5,1],[7,1]]
%
%   Hint: The problem is similar to problem P1.13.
prime_factors_mult(N, OutList):-
    prime_factors(N, Factors),
    encode(Factors, TmpList),
    flip_primes_multiples(TmpList, OutList).

flip_primes_multiples([],[]).
flip_primes_multiples([[N, Prime]|Xs], [[Prime, N]|Rest]):-
    flip_primes_multiples(Xs, Rest).
