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

%========================================================================
% P2.07 (**) Calculate Euler's totient function phi(m) (improved).
% See problem P2.04 for the definition of Euler's totient function. If the list
% of prime factors of a number m is known in the form of problem P2.06 then 
% the function φ(m) can be efficiently calculated as follows:
% Let [[p1,m1],[p2,m2],[p3,m3],...] be the list of prime factors (and their
% multiplicities) of a given numberm. Then φ(m) can be calculated with the
% following formula:
%
% φ(m) = (p1 - 1) * p1 ** (m1 - 1) + (p2 - 1) * p2 ** (m2 - 1) + (p3 - 1) * p3 ** (m3 - 1) + ...
%
% Note that a ** b stands for the b'th power of a.
totient_phi2(N, Phi):-
    prime_factors_mult(N, List),
    sum_phi(List, Phi).

sum_phi([], 1).
sum_phi([[Factor, 1]|Xs], Phi):-
    !, sum_phi(Xs, NewPhi),
    Phi is NewPhi * (Factor -1).
sum_phi([[Factor, Multple]|Xs], Phi):-
    Multple > 1,
    NewMultple is Multple -1,
    sum_phi([[Factor, NewMultple]|Xs], NewPhi),
    Phi is NewPhi * Factor.

%========================================================================
% P2.08 (*) Compare the two methods of calculating Euler's totient function.
% Use the solutions of problems P2.04 and P2.07 to compare the algorithms.
% Take the number of logical inferences as a measure for efficiency.
% Try to calculate φ(10090) as an example.

% ?- totient_tests(10090).
% totient_phi: 
% % 742,165 inferences, 0.132 CPU in 0.132 seconds (100% CPU, 5623548 Lips)
% 4032
% totient_phi2: 
% % 154 inferences, 0.000 CPU in 0.000 seconds (99% CPU, 3029588 Lips)
% 4032
% true.
totient_tests(N):-
    write('totient_phi: '),
    time(totient_phi(N, Phi1)),
    write(Phi1), nl,
    write('totient_phi2: '),
    time(totient_phi2(N, Phi2)),
    write(Phi2), nl.

%========================================================================
% P2.09 (*) A list of prime numbers.
% Given a range of integers by its lower and upper limit, construct a list
% of all prime numbers in that range.
list_of_primes(Lower, Upper, List):-
    Lower =< 2,
    acc_primes(2, Upper, List), !.
list_of_primes(Lower, Upper, List):-
    NewLower is (Lower // 2) * 2 + 1,       % make odd if not already
    acc_primes(NewLower, Upper, List).

acc_primes(N, Upper, []):-
    N > Upper, !.
acc_primes(N, Upper, [N|List]):-
    is_prime(N),
    !, next_number(N, NewN),
    acc_primes(NewN, Upper, List).
acc_primes(N, Upper, List):-
    next_number(N, NewN),
    acc_primes(NewN, Upper, List).

next_number(2, 3).
next_number(N, NewN):-
    NewN is N + 2.

%========================================================================
% P2.10 (**) Goldbach's conjecture.
% Goldbach's conjecture says that every positive even number greater than
% 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the
% most famous facts in number theory that has not been  proved to be correct
% in the general case. It has been numerically confirmed up to very large
% numbers (much larger than we can go with our Prolog system). Write a 
% predicate to find two prime numbers that sum up to a given integer.
%
%   ?- goldback(28,L).
%   L = [5,23]

% Leverage backtracking to test set of primes
goldbach(N, Pair):-
    list_of_primes(2, N, Primes),
    goldbach(N, Primes, Pair).

goldbach(N, Primes, [X,Y]):-
    creap_list(X, Primes, _),
    creap_list(Y, Primes, _),
    X < N // 2 + 1,                 % Cut duplicate solutions
    N =:= X + Y.                % Considered cut but likely various solutions

%========================================================================
% P2.11 (**) A list of Goldbach compositions.
% Given a range of integers by its lower and upper limit, print a list of all
% even numbers and their Goldbach composition.
%   ?- goldbach_list(9,20).
%   10 = 3 + 7
%   12 = 5 + 7
%   14 = 3 + 11
%   16 = 3 + 13
%   18 = 5 + 13
%   20 = 3 + 17
%
% In most cases, if an even number is written as the sum of two prime numbers,
% one of them is very small.Very rarely, the primes are both bigger than say 
% 50. Try to find out how many such cases there are in therange 2..3000.
%
% Example (for a print limit of 50):
%   ?- goldbach_list(1,2000,50).
%   992 = 73 + 919
%   1382 = 61 + 1321
%   1856 = 67 + 1789
%   1928 = 61 + 1867
goldbach_list(Lower, Upper):-
    goldbach_list(Lower, Upper, 2).

goldbach_list(Lower, Upper, Limit):-
    Lower =< 4,
    !, list_of_primes(2, Upper, Primes),
    build_range(4, Upper, 2, Range),
    goldbach_range(Range, Primes, Limit), !.
goldbach_list(Lower, Upper, Limit):-
    NewLower is ((Lower + 1) //2) * 2,      % Adjust lower bound
    list_of_primes(2, Upper, Primes),
    build_range(NewLower, Upper, 2, Range),
    goldbach_range(Range, Primes, Limit), !.


% For given even range, find Goldbach compositions
% List of Primes supplied to avoid prime lookups
goldbach_range([], _, _).
goldbach_range([N|Ns], Primes, Limit):-
    creap_list(X, Primes, _),
    creap_list(Y, Primes, _),
    N =:= X + Y,
    print(N, X, Y, Limit),
    goldbach_range(Ns, Primes, Limit).
    
print(N, X, Y, Limit):-
    X >= Limit,
    write(N), write(' = '),
    write(X), write(' + '),
    write(Y), nl.
print(_, _, _, _).              % Suppress output when below Limit

%========================================================================
%========================================================================
%% Logic and Codes

%========================================================================
% P3.01 (**) Truth tables for logical expressions.
% Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 
% (for logical equivalence) whichsucceed or fail according to the result
% of their respective operations; e.g. and(A,B) will succeed, if and only
% if both A and B succeed. Note that A and B can be Prolog goals (not
% only the constants true and fail).
% 
% A logical expression in two variables can then be written in prefix
% notation, as in the following example:and(or(A,B),nand(A,B)).Now, write
% a predicate table/3 which prints the truth table of a given logical
% expression in two variables.
%
%   ?- table(A,B,and(A,or(A,B))).
%   true true true
%   true fail true
%   fail true fail
%   fail fail fail
and(X, Y):- X, Y.

or(X, _):- X.
or(_, X):- X.

equ(X, Y):- or(and(X, Y), and(not(X), not(Y))).

nand(X, Y):- not(and(X, Y)).

nor(X, Y):- not(or(X, Y)).

xor(X, Y):- not(equl(X, Y)).

impl(X, Y):- or(not(X), Y).

bind(true).
bind(fail).

table(A,B,Expr):-
    bind(A),
    bind(B),
    do(A,B,Expr),
    fail.

do(A,B,_):-
    write(A),
    write('  '),
    write(B),
    write('  '),
    fail.
do(_,_,Expr):-
    Expr,
    !, write(true), nl.
do(_,_,_):-
    write(fail), nl.

%========================================================================
% P3.02 (*) Truth tables for logical expressions (2).
% Continue problem P3.01 by defining and/2, or/2, etc as being operators.
% This allows to write the logical expression in the more natural way, as
% in the example: A and (A or not B). Define operator precedence as usual;
% i.e. as in Java.
%
% ?- table(A,B, A and (A or not B)).
% true  true  true
% true  fail  true
% fail  true  fail
% fail  fail  fail
:- op(600, fy,not).
:- op(700, yfx, and).
:- op(700, yfx, nand).
:- op(800, yfx, or).
:- op(800, yfx, nor).
:- op(900, yfx, impl).
:- op(900, yfx, equ).
:- op(900, yfx, xor).

%========================================================================
% P3.03 (*) Truth tables for logical expressions (3).
% Generalize problem P3.02 in such a way that the logical expression may
% contain any number of logical variables. Define table/2 in a way that 
% table(List, Expr) prints the truth table for Expr, which contains the
% logical variables enumerated in List.
%
%   ?- table([A,B,C], A and (B or C) equ A and B or A and C).
%   true true true true
%   true true fail true
%   true fail true true
%   true fail fail true
%   fail true true true
%   fail true fail true
%   fail fail true true
%   fail fail fail true
table(VarList, Expr):-
    bindList(VarList),
    do(VarList,Expr),
    fail.

bindList([]).
bindList([X|Xs]):-
    bind(X),
    bindList(Xs).

do(VarList,Expr):-
    writeVarList(VarList),
    writeExpr(Expr), nl.

writeVarList([]).
writeVarList([X|Xs]):-
    write(X),
    write('  '),
    writeVarList(Xs).

writeExpr(Expr):-
    Expr,
    !, write(true).
writeExpr(_):-
    write(fail).

%========================================================================
% P3.04 (**) Gray code.
% An n-bit Gray code is a sequence of n-bit strings constructed according
% to certain rules. For example,
%   n = 1: C(1) = ['0','1'].
%   n = 2: C(2) = ['00','01','11','10'].
%   n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
%
% Find out the construction rules and write a predicate with the following
% specification:
%   % gray(N,C) :- C is the N-bit Gray code
%
% Can you apply the method of "result caching" in order to make the
% predicate more efficient, when it is to be used repeatedly?
%   Make predicate dynamic, and include asserta line for current args.
%   Solutions of m <= N should now be in listing(gray_code/2),
%   for future use.

%:- dynamic gray_code/2.
gray_code(1, ['0', '1']).
gray_code(N, Code):-
    N > 1,
    NewN is N - 1,
    gray_code(NewN, SubCode),
    reverse(SubCode, ReversedSubCode),
    prepend('0', SubCode, FirstHalf),
    prepend('1', ReversedSubCode, SecondHalf),
    !, append(FirstHalf, SecondHalf, Code).
%    asserta((gray_code(N, Code):- !)).

prepend(_, [], []).
prepend(X, [Code|Codes], [NewCode|NewCodes]):-
    atom_concat(X, Code, NewCode),
    prepend(X, Codes, NewCodes).

%========================================================================
% P3.05 (***) Huffman code.
% First of all, consult a good book on discrete mathematics or algorithms
% for a detailed description of Huffman codes!
%
% We suppose a set of symbols with their frequencies, given as a list of
% fr(S,F) terms. Example: 
% [fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]. Our objective is
% to construct a list hc(S,C) terms, where C is the Huffman code word for
% the symbol S. In our example, the result could be Hs = [hc(a,'0'),
% hc(b,'101'), hc(c,'100'), hc(d,'111'), hc(e,'1101'), hc(f,'1100')]
% [hc(a,'01'),...etc.]. The task shall beperformed by the predicate
% huffman/2 defined as follows:
%
%   % huffman(Fs,Hs) :- Hs is the Huffman code table for the frequency
% table Fs

%========================================================================
%========================================================================
%% Binary Trees
%
% A binary tree is either empty or it is composed of a root element and
% two successors, which are binarytrees themselves. 
%
% In Prolog we represent the empty tree by the atom 'nil' and the
% non-emptytree by the term t(X,L,R), where X denotes the root node and
% L and Rdenote the left and right subtree, respectively. The example
% tree depictedopposite is therefore represented by the following Prolog
% term:
%
% T1 = t(a,t(b,t(d,nil,nil),t(e,nil,nil)),t(c,nil,t(f,t(g,nil,nil),nil)))
%
% Other examples are a binary tree that consists of a root node only:
%
% T2 = t(a,nil,nil) or an empty binary tree: T3 = nil
%
% You can check your predicates using these example trees. They are given
% as test cases in P54.pl.


%========================================================================
% P4.01 (*) Check whether a given term represents a binary tree
% Write a predicate istree/1 which succeeds if and only if its argument is
% a Prolog term representing a binary tree.
%
%   ?- istree(t(a,t(b,nil,nil),nil)).
%   Yes
%   ?- istree(t(a,t(b,nil,nil))).
%   No
istree(nil).
istree(t(_, Left, Right)):-
    istree(Left),
    istree(Right).

%========================================================================
% P4.02 (**) Construct completely balanced binary trees
% In a completely balanced binary tree, the following property holds for
% every node: the number of nodes in its left subtree and the number of nodes
% in its righ subtree are almost equal, which means their difference is not
% greater than one.
%
% Write a predicate cbal_tree/2 to construct completely balanced binary tree
% for a given number of nodes. The predicate should generate all solutions via
% backtracking. Put the letter 'x' as information into all nodes of the tree.
%
%   ?- cbal_tree(4,T).
%   T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
%   T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
%   etc......No
   
cbal_tree(0, nil).
cbal_tree(N, t(x, Left, Right)):-
    N > 0,
    RemainingNodes is N - 1,
    HalfOfNodes is RemainingNodes // 2,
    OtherHalfOfNodes is RemainingNodes - HalfOfNodes,
    distribute_nodes(HalfOfNodes, OtherHalfOfNodes, SubLeft, SubRight),
    cbal_tree(SubLeft, Left),
    cbal_tree(SubRight, Right).

distribute_nodes(N, N, N, N):- !.     % Full tree
distribute_nodes(N, N1, N, N1).     % Alternate left/right nodes
distribute_nodes(N, N1, N1, N).     % Alternate left/right nodes

%========================================================================
% P4.03 (**) Symmetric binary trees
% Let us call a binary tree symmetric if you can draw a vertical line
% through the root node and then the right subtree is the mirror image of
% the left subtree. Write a predicate symmetric/1 to check whether a given
% binary tree is symmetric. Hint: Write a predicate mirror/2 first to check
% whether one tree is the mirror image of another. We are only interested in
% the structure, not in the contents of the nodes.
symmetric(nil).
symmetric(t(_, Left, Right)):-
    mirror(Left, Right).

mirror(nil, nil).
mirror(t(_, Left, Right), t(_, RighttMirrored, LeftMirrored)):-
    mirror(Left, LeftMirrored),
    mirror(Right, RighttMirrored).

%========================================================================
% P4.04 (**) Binary search trees (dictionaries)
% Use the predicate add/3, developed in chapter 4 of the course, to write
% a predicate to construct a binary search tree from a list of integer
% numbers.
%
%   ?- construct([3,2,5,7,1],T).
%   T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
% 
% Then use this predicate to test the solution of the problem P4.03.
%
%   ?- test_symmetric([5,3,18,1,4,12,21]).
%   Yes
%   ?- test_symmetric([3,2,5,7,4]).
%   No
construct(List, Tree):-
    construct(List, nil, Tree), !.

construct([], Tree, Tree).
construct([X|Xs], IntermediateTree, Tree):-
    add(X, IntermediateTree, NewTree),
    construct(Xs, NewTree, Tree).

test_symmetric(List):-
    construct(List, Tree),
    symmetric(Tree).

add(X, nil, t(X, nil, nil)).
add(X, t(Node, Left, Right), t(Node, NewLeft, Right)):-
    X @< Node,      % X is before Node in the standard order of terms
    add(X, Left, NewLeft).
add(X, t(Node, Left, Right), t(Node, Left, NewRight)):-
   X @> Node,       % X is after Node in the standard order of terms
   add(X, Right, NewRight).

%========================================================================
% P4.05 (**) Generate-and-test paradigm
% Apply the generate-and-test paradigm to construct all symmetric, completely
% balanced binary trees given number of nodes. Example:
%
%   ?- sym_cbal_trees(5,Ts).
%   Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]
%
% How many such trees are there with 57 nodes? Investigate about how many
% solutions there are for a given number of nodes? What if the number is even?
% Write an appropriate predicate.
sym_cbal_trees(N, Trees):-
    setof(Tree, sym_cbal_tree(N, Tree), Trees).

sym_cbal_tree(N, Tree):-
    cbal_tree(N, Tree),
    symmetric(Tree).

investigate(Start, End) :-
    between(Start, End, N),                 % Start ≤ N ≤ End
    number_of_trees_given_nodes(N, Trees),
    write(N), write(':'), write(Trees), nl,
    fail.                                   % Force backtrack for rest N
investigate(_, _).

number_of_trees_given_nodes(N, UniqueTrees):-
    sym_cbal_trees(N, Trees),
    length(Trees, UniqueTrees).

%========================================================================
% P4.06 (**) Construct height-balanced binary trees
% In a height-balanced binary tree, the following property holds for every
% node: The height of its leftsubtree and the height of its right subtree
% are almost equal, which means their difference is not greater than one.
%
% Write a predicate hbal_tree/2 to construct height-balanced binary trees
% for a given height. The predicate should generate all solutions via
% backtracking. Put the letter 'x' as information into all nodes of the
% tree.
%
%   ?- hbal_tree(3,T).
%   T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
%   T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
%   etc......No
hbal_tree(0, nil):- !.
hbal_tree(1, t(x, nil, nil)):- !.
hbal_tree(Height, t(x, Left, Right)):-
    Height1 is Height - 1,
    Height2 is Height - 2,
    distribute_nodes(Height1, Height2, SubLeft, SubRight),      % Defined P4.02
    hbal_tree(SubLeft, Left),
    hbal_tree(SubRight, Right).

%========================================================================
% P4.07 (**) Construct height-balanced binary trees with a given number of nodes
% Consider a height-balanced binary tree of height H. What is the maximum
% number of nodes it can contain?
%
% Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This
% question is more difficult. Try to find a recursive statement and turn it
% into a predicate minNodes/2 defined as follows:
% minNodes(H,N) :- N is the minimum number of nodes in a height-balanced
% binary tree of height H.(integer,integer), (+,?)
%
% On the other hand, we might ask: what is the maximum height H a height-
% balanced binary tree with Nnodes can have?
% maxHeight(N,H) :- H is the maximum height of a height-balanced binary
% tree with N nodes(integer,integer), (+,?)
%
% Now, we can attack the main problem: construct all the height-balanced
% binary trees with a given number of nodes.
% hbal_tree_nodes(N,T) :- T is a height-balanced binary tree with N nodes.
%
% Find out how many height-balanced trees exist for N = 15.
