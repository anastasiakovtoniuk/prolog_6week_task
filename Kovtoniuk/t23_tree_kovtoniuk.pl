% Анастасія Ковтонюк
% ------------------------------------------------------------
% Пункт 1. Реалізація 2-3-дерева (збалансованого дерева пошуку)
%
% Представлення дерева:
%   empty
%   two(K, Left, Right)
%   three(K1, K2, Left, Middle, Right)     де K1 @< K2
%
% Інваріанти:
%   - ключі в кожному вузлі відсортовані (для three: K1 @< K2)
%   - у two/3: усі ключі Left @< K, а всі ключі Right @> K
%   - у three/5: Left @< K1, Middle між K1 і K2, Right @> K2
%   - вставка підтримує баланс через split/3 (розщеплення)
%
% Примітка щодо неочевидних вбудованих предикатів/операторів:
%   - @</2, @>/2 : стандартний порядок термів (term order) у Prolog
%   - memberchk/2: перевірка належності (без породження відповідей)
%   - append/3   : конкатенація списків (очевидний, але використовується)
% ------------------------------------------------------------

:- use_module(library(lists)).  % foldl/4 (неочевидний: згортає список у значення)

% ------------------------------------------------------------
% t23_empty(-T)
% Індикатори:
%   1) (--T)  : повернути порожнє дерево.
%   2) (+T)   : перевірити, що дерево порожнє (T = empty).
% Мультипризначенність:
%   - Змістовні режими: (--), (+)
% ------------------------------------------------------------
t23_empty(empty).

/** <examples>
?- t23_empty(T).
% Очікувано: T = empty.

?- t23_empty(empty).
% Очікувано: true.
*/

% ------------------------------------------------------------
% t23_member(++X, ++T)
% Індикатори:
%   1) (++X, ++T) : перевірити належність ключа X дереву T.
% Мультипризначенність:
%   - X має бути конкретизований, бо використовуються порівняння @</@>.
%   - T має бути конкретизоване як коректне 2-3-дерево.
% ------------------------------------------------------------
t23_member(_X, empty) :-
    fail.
t23_member(X, two(K, L, R)) :-
    (   X == K
    ;   X @< K -> t23_member(X, L)
    ;             t23_member(X, R)
    ).
t23_member(X, three(K1, K2, A, B, C)) :-
    (   X == K1
    ;   X == K2
    ;   X @< K1 -> t23_member(X, A)
    ;   X @< K2 -> t23_member(X, B)
    ;             t23_member(X, C)
    ).

/** <examples>
?- t23_from_list([5,2,8,1,3], T), t23_member(3, T).
% Очікувано: true.

?- t23_from_list([5,2,8,1,3], T), t23_member(7, T).
% Очікувано: false.
*/

% ------------------------------------------------------------
% t23_insert(++T, ++X, --T2)
% Індикатори:
%   1) (++T, ++X, --T2) : вставити X у дерево T, отримати нове дерево T2.
% Мультипризначенність:
%   - Змістовний режим один (вставка з побудовою).
%   - Дублікати не додаються (дерево як множина ключів).
% ------------------------------------------------------------
t23_insert(T, X, T2) :-
    ins(T, X, Res),
    (   Res = split(P, L, R)
    ->  T2  = two(P, L, R)
    ;   T2  = Res
    ).

/** <examples>
?- t23_insert(empty, 10, T).
% Очікувано: T = two(10, empty, empty).

?- t23_from_list([5,2,8,1,3,7,9,6,4], T), t23_to_list(T, L).
% Очікувано: L = [1,2,3,4,5,6,7,8,9].
*/

% ------------------------------------------------------------
% t23_to_list(++T, --List)
% Індикатори:
%   1) (++T, --List) : отримати відсортований список ключів дерева (inorder).
% Мультипризначенність:
%   - Змістовний режим один (дерево -> список).
% ------------------------------------------------------------
t23_to_list(empty, []).
t23_to_list(two(K, L, R), List) :-
    t23_to_list(L, LL),
    t23_to_list(R, RL),
    append(LL, [K|RL], List).
t23_to_list(three(K1, K2, A, B, C), List) :-
    t23_to_list(A, LA),
    t23_to_list(B, LB),
    t23_to_list(C, LC),
    append(LA, [K1|LB], T1),
    append(T1, [K2|LC], List).

/** <examples>
?- t23_from_list([3,1,2], T), t23_to_list(T, L).
% Очікувано: L = [1,2,3].
*/

% ------------------------------------------------------------
% t23_from_list(++Xs, --T)
% Індикатори:
%   1) (++Xs, --T) : побудувати 2-3-дерево з елементів списку Xs.
% Мультипризначенність:
%   - Змістовний режим один.
% ------------------------------------------------------------
t23_from_list(Xs, T) :-
    foldl(t23_insert_key, Xs, empty, T).

% t23_insert_key(++X, ++T0, --T1)
% Допоміжний предикат для foldl/4: Elem іде першим аргументом.
t23_insert_key(X, T0, T1) :-
    t23_insert(T0, X, T1).


/** <examples>
?- t23_from_list([5,2,2,1], T), t23_to_list(T, L).
% Очікувано: L = [1,2,5].
*/

% ============================================================
% ВНУТРІШНІ ПРЕДИКАТИ (службові): ins/3, split/3, order2/4, sort3/6
% Коментарі є, але режимні індикатори тут не критичні для здачі API.
% ============================================================

% ins(+Tree, +X, -Result)
% Result = Tree  або  Result = split(PromKey, LeftTree, RightTree)

ins(empty, X, two(X, empty, empty)).

% Лист two/3: або дубль, або перетворюємо на three/5
ins(two(K, empty, empty), X, Res) :- !,
    (   X == K
    ->  Res = two(K, empty, empty)
    ;   order2(X, K, A, B),
        Res = three(A, B, empty, empty, empty)
    ).

% Лист three/5: переповнення -> split
ins(three(K1, K2, empty, empty, empty), X, Res) :- !,
    (   X == K1
    ;   X == K2
    ) ->
        Res = three(K1, K2, empty, empty, empty)
    ;   sort3(K1, K2, X, A, B, C),
        Res = split(B,
                    two(A, empty, empty),
                    two(C, empty, empty)).

% Внутрішній вузол two/3: можемо поглинути split і стати three/5
ins(two(K, L, R), X, Res) :-
    (   X == K
    ->  Res = two(K, L, R)
    ;   X @< K
    ->  ins(L, X, LRes),
        (   LRes = split(P, LL, LR)
        ->  Res  = three(P, K, LL, LR, R)
        ;   Res  = two(K, LRes, R)
        )
    ;   ins(R, X, RRes),
        (   RRes = split(P, RL, RR)
        ->  Res  = three(K, P, L, RL, RR)
        ;   Res  = two(K, L, RRes)
        )
    ).

% Внутрішній вузол three/5: якщо дитина split, треба split цього вузла
ins(three(K1, K2, A, B, C), X, Res) :-
    (   X == K1
    ;   X == K2
    ) ->
        Res = three(K1, K2, A, B, C)
    ;   X @< K1
    ->  ins(A, X, ARes),
        (   ARes = split(P, L1, L2)
        ->  Res  = split(K1,
                         two(P,  L1, L2),
                         two(K2, B,  C))
        ;   Res  = three(K1, K2, ARes, B, C)
        )
    ;   X @< K2
    ->  ins(B, X, BRes),
        (   BRes = split(P, L1, L2)
        ->  Res  = split(P,
                         two(K1, A,  L1),
                         two(K2, L2, C))
        ;   Res  = three(K1, K2, A, BRes, C)
        )
    ;   ins(C, X, CRes),
        (   CRes = split(P, L1, L2)
        ->  Res  = split(K2,
                         two(K1, A, B),
                         two(P,  L1, L2))
        ;   Res  = three(K1, K2, A, B, CRes)
        ).

% order2(+X, +Y, -A, -B) : A @< B (із {X,Y})
order2(X, Y, X, Y) :-
    X @< Y, !.
order2(X, Y, Y, X).

% sort3(+X,+Y,+Z,-A,-B,-C) : A @< B @< C (із {X,Y,Z})
sort3(X, Y, Z, A, B, C) :-
    order2(X, Y, P, Q),
    (   Z @< P
    ->  A = Z, B = P, C = Q
    ;   Z @< Q
    ->  A = P, B = Z, C = Q
    ;   A = P, B = Q, C = Z
    ).
