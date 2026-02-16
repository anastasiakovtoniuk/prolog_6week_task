% Анастасія Ковтонюк
% ------------------------------------------------------------
% Пункт 2. Реалізація (m,n)-дерева (узагальнення багатошляхового дерева)
%
% Представлення дерева:
%   empty
%   leaf(Keys)                   % відсортовані унікальні ключі
%   node(Keys, Children)          % Keys відсортовані, Children = length(Keys)+1
%
% Інваріанти:
%   - Keys у вузлі відсортовані та без дублікатів
%   - Children має розмір на 1 більше, ніж Keys (для node/2)
%   - (m,n)-обмеження (для внутрішніх вузлів, окрім кореня):
%       кількість дітей   у [M, N]
%       кількість ключів  у [M-1, N-1]
%   - Умова коректного split:
%       N >= 2*M - 1  (щоб після розщеплення обидві частини мали >= M-1 ключів)
%
% Неочевидні вбудовані предикати:
%   - must_be/2   : перевірка типів (SWI-Prolog)
%   - memberchk/2 : перевірка належності без породження
%   - nth1/3      : доступ до елемента списку за 1-based індексом
%   - foldl/4     : "згортання" списку
% ------------------------------------------------------------

:- use_module(library(lists)).   % foldl/4, length/2, nth1/3

% ------------------------------------------------------------
% mn_empty(-T)
% Індикатори:
%   1) (--T) : повернути порожнє дерево.
%   2) (+T)  : перевірити, що дерево порожнє.
% ------------------------------------------------------------
mn_empty(empty).

/** <examples>
?- mn_empty(T).
% Очікувано: T = empty.

?- mn_empty(empty).
% Очікувано: true.
*/

% ------------------------------------------------------------
% mn_member(++X, ++T)
% Індикатори:
%   1) (++X, ++T) : перевірка належності ключа X дереву T.
% Мультипризначенність:
%   - Не породжує значення X (бо memberchk/2 не генерує варіанти).
% ------------------------------------------------------------
mn_member(_X, empty) :-
    fail.
mn_member(X, leaf(Keys)) :-
    memberchk(X, Keys).
mn_member(X, node(Keys, Children)) :-
    (   memberchk(X, Keys)
    ->  true
    ;   child_index(X, Keys, Idx),
        nth1(Idx, Children, Child),
        mn_member(X, Child)
    ).

/** <examples>
?- mn_from_list(2, 3, [5,2,8,1,3], T), mn_member(8, T).
% Очікувано: true.

?- mn_from_list(2, 3, [5,2,8,1,3], T), mn_member(7, T).
% Очікувано: false.
*/

% ------------------------------------------------------------
% mn_insert(++M, ++N, ++T, ++X, --T2)
% Індикатори:
%   1) (++M, ++N, ++T, ++X, --T2) : вставити X у дерево T з параметрами (M,N).
% Мультипризначенність:
%   - Змістовний режим один: побудова нового дерева.
% ------------------------------------------------------------
mn_insert(M, N, T, X, T2) :-
    must_be(integer, M),
    must_be(integer, N),
    M >= 2,
    N >= 2*M - 1,                 % важливо для коректного split
    ins_mn(M, N, T, X, Res),
    (   Res = split(P, L, R)
    ->  T2 = node([P], [L, R])
    ;   T2 = Res
    ).

/** <examples>
?- mn_insert(2, 3, empty, 10, T).
% Очікувано: T = leaf([10]).

?- mn_from_list(2, 3, [5,2,8,1,3,7,9,6,4], T), mn_to_list(T, L).
% Очікувано: L = [1,2,3,4,5,6,7,8,9].

?- mn_from_list(3, 5, [10,20,5,6,12,30,7,17], T), mn_to_list(T, L).
% Очікувано: L = [5,6,7,10,12,17,20,30].
*/

% ------------------------------------------------------------
% mn_to_list(++T, --List)
% Індикатори:
%   1) (++T, --List) : отримати відсортований список ключів (inorder).
% ------------------------------------------------------------
mn_to_list(empty, []).
mn_to_list(leaf(Keys), Keys).
mn_to_list(node(Keys, Children), List) :-
    mn_to_list_ck(Children, Keys, List).

% mn_to_list_ck(++Children, ++Keys, --List)
% Допоміжний inorder-обхід для node/2.
% Важливо: length(Children) = length(Keys) + 1
mn_to_list_ck([Child], [], List) :- !,
    mn_to_list(Child, List).
mn_to_list_ck([Child1, Child2 | RestChildren], [K | RestKeys], List) :-
    mn_to_list(Child1, L1),
    mn_to_list_ck([Child2 | RestChildren], RestKeys, L2),
    append(L1, [K | L2], List).

/** <examples>
?- mn_to_list(leaf([1,2,3]), L).
% Очікувано: L = [1,2,3].
*/

% ------------------------------------------------------------
% mn_from_list(++M, ++N, ++Xs, --T)
% Індикатори:
%   1) (++M, ++N, ++Xs, --T) : побудувати (m,n)-дерево зі списку Xs.
% ------------------------------------------------------------
mn_from_list(M, N, Xs, T) :-
    foldl(mn_insert_key(M, N), Xs, empty, T).

/** <examples>
?- mn_from_list(2, 3, [3,1,2,2], T), mn_to_list(T, L).
% Очікувано: L = [1,2,3].
*/

% ============================================================
% ВНУТРІШНІ ПРЕДИКАТИ (службові): ins_mn/5, split_node/3 тощо
% ============================================================

% ins_mn(+M,+N,+Tree,+X,-Res)
% Res = Tree  або  Res = split(Prom, LeftTree, RightTree)

ins_mn(_M, _N, empty, X, leaf([X])).

ins_mn(M, N, leaf(Keys), X, Res) :-
    insert_sorted_unique(X, Keys, Keys2, Inserted),
    (   Inserted == false
    ->  Res = leaf(Keys)
    ;   MaxKeys is N - 1,
        length(Keys2, Len),
        (   Len =< MaxKeys
        ->  Res = leaf(Keys2)
        ;   split_leaf(M, Keys2, split(P, LK, RK)),
            Res = split(P, leaf(LK), leaf(RK))
        )
    ).

ins_mn(M, N, node(Keys, Children), X, Res) :-
    (   memberchk(X, Keys)
    ->  Res = node(Keys, Children)
    ;   child_index(X, Keys, Idx),
        nth1(Idx, Children, Child),
        ins_mn(M, N, Child, X, ChildRes),
        (   ChildRes = split(PK, L, R)
        ->  insert_at(Idx, PK, Keys, Keys2),
            replace_child_with_two(Idx, L, R, Children, Children2),
            MaxKeys is N - 1,
            length(Keys2, KLen),
            (   KLen =< MaxKeys
            ->  Res = node(Keys2, Children2)
            ;   split_node(M, node(Keys2, Children2), Res)
            )
        ;   replace_nth(Idx, Children, ChildRes, Children2),
            Res = node(Keys, Children2)
        )
    ).

% split_leaf(+M, +KeysN, -split(Prom, LeftKeys, RightKeys))
% KeysN має довжину N (переповнення: N ключів).
% Промотуємо M-й ключ.
split_leaf(M, Keys, split(Prom, LeftKeys, RightKeys)) :-
    KLeft is M - 1,
    split_at(KLeft, Keys, LeftKeys, Rest),
    Rest = [Prom|RightKeys].

% split_node(+M, +node(KeysN, ChildrenN), -ResSplit)
% Промотуємо M-й ключ; дітей ліворуч = M, праворуч = N-M+1.
split_node(M, node(Keys, Children), Res) :-
    KLeft is M - 1,
    split_at(KLeft, Keys, LeftKeys, RestKeys),
    RestKeys = [Prom|RightKeys],
    split_at(M, Children, LeftChildren, RightChildren),
    make_node(LeftKeys,  LeftChildren,  LeftTree),
    make_node(RightKeys, RightChildren, RightTree),
    Res = split(Prom, LeftTree, RightTree).

% make_node(+Keys, +Children, -Tree)
% Якщо Children порожні — це лист; інакше — внутрішній вузол.
make_node(Keys, [], leaf(Keys)) :- !.
make_node(Keys, Children, node(Keys, Children)).

% child_index(+X, +Keys, -Idx)
% Вибір індексу дитини (1..length(Keys)+1), куди має йти X.
child_index(_X, [], 1).
child_index(X, [K|Ks], Idx) :-
    (   X @< K
    ->  Idx = 1
    ;   child_index(X, Ks, I1),
        Idx is I1 + 1
    ).

% insert_sorted_unique(+X,+Sorted,-Sorted2,-Inserted)
% Вставка у відсортований список без дублікатів.
insert_sorted_unique(X, [], [X], true).
insert_sorted_unique(X, [H|T], [X,H|T], true) :-
    X @< H, !.
insert_sorted_unique(X, [H|T], [H|T], false) :-
    X == H, !.
insert_sorted_unique(X, [H|T], [H|T2], Inserted) :-
    X @> H,
    insert_sorted_unique(X, T, T2, Inserted).

% insert_at(+Pos, +Elem, +List, -List2)  (Pos 1-based)
insert_at(1, X, L, [X|L]) :- !.
insert_at(I, X, [H|T], [H|R]) :-
    I > 1,
    I1 is I - 1,
    insert_at(I1, X, T, R).

% replace_nth(+Pos,+List,+Elem,-List2) (Pos 1-based)
replace_nth(1, [_|T], X, [X|T]) :- !.
replace_nth(I, [H|T], X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace_nth(I1, T, X, R).

% replace_child_with_two(+Pos,+L,+R,+Children,-Children2)
% Замінює дитину на позиції Pos двома дітьми L і R.
replace_child_with_two(1, L, R, [_|T], [L,R|T]) :- !.
replace_child_with_two(I, L, R, [H|T], [H|T2]) :-
    I > 1,
    I1 is I - 1,
    replace_child_with_two(I1, L, R, T, T2).

% split_at(+N,+List,-Front,-Back)
split_at(0, L, [], L) :- !.
split_at(N, [H|T], [H|F], B) :-
    N > 0,
    N1 is N - 1,
    split_at(N1, T, F, B).

% inorder_pairs(+Keys, +ChildrenTail, -List)
% Допоміжний inorder-обхід: між дітьми вставляємо ключі.
inorder_pairs([], [Last], List) :-
    mn_to_list(Last, List).
inorder_pairs([K|Ks], [C|Cs], List) :-
    mn_to_list(C, LC),
    inorder_pairs(Ks, Cs, Rest),
    append([K|LC], Rest, List).

% mn_insert_key(+M,+N,+X,+T0,-T1) для foldl/4
mn_insert_key(M, N, X, T0, T1) :-
    mn_insert(M, N, T0, X, T1).
