% Малій Олександра Михайлівна
% Модуль: Допоміжні функції для BST

:- module(bst_utils, [
    list_to_bst/2,
    bst_to_list/2,
    inorder/2,
    preorder/2,
    postorder/2,
    levelorder/2,
    print_tree/1,
    validate_bst/1,
    validate_bst_clpfd/1,
    list_to_set/2
]).

:- use_module(library(clpfd)).
:- use_module(bst_basic).

% ============================================================================
% Конверсія між деревом та списком
% ============================================================================

% -----------------------------------------------------------------------------
% list_to_bst(+List, -Tree)
% Індикатори та мультипризначенність:
%   list_to_bst(++List, --Tree) : будує BST із відомого списку
%   list_to_bst(--List, ++Tree) : не має сенсу (неможливо однозначно відновити список)
list_to_bst(List, Tree) :-
    list_to_bst_acc(List, nil, Tree). % Опис: будуємо дерево, вставляючи елементи зліва направо

list_to_bst_acc([], Acc, Acc).
list_to_bst_acc([H|T], Acc, Tree) :-
    insert(Acc, H, NextAcc),
    list_to_bst_acc(T, NextAcc, Tree).
/** <examples>
?- list_to_bst([5, 3, 7, 1, 9], Tree).
Tree = node(node(node(nil, 1, nil), 3, nil), 5, node(nil, 7, node(nil, 9, nil))).

?- list_to_bst([], Tree).
Tree = nil.

?- list_to_bst([5, 3, 7, 3, 5], Tree), bst_to_list(Tree, L).
L = [3, 5, 7].
*/

% bst_to_list(+Tree, -List)
% Конвертація BST у відсортований список (симетричний обхід)
% Мультипризначенність: детермінований
/** <examples>
?- bst_to_list(node(node(nil, 1, nil), 3, node(nil, 5, nil)), L).
L = [1, 3, 5].

?- bst_to_list(nil, L).
L = [].
*/
bst_to_list(Tree, List) :-
    inorder(Tree, List).

% ============================================================================
% Обходи дерева
% ============================================================================

% -----------------------------------------------------------------------------
% inorder(+Tree, -List)
% Індикатори:
%   inorder(++Tree, --List) : симетричний обхід (ліво, корінь, право)
inorder(nil, []). % Опис: порожнє дерево повертає порожній список
inorder(node(Left, Value, Right), List) :-
    inorder(Left, LeftList),
    inorder(Right, RightList),
    append(LeftList, [Value|RightList], List). % Опис: збираємо відсортовані значення
/** <examples>
?- inorder(node(node(nil, 1, nil), 3, node(nil, 5, nil)), L).
L = [1, 3, 5].

?- inorder(nil, L).
L = []
*/

% -----------------------------------------------------------------------------
% preorder(+Tree, -List)
% Індикатори:
%   preorder(++Tree, --List) : прямий обхід (корінь, ліво, право)
preorder(nil, []). % Опис: порожнє дерево повертає порожній список
preorder(node(Left, Value, Right), [Value|List]) :-
    preorder(Left, LeftList),
    preorder(Right, RightList),
    append(LeftList, RightList, List). % Опис: корінь + значення лівого та правого
/** <examples>
?- preorder(node(node(nil, 1, nil), 3, node(nil, 5, nil)), L).
L = [3, 1, 5].
*/

% -----------------------------------------------------------------------------
% postorder(+Tree, -List)
% Індикатори:
%   postorder(++Tree, --List) : зворотний обхід (ліво, право, корінь)
postorder(nil, []). % Опис: порожнє дерево повертає порожній список
postorder(node(Left, Value, Right), List) :-
    postorder(Left, LeftList),
    postorder(Right, RightList),
    append(LeftList, RightList, TempList),
    append(TempList, [Value], List). % Опис: додаємо корінь останнім
/** <examples>
?- postorder(node(node(nil, 1, nil), 3, node(nil, 5, nil)), L).
L = [1, 5, 3].
*/

% -----------------------------------------------------------------------------
% levelorder(+Tree, -List)
% Індикатори:
%   levelorder(++Tree, --List) : обхід у ширину
levelorder(Tree, List) :-
    levelorder_helper([Tree], List). % Опис: використовуємо чергу вузлів
/** <examples>
?- levelorder(node(node(nil, 1, nil), 3, node(nil, 5, nil)), L).
L = [3, 1, 5].
*/

% -----------------------------------------------------------------------------
% levelorder_helper(+Queue, -Values)
% Індикатори:
%   levelorder_helper(++Queue, --Values) : службовий обхід
levelorder_helper([], []). % Опис: якщо черга порожня — результат пустий
levelorder_helper(Trees, List) :-
    extract_values(Trees, Values),
    extract_children(Trees, Children),
    append(Values, RestList, List),
    levelorder_helper(Children, RestList). % Опис: рекурсія по наступному рівню

% -----------------------------------------------------------------------------
% extract_values(+Trees, -Values)
% Індикатори:
%   extract_values(++Trees, --Values) : збирає значення вузлів з рівня
extract_values([], []). % Опис: порожній рівень повертає порожній список
extract_values([nil|T], Values) :- !,
    extract_values(T, Values). % Опис: пропускаємо nil
extract_values([node(_, Value, _)|T], [Value|Values]) :-
    extract_values(T, Values). % Опис: додаємо значення вузла
/** <examples>
?- extract_values([node(nil, 1, nil), node(nil, 2, nil)], Values).
Values = [1, 2].
*/

% -----------------------------------------------------------------------------
% extract_children(+Trees, -Children)
% Індикатори:
%   extract_children(++Trees, --Children) : зібрати нащадків вузлів рівня
extract_children([], []). % Опис: порожній рівень повертає порожній список
extract_children([nil|T], Children) :- !,
    extract_children(T, Children). % Опис: пропуск nil
extract_children([node(Left, _, Right)|T], Children) :-
    extract_children(T, RestChildren),
    append([Left, Right], RestChildren, Children). % Опис: додаємо дітей у чергу
/** <examples>
?- extract_children([node(node(nil, 1, nil), 2, node(nil, 3, nil))], Children).
Children = [node(nil, 1, nil), node(nil, 3, nil)].
*/

% ============================================================================
% Візуалізація дерева
% ============================================================================

% -----------------------------------------------------------------------------
% print_tree(+Tree)
% Індикатори:
%   print_tree(++Tree) : детермінований (ефект виведення)
print_tree(Tree) :-
    print_tree_helper(Tree, 0). % Опис: рекурсивно друкуємо дерево із відступами
/** <examples>
?- print_tree(node(node(nil, 1, nil), 3, node(nil, 5, nil))).
        5
    3
        1
true.
*/

print_tree_helper(nil, _).
print_tree_helper(node(Left, Value, Right), Indent) :-
    RightIndent is Indent + 4,
    print_tree_helper(Right, RightIndent),
    print_indent(Indent),
    write(Value), nl,
    print_tree_helper(Left, RightIndent).

% -----------------------------------------------------------------------------
% print_indent(+N)
% Індикатори:
%   print_indent(++N) : службовий предикат для форматування
print_indent(0) :- !.
print_indent(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    print_indent(N1).

% ============================================================================
% Валідація BST
% ============================================================================

% -----------------------------------------------------------------------------
% validate_bst(+Tree)
% Індикатори:
%   validate_bst(++Tree) : детермінована перевірка властивостей BST
validate_bst(Tree) :-
    validate_bst_bounds(Tree, none, none). % Опис: використовує межі-обмежувачі без нескінченностей
/** <examples>
?- validate_bst(node(node(nil, 1, nil), 3, node(nil, 5, nil))).
true.

?- validate_bst(node(node(nil, 6, nil), 5, node(nil, 7, nil))).
false.
*/

validate_bst_bounds(nil, _, _).
validate_bst_bounds(node(Left, Value, Right), Min, Max) :-
    within_lower_bound(Value, Min),
    within_upper_bound(Value, Max),
    validate_bst_bounds(Left, Min, Value),
    validate_bst_bounds(Right, Value, Max).

within_lower_bound(_, none) :- !.
within_lower_bound(Value, Min) :-
    Value > Min.

within_upper_bound(_, none) :- !.
within_upper_bound(Value, Max) :-
    Value < Max.

% -----------------------------------------------------------------------------
% validate_bst_clpfd(+Tree)
% Індикатори:
%   validate_bst_clpfd(++Tree) : перевірка з використанням CLP(FD)
validate_bst_clpfd(Tree) :-
    collect_values(Tree, Values),
    validate_ordering(Tree, Values). % Опис: CLP(FD) контролює порівняння між вузлами
/** <examples>
?- validate_bst_clpfd(node(node(nil, 1, nil), 3, node(nil, 5, nil))).
true.

?- validate_bst_clpfd(node(node(nil, 6, nil), 5, node(nil, 7, nil))).
false.
*/

collect_values(nil, []). % Опис: допоміжна функція для збору значень
collect_values(node(Left, Value, Right), Values) :-
    collect_values(Left, LeftValues),
    collect_values(Right, RightValues),
    append(LeftValues, [Value|RightValues], Values).

validate_ordering(nil, _).
validate_ordering(node(Left, Value, Right), AllValues) :-
    validate_left_subtree(Left, Value, AllValues),
    validate_right_subtree(Right, Value, AllValues),
    validate_ordering(Left, AllValues),
    validate_ordering(Right, AllValues).

validate_left_subtree(nil, _, _).
validate_left_subtree(node(_, LeftValue, _), NodeValue, _) :-
    LeftValue #< NodeValue.

validate_right_subtree(nil, _, _).
validate_right_subtree(node(_, RightValue, _), NodeValue, _) :-
    RightValue #> NodeValue.

% ============================================================================
% Допоміжні предикати для роботи зі списками
% ============================================================================

% -----------------------------------------------------------------------------
% list_to_set(+List, -Set)
% Індикатори:
%   list_to_set(++List, --Set) : видаляє дублікати
list_to_set([], []).
list_to_set([H|T], [H|Set]) :-
    delete_all(T, H, T1),
    list_to_set(T1, Set). % Опис: рекурсивно фільтруємо повтори
/** <examples>
?- list_to_set([1, 2, 3, 2, 1], Set).
Set = [1, 2, 3].

?- list_to_set([], Set).
Set = [].
*/

% -----------------------------------------------------------------------------
% delete_all(+List, +Value, -Result)
% Індикатори:
%   delete_all(++List, ++Value, --Result) : забирає всі входження Value
delete_all([], _, []).
delete_all([H|T], H, Result) :- !,
    delete_all(T, H, Result). % Опис: пропускаємо співпадіння
delete_all([H|T], X, [H|Result]) :-
    delete_all(T, X, Result). % Опис: залишаємо інші значення
