% Малій Олександра Михайлівна
% Модуль: Операції над множинами для BST

:- module(bst_sets, [
    union/3,
    intersection/3,
    difference/3,
    symmetric_difference/3,
    union_direct/3,
    intersection_direct/3,
    difference_direct/3,
    symmetric_difference_direct/3
]).

:- use_module(bst_basic).
:- use_module(bst_utils).

% ============================================================================
% Операції над множинами (через списки)
% ============================================================================

% -----------------------------------------------------------------------------
% union(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   union(++Tree1, ++Tree2, --ResultTree) : детерміновано виконує об'єднання
union(Tree1, Tree2, ResultTree) :-
    bst_to_list(Tree1, List1),
    bst_to_list(Tree2, List2),
    append(List1, List2, CombinedList),
    list_to_set(CombinedList, UniqueList),
    list_to_bst(UniqueList, ResultTree). % Опис: поєднуємо значення через списки
/** <examples>
?- union(node(node(nil,1,nil),3,nil), node(node(nil,5,nil),7,nil), U),
   bst_to_list(U, L).
L = [1, 3, 5, 7].

?- union(node(nil,5,nil), node(nil,5,nil), U),
   bst_to_list(U, L).
L = [5].
*/

% -----------------------------------------------------------------------------
% intersection(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   intersection(++Tree1, ++Tree2, --ResultTree) : детермінований перетин
intersection(Tree1, Tree2, ResultTree) :-
    bst_to_list(Tree1, List1),
    bst_to_list(Tree2, List2),
    intersection_lists(List1, List2, IntersectList),
    list_to_bst(IntersectList, ResultTree). % Опис: лишаємо спільні елементи
/** <examples>
?- intersection(node(node(nil,1,nil),3,node(nil,5,nil)),
                node(node(nil,3,nil),5,node(nil,7,nil)), I),
   bst_to_list(I, L).
L = [3, 5].

?- intersection(node(nil,1,nil), node(nil,5,nil), I).
I = nil.
*/

% intersection_lists(+List1, +List2, -Result)
% Допоміжний предикат для перетину списків
intersection_lists([], _, []).
intersection_lists([H|T], List2, [H|Result]) :-
    member(H, List2), !,
    intersection_lists(T, List2, Result).
intersection_lists([_|T], List2, Result) :-
    intersection_lists(T, List2, Result).

% -----------------------------------------------------------------------------
% difference(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   difference(++Tree1, ++Tree2, --ResultTree) : детермінована різниця
difference(Tree1, Tree2, ResultTree) :-
    bst_to_list(Tree1, List1),
    bst_to_list(Tree2, List2),
    difference_lists(List1, List2, DiffList),
    list_to_bst(DiffList, ResultTree). % Опис: елементи Tree1 без Tree2
/** <examples>
?- difference(node(node(nil,1,nil),3,node(nil,5,nil)),
              node(node(nil,3,nil),5,node(nil,7,nil)), D),
   bst_to_list(D, L).
L = [1].

?- difference(node(nil,1,nil), node(nil,5,nil), D),
   bst_to_list(D, L).
L = [1].
*/

% difference_lists(+List1, +List2, -Result)
% Допоміжний предикат для різниці списків
difference_lists([], _, []).
difference_lists([H|T], List2, Result) :-
    member(H, List2), !,
    difference_lists(T, List2, Result).
difference_lists([H|T], List2, [H|Result]) :-
    difference_lists(T, List2, Result).

% -----------------------------------------------------------------------------
% symmetric_difference(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   symmetric_difference(++Tree1, ++Tree2, --ResultTree)
symmetric_difference(Tree1, Tree2, ResultTree) :-
    difference(Tree1, Tree2, Diff1),
    difference(Tree2, Tree1, Diff2),
    union(Diff1, Diff2, ResultTree). % Опис: об'єднуємо різниці
/** <examples>
?- symmetric_difference(node(node(nil,1,nil),3,node(nil,5,nil)),
                        node(node(nil,3,nil),5,node(nil,7,nil)), SD),
   bst_to_list(SD, L).
L = [1, 7].
*/

% ============================================================================
% Операції над множинами (безпосередньо на структурах)
% ============================================================================

% -----------------------------------------------------------------------------
% union_direct(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   union_direct(++Tree1, ++Tree2, --ResultTree) : безпосереднє об'єднання
union_direct(nil, Tree2, Tree2) :- !.
union_direct(Tree1, nil, Tree1) :- !.
union_direct(Tree1, Tree2, ResultTree) :-
    fold_tree(Tree2, Tree1, ResultTree). % Опис: вставляємо Tree2 у Tree1
/** <examples>
?- union_direct(node(node(nil,1,nil),3,nil),
                node(node(nil,5,nil),7,nil), U),
   bst_to_list(U, L).
L = [1, 3, 5, 7].
*/

% fold_tree(+SourceTree, +AccTree, -ResultTree)
% Вставка всіх елементів з SourceTree в AccTree
% Використовує симетричний обхід для збереження порядку
fold_tree(nil, Acc, Acc).
fold_tree(node(Left, Value, Right), Acc, Result) :-
    fold_tree(Left, Acc, Acc1),
    insert(Acc1, Value, Acc2),
    fold_tree(Right, Acc2, Result).

% -----------------------------------------------------------------------------
% intersection_direct(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   intersection_direct(++Tree1, ++Tree2, --ResultTree)
intersection_direct(nil, _, nil) :- !.
intersection_direct(_, nil, nil) :- !.
intersection_direct(Tree1, Tree2, ResultTree) :-
    filter_tree(Tree1, Tree2, ResultTree). % Опис: фільтруємо елементи Tree1
/** <examples>
?- intersection_direct(node(node(nil,1,nil),3,node(nil,5,nil)),
                       node(node(nil,3,nil),5,node(nil,7,nil)), I),
   bst_to_list(I, L).
L = [3, 5].
*/

% filter_tree(+SourceTree, +FilterTree, -ResultTree)
% Залишити тільки елементи, що є в FilterTree
filter_tree(nil, _, nil).
filter_tree(node(Left, Value, Right), FilterTree, Result) :-
    filter_tree(Left, FilterTree, LeftResult),
    filter_tree(Right, FilterTree, RightResult),
    (search(FilterTree, Value) ->
        merge_with_value(LeftResult, Value, RightResult, Result)
    ;
        merge_trees(LeftResult, RightResult, Result)
    ).

% merge_with_value(+Left, +Value, +Right, -Result)
% Об'єднати два дерева з новим значенням у корені
merge_with_value(Left, Value, Right, node(Left, Value, Right)).

% merge_trees(+Tree1, +Tree2, -Result)
% Об'єднати два дерева
merge_trees(nil, Tree, Tree) :- !.
merge_trees(Tree, nil, Tree) :- !.
merge_trees(Tree1, Tree2, Result) :-
    fold_tree(Tree2, Tree1, Result).

% -----------------------------------------------------------------------------
% difference_direct(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   difference_direct(++Tree1, ++Tree2, --ResultTree)
difference_direct(nil, _, nil) :- !.
difference_direct(Tree1, nil, Tree1) :- !.
difference_direct(Tree1, Tree2, ResultTree) :-
    filter_tree_exclude(Tree1, Tree2, ResultTree). % Опис: видаляємо елементи Tree2
/** <examples>
?- difference_direct(node(node(nil,1,nil),3,node(nil,5,nil)),
                     node(node(nil,3,nil),5,node(nil,7,nil)), D),
   bst_to_list(D, L).
L = [1].
*/

% filter_tree_exclude(+SourceTree, +ExcludeTree, -ResultTree)
% Залишити елементи, яких немає в ExcludeTree
filter_tree_exclude(nil, _, nil).
filter_tree_exclude(node(Left, Value, Right), ExcludeTree, Result) :-
    filter_tree_exclude(Left, ExcludeTree, LeftResult),
    filter_tree_exclude(Right, ExcludeTree, RightResult),
    (search(ExcludeTree, Value) ->
        merge_trees(LeftResult, RightResult, Result)
    ;
        merge_with_value(LeftResult, Value, RightResult, Result)
    ).

% -----------------------------------------------------------------------------
% symmetric_difference_direct(+Tree1, +Tree2, -ResultTree)
% Мультипризначенність:
%   symmetric_difference_direct(++Tree1, ++Tree2, --ResultTree)
symmetric_difference_direct(Tree1, Tree2, ResultTree) :-
    difference_direct(Tree1, Tree2, Diff1),
    difference_direct(Tree2, Tree1, Diff2),
    union_direct(Diff1, Diff2, ResultTree). % Опис: симетрична різниця через direct-операції
/** <examples>
?- symmetric_difference_direct(node(node(nil,1,nil),3,node(nil,5,nil)),
                               node(node(nil,3,nil),5,node(nil,7,nil)), SD),
   bst_to_list(SD, L).
L = [1, 7].
*/
