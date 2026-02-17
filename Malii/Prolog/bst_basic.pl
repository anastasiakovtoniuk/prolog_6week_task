% Малій Олександра Михайлівна
% Модуль: Базові операції з бінарним деревом пошуку

:- module(bst_basic, [
    empty_tree/1,
    is_empty/1,
    search/2,
    member_bst/2,
    insert/3,
    delete/3,
    find_min/2,
    find_max/2,
    size/2,
    height/2
]).

% ============================================================================
% Представлення дерева: nil | node(Left, Value, Right)
% де Left, Right - піддерева (nil або node/3)
%     Value - значення вузла (число)
% ============================================================================

% ============================================================================
% Створення та перевірка порожнього дерева
% ============================================================================

% -----------------------------------------------------------------------------
% empty_tree(?Tree)
% Мультипризначенність:
%   empty_tree(--Tree) : створює порожнє дерево (Tree уніфікується з nil)
%   empty_tree(++Tree) : перевіряє, чи Tree вже є порожнім деревом
empty_tree(nil). % Опис: створює або перевіряє порожнє дерево
/** <examples>
?- empty_tree(T).
T = nil.

?- empty_tree(nil).
true.

?- empty_tree(node(nil, 5, nil)).
false.
*/

% -----------------------------------------------------------------------------
% is_empty(+Tree)
% Мультипризначенність:
%   is_empty(++Tree) : детерміновано перевіряє, чи значення дорівнює nil
is_empty(nil). % Опис: істинно лише для порожнього дерева
/** <examples>
?- is_empty(nil).
true.

?- is_empty(node(nil, 5, nil)).
false.
*/

% ============================================================================
% Операція пошуку
% ============================================================================

% -----------------------------------------------------------------------------
% search(+Tree, ?Value)
% Мультипризначенність:
%   search(++Tree, ++Value) : детерміновано перевіряє наявність Value у Tree
%   search(++Tree, --Value) : недетерміновано генерує всі значення з Tree
% Опис: шукаємо значення у дереві
search(node(_, Value, _), Value) :- !. 
search(node(Left, NodeValue, _), Value) :-
    Value < NodeValue,
    search(Left, Value). 
search(node(_, NodeValue, Right), Value) :-
    Value > NodeValue,
    search(Right, Value). 
/** <examples>
?- list_to_bst([5, 3, 7, 1, 9], Tree), search(Tree, 7).
Tree = node(node(node(nil, 1, nil), 3, nil), 5, node(nil, 7, node(nil, 9, nil))),
true.

?- list_to_bst([5, 3, 7, 1, 9], Tree), search(Tree, 10).
false.

?- list_to_bst([5, 3, 7], Tree), search(Tree, X).
X = 5 ;
X = 3 ;
X = 7.
*/

% -----------------------------------------------------------------------------
% member_bst(?Value, +Tree)
% Мультипризначенність: аналогічна до search/2
member_bst(Value, Tree) :- search(Tree, Value). 
/** <examples>
?- list_to_bst([5, 3, 7], Tree), member_bst(3, Tree).
true.

?- list_to_bst([5, 3, 7], Tree), member_bst(X, Tree).
X = 5 ;
X = 3 ;
X = 7.
*/

% ============================================================================
% Операція вставки
% ============================================================================

% -----------------------------------------------------------------------------
% insert(+Tree, +Value, -NewTree)
% Мультипризначенність:
%   insert(++Tree, ++Value, --NewTree) : детерміновано вставляє Value у Tree
insert(nil, Value, node(nil, Value, nil)). % Опис: створює вузол у порожньому дереві
insert(node(Left, Value, Right), Value, node(Left, Value, Right)) :- !. % Опис: дублікати ігноруємо
insert(node(Left, NodeValue, Right), Value, node(NewLeft, NodeValue, Right)) :-
    Value < NodeValue,
    insert(Left, Value, NewLeft). % Опис: вставляємо у ліве піддерево
insert(node(Left, NodeValue, Right), Value, node(Left, NodeValue, NewRight)) :-
    Value > NodeValue,
    insert(Right, Value, NewRight). % Опис: вставляємо у праве піддерево
/** <examples>
?- insert(nil, 5, Tree).
Tree = node(nil, 5, nil).

?- insert(node(nil, 5, nil), 3, Tree).
Tree = node(node(nil, 3, nil), 5, nil).

?- insert(node(nil, 5, nil), 7, Tree).
Tree = node(nil, 5, node(nil, 7, nil)).

?- insert(node(nil, 5, nil), 5, Tree).
Tree = node(nil, 5, nil).
*/

% ============================================================================
% Операція видалення
% ============================================================================

% -----------------------------------------------------------------------------
% delete(+Tree, +Value, -NewTree)
% Мультипризначенність:
%   delete(++Tree, ++Value, --NewTree) : детерміновано видаляє Value
delete(nil, _, nil). % Опис: видалення з порожнього дерева повертає nil
delete(node(Left, Value, Right), Value, Result) :- !,
    delete_root(Left, Right, Result). % Опис: видаляємо корінь і зшиваємо піддерева
delete(node(Left, NodeValue, Right), Value, node(NewLeft, NodeValue, Right)) :-
    Value < NodeValue,
    delete(Left, Value, NewLeft). % Опис: видаляємо в лівому піддереві
delete(node(Left, NodeValue, Right), Value, node(Left, NodeValue, NewRight)) :-
    Value > NodeValue,
    delete(Right, Value, NewRight). % Опис: видаляємо в правому піддереві
/** <examples>
?- delete(node(node(nil, 1, nil), 3, node(nil, 5, nil)), 1, Tree).
Tree = node(nil, 3, node(nil, 5, nil)).

?- delete(node(node(nil, 1, nil), 3, node(nil, 5, nil)), 3, Tree).
Tree = node(node(nil, 1, nil), 5, nil).

?- delete(nil, 5, Tree).
Tree = nil.
*/

% -----------------------------------------------------------------------------
% delete_root(+Left, +Right, -Result)
% Мультипризначенність:
%   delete_root(++Left, ++Right, --Result) : детерміновано комбінує піддерева
delete_root(nil, Right, Right) :- !. % Опис: якщо немає лівого піддерева, повертаємо праве
delete_root(Left, nil, Left) :- !. % Опис: якщо немає правого піддерева, повертаємо ліве
delete_root(Left, Right, node(Left, MinValue, NewRight)) :-
    find_min(Right, MinValue),
    delete(Right, MinValue, NewRight). % Опис: вибираємо мінімум з правого
/** <examples>
?- delete_root(node(nil,1,nil), node(nil,5,nil), R).
R = node(node(nil, 1, nil), 5, nil).
*/

% ============================================================================
% Пошук мінімального та максимального значення
% ============================================================================

% -----------------------------------------------------------------------------
% find_min(+Tree, -MinValue)
% Мультипризначенність:
%   find_min(++Tree, --MinValue) : детерміновано повертає мінімальне значення
find_min(node(nil, Value, _), Value) :- !. % Опис: лівий крайній елемент — мінімум
find_min(node(Left, _, _), MinValue) :-
    find_min(Left, MinValue). % Опис: рухаємось вліво
/** <examples>
?- find_min(node(node(nil, 1, nil), 3, node(nil, 5, nil)), Min).
Min = 1.

?- find_min(node(nil, 5, nil), Min).
Min = 5.
*/

% -----------------------------------------------------------------------------
% find_max(+Tree, -MaxValue)
% Мультипризначенність:
%   find_max(++Tree, --MaxValue) : детерміновано повертає максимальне значення
find_max(node(_, Value, nil), Value) :- !. % Опис: правий крайній елемент — максимум
find_max(node(_, _, Right), MaxValue) :-
    find_max(Right, MaxValue). % Опис: рухаємось вправо
/** <examples>
?- find_max(node(node(nil, 1, nil), 3, node(nil, 5, nil)), Max).
Max = 5.

?- find_max(node(nil, 5, nil), Max).
Max = 5.
*/

% ============================================================================
% Властивості дерева
% ============================================================================

% -----------------------------------------------------------------------------
% size(+Tree, -Size)
% Мультипризначенність:
%   size(++Tree, --Size) : детерміновано підраховує кількість вузлів
size(nil, 0). % Опис: порожнє дерево має розмір 0
size(node(Left, _, Right), Size) :-
    size(Left, LeftSize),
    size(Right, RightSize),
    Size is LeftSize + RightSize + 1. % Опис: розмір = 1 + розміри піддерев
/** <examples>
?- size(nil, S).
S = 0.

?- size(node(node(nil, 1, nil), 3, node(nil, 5, nil)), S).
S = 3.
*/

% -----------------------------------------------------------------------------
% height(+Tree, -Height)
% Мультипризначенність:
%   height(++Tree, --Height) : детерміновано повертає висоту дерева
height(nil, 0). % Опис: порожнє дерево має висоту 0
height(node(Left, _, Right), Height) :-
    height(Left, LeftHeight),
    height(Right, RightHeight),
    Height is max(LeftHeight, RightHeight) + 1. % Опис: висота = 1 + макс з піддерев
/** <examples>
?- height(nil, H).
H = 0.

?- height(node(node(nil, 1, nil), 3, node(nil, 5, nil)), H).
H = 2.
*/
