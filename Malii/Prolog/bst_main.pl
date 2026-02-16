% Малій Олександра Михайлівна
% Завдання: Реалізація бінарного дерева пошуку (BST) в Prolog
% Головний модуль, що об'єднує всі компоненти

% ============================================================================
% Імпорт модулів
% ============================================================================

:- use_module(bst_basic).
:- use_module(bst_sets).
:- use_module(bst_utils).

% ============================================================================
% Реекспорт всіх предикатів для зручності використання
% ============================================================================

% Базові операції (bst_basic)
:- reexport(bst_basic, [
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

% Множинні операції (bst_sets)
:- reexport(bst_sets, [
    union/3,
    intersection/3,
    difference/3,
    symmetric_difference/3,
    union_direct/3,
    intersection_direct/3,
    difference_direct/3,
    symmetric_difference_direct/3
]).

% Допоміжні функції (bst_utils)
:- reexport(bst_utils, [
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