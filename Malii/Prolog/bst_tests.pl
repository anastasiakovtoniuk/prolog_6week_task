% Malij Oleksandra Mykhailivna
% File: BST test suite

:- consult('bst_main.pl').

% ============================================================================
% Test suite for BST operations
% ============================================================================

test_all :-
    write('Starting BST test suite...'), nl, nl,
    test_basic_operations,
    test_set_operations,
    test_direct_operations,
    test_tree_properties,
    test_validation,
    test_traversals,
    write('All tests finished!'), nl.

% ============================================================================
% Basic operations tests
% ============================================================================

test_basic_operations :-
    write('=== Testing basic operations ==='), nl,
    
    write('Test 1: Creating an empty tree... '),
    empty_tree(T0),
    (T0 = nil -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 2: Inserting a single element... '),
    insert(nil, 5, T1),
    (T1 = node(nil, 5, nil) -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 3: Inserting multiple elements... '),
    list_to_bst([5, 3, 7, 1, 9], T2),
    bst_to_list(T2, L2),
    (L2 = [1, 3, 5, 7, 9] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 4: Searching for an existing element... '),
    list_to_bst([5, 3, 7, 1, 9], T3),
    (search(T3, 7) -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 5: Searching for a missing element... '),
    list_to_bst([5, 3, 7, 1, 9], T4),
    (\+ search(T4, 10) -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 6: Deleting a leaf node... '),
    list_to_bst([5, 3, 7, 1, 9], T5),
    delete(T5, 1, T5_del),
    bst_to_list(T5_del, L5),
    (L5 = [3, 5, 7, 9] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 7: Deleting a node with one child... '),
    list_to_bst([5, 3, 7, 9], T6),
    delete(T6, 7, T6_del),
    bst_to_list(T6_del, L6),
    (L6 = [3, 5, 9] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 8: Deleting a node with two children... '),
    list_to_bst([5, 3, 7, 1, 4, 6, 9], T7),
    delete(T7, 5, T7_del),
    bst_to_list(T7_del, L7),
    (L7 = [1, 3, 4, 6, 7, 9] -> write('PASSED') ; write('FAILED')), nl,
    
    nl.

% ============================================================================
% Set operations tests
% ============================================================================

test_set_operations :-
    write('=== Testing set operations ==='), nl,
    
    list_to_bst([1, 3, 5, 7], Tree1),
    list_to_bst([5, 7, 9, 11], Tree2),
    
    write('Test 9: Union of two trees... '),
    union(Tree1, Tree2, UnionTree),
    bst_to_list(UnionTree, UnionList),
    (UnionList = [1, 3, 5, 7, 9, 11] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 10: Intersection of two trees... '),
    intersection(Tree1, Tree2, IntersectTree),
    bst_to_list(IntersectTree, IntersectList),
    (IntersectList = [5, 7] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 11: Difference of two trees... '),
    difference(Tree1, Tree2, DiffTree),
    bst_to_list(DiffTree, DiffList),
    (DiffList = [1, 3] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 12: Symmetric difference... '),
    symmetric_difference(Tree1, Tree2, SymDiff),
    bst_to_list(SymDiff, SymDiffList),
    (SymDiffList = [1, 3, 9, 11] -> write('PASSED') ; write('FAILED')), nl,
    
    nl.

% ============================================================================
% Direct operation tests
% ============================================================================

test_direct_operations :-
    write('=== Testing direct operations ==='), nl,
    
    list_to_bst([1, 3, 5, 7], Tree1),
    list_to_bst([5, 7, 9, 11], Tree2),
    
    write('Test 13: Union (direct)... '),
    union_direct(Tree1, Tree2, UnionDirect),
    bst_to_list(UnionDirect, UnionDirectList),
    (UnionDirectList = [1, 3, 5, 7, 9, 11] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 14: Intersection (direct)... '),
    intersection_direct(Tree1, Tree2, IntersectDirect),
    bst_to_list(IntersectDirect, IntersectDirectList),
    (IntersectDirectList = [5, 7] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 15: Difference (direct)... '),
    difference_direct(Tree1, Tree2, DiffDirect),
    bst_to_list(DiffDirect, DiffDirectList),
    (DiffDirectList = [1, 3] -> write('PASSED') ; write('FAILED')), nl,
    
    nl.

% ============================================================================
% Tree property tests
% ============================================================================

test_tree_properties :-
    write('=== Testing tree properties ==='), nl,
    
    list_to_bst([5, 3, 7, 1, 9], Tree),
    
    write('Test 16: Tree size... '),
    size(Tree, Size),
    (Size = 5 -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 17: Tree height... '),
    height(Tree, Height),
    (Height = 3 -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 18: Minimum value... '),
    find_min(Tree, Min),
    (Min = 1 -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 19: Maximum value... '),
    find_max(Tree, Max),
    (Max = 9 -> write('PASSED') ; write('FAILED')), nl,
    
    nl.

% ============================================================================
% Validation tests
% ============================================================================

test_validation :-
    write('=== Testing BST validation ==='), nl,
    
    write('Test 20: Validating a correct tree... '),
    list_to_bst([5, 3, 7, 1, 9], ValidTree),
    (validate_bst(ValidTree) -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 21: Validating an incorrect tree... '),
    InvalidTree = node(node(nil, 6, nil), 5, node(nil, 7, nil)),
    (\+ validate_bst(InvalidTree) -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 22: Validation with CLP(FD)... '),
    list_to_bst([5, 3, 7, 1, 9], ValidTree2),
    (validate_bst_clpfd(ValidTree2) -> write('PASSED') ; write('FAILED')), nl,
    
    nl.

% ============================================================================
% Tree traversal tests
% ============================================================================

test_traversals :-
    write('=== Testing tree traversals ==='), nl,
    
    list_to_bst([5, 3, 7, 1, 4, 6, 9], Tree),
    
    write('Test 23: Inorder traversal... '),
    inorder(Tree, InList),
    (InList = [1, 3, 4, 5, 6, 7, 9] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 24: Preorder traversal... '),
    preorder(Tree, PreList),
    (PreList = [5, 3, 1, 4, 7, 6, 9] -> write('PASSED') ; write('FAILED')), nl,
    
    write('Test 25: Postorder traversal... '),
    postorder(Tree, PostList),
    (PostList = [1, 4, 3, 6, 9, 7, 5] -> write('PASSED') ; write('FAILED')), nl,
    
    nl.
