% Ñòðóêòóðà âóçëà: node(Value, Height, LeftTree, RightTree)
% Ïîðîæíº äåðåâî: empty

% -----------------------------------------------------------------
% get_height(++Tree, --Height)
% Îòðèìóº âèñîòó äåðåâà. Ïîðîæíº äåðåâî ìàº âèñîòó 0.
% Ìóëüòèïðèçíà÷åí³ñòü: (++, --) - îòðèìàííÿ âèñîòè ³ñíóþ÷îãî äåðåâà.
% Ïðèêëàä: get_height(node(5,1,empty,empty), H). -> H=1.
% -----------------------------------------------------------------
get_height(empty, 0).
get_height(node(_, H, _, _), H).

% -----------------------------------------------------------------
% update_height(++L, ++R, --NewH)
% Îá÷èñëþº íîâó âèñîòó âóçëà íà îñíîâ³ éîãî ï³ääåðåâ.
% Ìóëüòèïðèçíà÷åí³ñòü: (++, ++, --) - ðîçðàõóíîê âèñîòè áàòüê³âñüêîãî âóçëà.
% -----------------------------------------------------------------
update_height(L, R, NewH) :-
    get_height(L, HL),
    get_height(R, HR),
    NewH is max(HL, HR) + 1.

% -----------------------------------------------------------------
% get_balance(++Tree, --Balance)
% Îá÷èñëþº êîåô³ö³ºíò áàëàíñó (Ð³çíèöÿ âèñîò ë³âîãî ³ ïðàâîãî ï³ääåðåâ).
% Ìóëüòèïðèçíà÷åí³ñòü: (++, --) - ïåðåâ³ðêà íåîáõ³äíîñò³ ðîòàö³¿.
% -----------------------------------------------------------------
get_balance(empty, 0).
get_balance(node(_, _, L, R), Balance) :-
    get_height(L, HL),
    get_height(R, HR),
    Balance is HL - HR.

% =================================================================
% ÐÎÒÀÖ²¯ (Áàëàíñóâàííÿ)
% =================================================================

% -----------------------------------------------------------------
% rotate_right(++OldTree, --NewTree)
% Ïðàâå îäèíàðíå îáåðòàííÿ (äëÿ ñèòóàö³¿ Left-Left).
% Ìóëüòèïðèçíà÷åí³ñòü: (++, --) - òðàíñôîðìàö³ÿ ñòðóêòóðè äëÿ áàëàíñó.
% -----------------------------------------------------------------
rotate_right(node(Y, _, node(X, _, L, Mid), R), node(X, NH_X, L, node(Y, NH_Y, Mid, R))) :-
    update_height(Mid, R, NH_Y),
    update_height(L, node(Y, NH_Y, Mid, R), NH_X).

% -----------------------------------------------------------------
% rotate_left(++OldTree, --NewTree)
% Ë³âå îäèíàðíå îáåðòàííÿ (äëÿ ñèòóàö³¿ Right-Right).
% -----------------------------------------------------------------
rotate_left(node(X, _, L, node(Y, _, Mid, R)), node(Y, NH_Y, node(X, NH_X, L, Mid), R)) :-
    update_height(L, Mid, NH_X),
    update_height(node(X, NH_X, L, Mid), R, NH_Y).

% -----------------------------------------------------------------
% balance(++Tree, --BalancedTree)
% Ãîëîâíèé ïðåäèêàò áàëàíñóâàííÿ, ùî îáèðàº òèï ðîòàö³¿.
% Ìóëüòèïðèçíà÷åí³ñòü: (++, --) - ïðèâåäåííÿ ðîçáàëàíñîâàíîãî äåðåâà äî íîðìè ÀÂË.
% -----------------------------------------------------------------
balance(node(Val, H, L, R), NewTree) :-
    get_balance(node(Val, H, L, R), B),
    (   B > 1 -> % Ë³âà ñòîðîíà âàæ÷à
        get_balance(L, BL),
        (   BL >= 0 -> rotate_right(node(Val, H, L, R), NewTree) % LL âèïàäîê
        ;   rotate_left(L, NL), rotate_right(node(Val, H, NL, R), NewTree) % LR âèïàäîê
        )
    ;   B < -1 -> % Ïðàâà ñòîðîíà âàæ÷à
        get_balance(R, BR),
        (   BR =< 0 -> rotate_left(node(Val, H, L, R), NewTree) % RR âèïàäîê
        ;   rotate_right(R, NR), rotate_left(node(Val, H, L, NR), NewTree) % RL âèïàäîê
        )
    ;   NewTree = node(Val, H, L, R) % Áàëàíñ ó íîðì³
    ).

% =================================================================
% ÎÑÍÎÂÍ² ÎÏÅÐÀÖ²¯
% =================================================================

% -----------------------------------------------------------------
% insert(++Value, ++OldTree, --NewTree)
% Ðåêóðñèâíà âñòàâêà åëåìåíòà ç ïîäàëüøèì áàëàíñóâàííÿì.
% Ìóëüòèïðèçíà÷åí³ñòü:
% 1. (++, ++, --) - âñòàâêà íîâîãî çíà÷åííÿ.
%    Ïðèêëàä: insert(10, empty, T). -> T = node(10, 1, empty, empty).
% 2. (++, ++, -) - ïåðåâ³ðêà, ÷è ïðèçâåäå âñòàâêà äî ïåâíî¿ ñòðóêòóðè.
% -----------------------------------------------------------------
insert(X, empty, node(X, 1, empty, empty)).
insert(X, node(Val, _, L, R), NewTree) :-
    X < Val,
    insert(X, L, NL),
    update_height(NL, R, NH),
    balance(node(Val, NH, NL, R), NewTree).
insert(X, node(Val, _, L, R), NewTree) :-
    X > Val,
    insert(X, R, NR),
    update_height(L, NR, NH),
    balance(node(Val, NH, L, NR), NewTree).
insert(X, node(X, H, L, R), node(X, H, L, R)). % Çíà÷åííÿ âæå º

% -----------------------------------------------------------------
% contains(++Value, ++Tree)
% Ïåðåâ³ðêà íàÿâíîñò³ åëåìåíòà (ñòàíäàðòíèé ïîøóê ó äâ³éêîâîìó äåðåâ³).
% Ìóëüòèïðèçíà÷åí³ñòü:
% 1. (++, ++) - òåñò íà ïðèíàëåæí³ñòü (True/False).
% 2. (--, ++) - ãåíåðàö³ÿ (ïåðåðàõóíîê) óñ³õ åëåìåíò³â äåðåâà ÷åðåç áåêòðåê³íã.
%    Ïðèêëàä: contains(X, Tree). -> X çíàéäå âñ³ åëåìåíòè ïî ÷åðç³.
% -----------------------------------------------------------------
contains(X, node(X, _, _, _)).
contains(X, node(Val, _, L, _)) :-
    X < Val,
    contains(X, L).
contains(X, node(Val, _, _, R)) :-
    X > Val,
    contains(X, R).

% -----------------------------------------------------------------
% build_tree(++List, --Tree)
% Ñòâîðþº ÀÂË-äåðåâî ç³ ñïèñêó åëåìåíò³â.
% foldl/4 - âáóäîâàíèé ïðåäèêàò âèùîãî ïîðÿäêó (çãîðòêà ñïèñêó).
% Ìóëüòèïðèçíà÷åí³ñòü: (++, --) - ìàñîâà âñòàâêà åëåìåíò³â.
% -----------------------------------------------------------------
build_tree(List, Tree) :-
    foldl(insert, List, empty, Tree).

% =================================================================
% ÎÏÅÐÀÖ²¯ ÍÀÄ ÌÍÎÆÈÍÀÌÈ
% =================================================================

% tree_to_list(++Tree, --List)
% Ïåðåòâîðþº ÀÂË-äåðåâî íà â³äñîðòîâàíèé ñïèñîê (In-order traversal).
% -----------------------------------------------------------------
tree_to_list(empty, []).
tree_to_list(node(V, _, L, R), List) :-
    tree_to_list(L, LL),
    tree_to_list(R, RL),
    append(LL, [V|RL], List).

% -----------------------------------------------------------------
% set_op_list(++T1, ++T2, ++Op, --ResTree)
% Âèêîíóº îïåðàö³þ (union, intersection, subtract) ÷åðåç ñïèñêè.
% Ìóëüòèïðèçíà÷åí³ñòü: (++, ++, ++, --) - ñòâîðåííÿ íîâîãî äåðåâà.
% -----------------------------------------------------------------
set_op_list(T1, T2, Op, ResTree) :-
    tree_to_list(T1, L1),
    tree_to_list(T2, L2),
    (   Op = union     -> ord_union(L1, L2, ResL)        % Âáóäîâàí³ ïðåäèêàòè äëÿ
    ;   Op = intersect -> ord_intersection(L1, L2, ResL) % ðîáîòè ç â³äñîðòîâàíèìè
    ;   Op = diff      -> ord_subtract(L1, L2, ResL)     % ñïèñêàìè (Library ordsets)
    ),
    build_tree(ResL, ResTree).

% -----------------------------------------------------------------
% split(++Tree, ++Pivot, --L, --Found, --R)
% Ðîçáèâàº äåðåâî íà äâà: åëåìåíòè ìåíø³ çà Pivot òà á³ëüø³.
% -----------------------------------------------------------------
split(empty, _, empty, false, empty).
split(node(V, _, L, R), X, Left, Found, Right) :-
    (   X = V -> Found = true, Left = L, Right = R
    ;   X < V ->
        split(L, X, Left, Found, SubR),
        update_height(SubR, R, NH),
        balance(node(V, NH, SubR, R), Right)
    ;   X > V ->
        split(R, X, SubL, Found, Right),
        update_height(L, SubL, NH),
        balance(node(V, NH, L, SubL), Left)
    ).

% -----------------------------------------------------------------
% avl_union(++T1, ++T2, --Res)
% Ïðÿìå îá'ºäíàííÿ äâîõ ÀÂË-äåðåâ.
% -----------------------------------------------------------------
avl_union(empty, T, T).
avl_union(node(V, _, L1, R1), T2, Res) :-
    split(T2, V, L2, _, R2),
    avl_union(L1, L2, NL),
    avl_union(R1, R2, NR),
    update_height(NL, NR, NH),
    balance(node(V, NH, NL, NR), Res).

% -----------------------------------------------------------------
% avl_intersect(++T1, ++T2, --ResTree)
% Ïðÿìå ïåðåòèí äâîõ ÀÂË-äåðåâ.
% Ìóëüòèïðèçíà÷åí³ñòü: (++, ++, --) - ñòâîðþº äåðåâî ç ñï³ëüíèõ åëåìåíò³â.
% -----------------------------------------------------------------
avl_intersect(empty, _, empty).
avl_intersect(_, empty, empty).
avl_intersect(node(V, _, L1, R1), T2, Res) :-
    split(T2, V, L2, Found, R2),
    avl_intersect(L1, L2, NL),
    avl_intersect(R1, R2, NR),
    (   Found = true ->
        update_height(NL, NR, NH),
        balance(node(V, NH, NL, NR), Res)
    ;   join(NL, NR, Res) % ßêùî V íåìàº â T2, çøèâàºìî ë³âå ³ ïðàâå ï³ääåðåâà
    ).

% -----------------------------------------------------------------
% avl_diff(++T1, ++T2, --ResTree)
% Ð³çíèöÿ ìíîæèí (T1 \ T2): åëåìåíòè, ùî º â T1, àëå íåìàº â T2.
% -----------------------------------------------------------------
avl_diff(empty, _, empty).
avl_diff(T1, empty, T1).
avl_diff(node(V, _, L1, R1), T2, Res) :-
    split(T2, V, L2, Found, R2),
    avl_diff(L1, L2, NL),
    avl_diff(R1, R2, NR),
    (   Found = false ->
        update_height(NL, NR, NH),
        balance(node(V, NH, NL, NR), Res)
    ;   join(NL, NR, Res) % ßêùî V çíàéäåíî, âèäàëÿºìî éîãî ç ðåçóëüòàòó
    ).

% -----------------------------------------------------------------
% join(++L, ++R, --Tree)
% Äîïîì³æíèé ïðåäèêàò äëÿ îá'ºäíàííÿ äâîõ äåðåâ ï³ñëÿ âèäàëåííÿ êîðåíÿ.
% Çíàõîäèòü ìàêñèìàëüíèé åëåìåíò ó ë³âîìó äåðåâ³ òà ñòàâèòü éîãî êîð³ííÿì.
% -----------------------------------------------------------------
join(empty, R, R).
join(L, empty, L).
join(L, R, NewTree) :-
    extract_max(L, MaxVal, NewL),
    update_height(NewL, R, NH),
    balance(node(MaxVal, NH, NewL, R), NewTree).

% -----------------------------------------------------------------
% extract_max(++Tree, --Max, --NewTree)
% Âèäàëÿº íàéá³ëüøèé åëåìåíò äëÿ ïåðåñòàíîâêè ïðè áàëàíñóâàíí³.
% -----------------------------------------------------------------
extract_max(node(V, _, L, empty), V, L).
extract_max(node(V, _, L, R), Max, NewTree) :-
    extract_max(R, Max, NR),
    update_height(L, NR, NH),
    balance(node(V, NH, L, NR), NewTree).

% =================================================================
% Â²ÇÓÀË²ÇÀÖ²ß
% =================================================================

% -----------------------------------------------------------------
% print_tree(++Tree)
% Ãîëîâíèé ïðåäèêàò äëÿ çàïóñêó äðóêó. Ïî÷èíàº ç íóëüîâîãî â³äñòóïó.
% Ìóëüòèïðèçíà÷åí³ñòü: (++) - ëèøå âèâåäåííÿ ³ñíóþ÷îãî äåðåâà â êîíñîëü.
% ²íø³ êîìá³íàö³¿ íå ìàþòü ñåíñó, îñê³ëüêè ïðåäèêàò âèêîíóº Side-effect (äðóê).
% -----------------------------------------------------------------
print_tree(Tree) :-
    print_tree(Tree, 0).

% -----------------------------------------------------------------
% print_tree(++Tree, ++Indent)
% Ðåêóðñèâíî äðóêóº äåðåâî: ñïî÷àòêó ïðàâå ï³ääåðåâî, ïîò³ì êîð³íü, ïîò³ì ë³âå.
% tab/1 - âáóäîâàíèé ïðåäèêàò SWI-Prolog äëÿ äðóêó N ïðîá³ë³â.
% -----------------------------------------------------------------
print_tree(empty, _) :- !. % Â³äñ³êàííÿ (cut) äëÿ çóïèíêè ðåêóðñ³¿ íà ïîðîæíüîìó âóçë³.
print_tree(node(Val, _, L, R), Indent) :-
    NewIndent is Indent + 6, % Êðîê â³äñòóïó äëÿ êîæíîãî ð³âíÿ
    print_tree(R, NewIndent), % Ðåêóðñ³ÿ âïðàâî
    tab(Indent),              % Äðóêóºìî â³äñòóï
    format('~w~n', [Val]),    % format/2 - âèâ³ä çíà÷åííÿ ç ïåðåõîäîì íà íîâèé ðÿäîê
    print_tree(L, NewIndent). % Ðåêóðñ³ÿ âë³âî

/** <examples>
?- build_tree([10, 5, 15, 3, 7, 12, 20], T), print_tree(T).
*/
% =================================================================
% ÏÎÐ²ÂÍßÍÍß ÐÅÀË²ÇÀÖ²É
% =================================================================
% -----------------------------------------------------------------
% run_full_benchmark(++Size)
% Ïðîâîäèòü êîìïëåêñíèé òåñò øâèäêîñò³ äëÿ union, intersect òà diff.
% -----------------------------------------------------------------
run_full_benchmark(Size) :-
    % Ï³äãîòîâêà äàíèõ
    length(L1, Size), maplist(random(1, 500000), L1),
    length(L2, Size), maplist(random(1, 500000), L2),
    build_tree(L1, T1),
    build_tree(L2, T2),

    format('--- BENCHMARK: ~w elements ---~n', [Size]),

    % UNION
    benchmark_op(T1, T2, union, avl_union, 'Union'),

    % INTERSECT
    benchmark_op(T1, T2, intersect, avl_intersect, 'Intersection'),

    % DIFF
    benchmark_op(T1, T2, diff, avl_diff, 'Difference').

% -----------------------------------------------------------------
% benchmark_op(++T1, ++T2, ++ListOpName, ++DirectPred, ++Label)
% Âíóòð³øí³é ïðåäèêàò äëÿ çàì³ðó ÷àñó êîíêðåòíî¿ îïåðàö³¿.
% -----------------------------------------------------------------
benchmark_op(T1, T2, ListOp, DirectPred, Label) :-
    % Çàì³ð List-based
    statistics(runtime, [S1|_]),
    set_op_list(T1, T2, ListOp, _),
    statistics(runtime, [E1|_]),
    T_List is E1 - S1,

    % Çàì³ð Direct
    statistics(runtime, [S2|_]),
    call(DirectPred, T1, T2, _), % call/3 - äèíàì³÷íèé âèêëèê ïðåäèêàòà
    statistics(runtime, [E2|_]),
    T_Direct is E2 - S2,

    format('~w: List-based = ~w ms | Direct = ~w ms~n', [Label, T_List, T_Direct]).

    :- begin_tests(avl_set_operations).

% -----------------------------------------------------------------
% test_data(--T1, --T2)
% Äîïîì³æíèé ïðåäèêàò äëÿ ñòâîðåííÿ òåñòîâèõ äåðåâ.
% Ìóëüòèïðèçíà÷åí³ñòü: (-- , --) - ãåíåðóº äâà ô³êñîâàíèõ äåðåâà.
% -----------------------------------------------------------------
test_data(T1, T2) :-
    build_tree([10, 20, 30], T1),
    build_tree([20, 30, 40], T2).

% -----------------------------------------------------------------
% Òåñòè äëÿ UNION (Îá'ºäíàííÿ)
% -----------------------------------------------------------------
test(union_list) :-
    test_data(T1, T2),
    set_op_list(T1, T2, union, Res),
    tree_to_list(Res, List),
    List == [10, 20, 30, 40]. % Î÷³êóºìî îá'ºäíàííÿ áåç äóáë³êàò³â

test(union_direct) :-
    test_data(T1, T2),
    avl_union(T1, T2, Res),
    tree_to_list(Res, List),
    List == [10, 20, 30, 40].

% -----------------------------------------------------------------
% Òåñòè äëÿ INTERSECT (Ïåðåòèí)
% -----------------------------------------------------------------
test(intersect_list) :-
    test_data(T1, T2),
    set_op_list(T1, T2, intersect, Res),
    tree_to_list(Res, List),
    List == [20, 30].

test(intersect_direct) :-
    test_data(T1, T2),
    avl_intersect(T1, T2, Res),
    tree_to_list(Res, List),
    List == [20, 30].

% -----------------------------------------------------------------
% Òåñòè äëÿ DIFF (Ð³çíèöÿ)
% -----------------------------------------------------------------
test(diff_list) :-
    test_data(T1, T2),
    set_op_list(T1, T2, diff, Res),
    tree_to_list(Res, List),
    List == [10]. % Ò³ëüêè òå, ùî º â T1 ³ íåìàº â T2

test(diff_direct) :-
    test_data(T1, T2),
    avl_diff(T1, T2, Res),
    tree_to_list(Res, List),
    List == [10].

% -----------------------------------------------------------------
% Òåñò íà ïîðîæí³ ìíîæèíè
% -----------------------------------------------------------------
test(empty_operations) :-
    build_tree([1, 2, 3], T1),
    avl_intersect(T1, empty, Res),
    Res == empty.

:- end_tests(avl_set_operations).

% Çàïóñê óñ³õ òåñò³â:
% ?- run_tests.
