% Структура вузла: node(Value, Height, LeftTree, RightTree)
% Порожнє дерево: empty

% -----------------------------------------------------------------
% get_height(++Tree, --Height)
% Отримує висоту дерева. Порожнє дерево має висоту 0.
% Мультипризначеність: (++, --) - отримання висоти існуючого дерева.
% Приклад: get_height(node(5,1,empty,empty), H). -> H=1.
% -----------------------------------------------------------------
get_height(empty, 0).
get_height(node(_, H, _, _), H).

% -----------------------------------------------------------------
% update_height(++L, ++R, --NewH)
% Обчислює нову висоту вузла на основі його піддерев.
% Мультипризначеність: (++, ++, --) - розрахунок висоти батьківського вузла.
% -----------------------------------------------------------------
update_height(L, R, NewH) :-
    get_height(L, HL),
    get_height(R, HR),
    NewH is max(HL, HR) + 1.

% -----------------------------------------------------------------
% get_balance(++Tree, --Balance)
% Обчислює коефіцієнт балансу (Різниця висот лівого і правого піддерев).
% Мультипризначеність: (++, --) - перевірка необхідності ротації.
% -----------------------------------------------------------------
get_balance(empty, 0).
get_balance(node(_, _, L, R), Balance) :-
    get_height(L, HL),
    get_height(R, HR),
    Balance is HL - HR.

% =================================================================
% РОТАЦІЇ (Балансування)
% =================================================================

% -----------------------------------------------------------------
% rotate_right(++OldTree, --NewTree)
% Праве одинарне обертання (для ситуації Left-Left).
% Мультипризначеність: (++, --) - трансформація структури для балансу.
% -----------------------------------------------------------------
rotate_right(node(Y, _, node(X, _, L, Mid), R), node(X, NH_X, L, node(Y, NH_Y, Mid, R))) :-
    update_height(Mid, R, NH_Y),
    update_height(L, node(Y, NH_Y, Mid, R), NH_X).

% -----------------------------------------------------------------
% rotate_left(++OldTree, --NewTree)
% Ліве одинарне обертання (для ситуації Right-Right).
% -----------------------------------------------------------------
rotate_left(node(X, _, L, node(Y, _, Mid, R)), node(Y, NH_Y, node(X, NH_X, L, Mid), R)) :-
    update_height(L, Mid, NH_X),
    update_height(node(X, NH_X, L, Mid), R, NH_Y).

% -----------------------------------------------------------------
% balance(++Tree, --BalancedTree)
% Головний предикат балансування, що обирає тип ротації.
% Мультипризначеність: (++, --) - приведення розбалансованого дерева до норми АВЛ.
% -----------------------------------------------------------------
balance(node(Val, H, L, R), NewTree) :-
    get_balance(node(Val, H, L, R), B),
    (   B > 1 -> % Ліва сторона важча
        get_balance(L, BL),
        (   BL >= 0 -> rotate_right(node(Val, H, L, R), NewTree) % LL випадок
        ;   rotate_left(L, NL), rotate_right(node(Val, H, NL, R), NewTree) % LR випадок
        )
    ;   B < -1 -> % Права сторона важча
        get_balance(R, BR),
        (   BR =< 0 -> rotate_left(node(Val, H, L, R), NewTree) % RR випадок
        ;   rotate_right(R, NR), rotate_left(node(Val, H, L, NR), NewTree) % RL випадок
        )
    ;   NewTree = node(Val, H, L, R) % Баланс у нормі
    ).

% =================================================================
% ОСНОВНІ ОПЕРАЦІЇ
% =================================================================

% -----------------------------------------------------------------
% insert(++Value, ++OldTree, --NewTree)
% Рекурсивна вставка елемента з подальшим балансуванням.
% Мультипризначеність:
% 1. (++, ++, --) - вставка нового значення.
%    Приклад: insert(10, empty, T). -> T = node(10, 1, empty, empty).
% 2. (++, ++, -) - перевірка, чи призведе вставка до певної структури.
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
insert(X, node(X, H, L, R), node(X, H, L, R)). % Значення вже є

% -----------------------------------------------------------------
% contains(++Value, ++Tree)
% Перевірка наявності елемента (стандартний пошук у двійковому дереві).
% Мультипризначеність:
% 1. (++, ++) - тест на приналежність (True/False).
% 2. (--, ++) - генерація (перерахунок) усіх елементів дерева через бектрекінг.
%    Приклад: contains(X, Tree). -> X знайде всі елементи по черзі.
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
% Створює АВЛ-дерево зі списку елементів.
% foldl/4 - вбудований предикат вищого порядку (згортка списку).
% Мультипризначеність: (++, --) - масова вставка елементів.
% -----------------------------------------------------------------
build_tree(List, Tree) :-
    foldl(insert, List, empty, Tree).

% =================================================================
% ОПЕРАЦІЇ НАД МНОЖИНАМИ
% =================================================================

% tree_to_list(++Tree, --List)
% Перетворює АВЛ-дерево на відсортований список (In-order traversal).
% -----------------------------------------------------------------
tree_to_list(empty, []).
tree_to_list(node(V, _, L, R), List) :-
    tree_to_list(L, LL),
    tree_to_list(R, RL),
    append(LL, [V|RL], List).

% -----------------------------------------------------------------
% set_op_list(++T1, ++T2, ++Op, --ResTree)
% Виконує операцію (union, intersection, subtract) через списки.
% Мультипризначеність: (++, ++, ++, --) - створення нового дерева.
% -----------------------------------------------------------------
set_op_list(T1, T2, Op, ResTree) :-
    tree_to_list(T1, L1),
    tree_to_list(T2, L2),
    (   Op = union     -> ord_union(L1, L2, ResL)        % Вбудовані предикати для
    ;   Op = intersect -> ord_intersection(L1, L2, ResL) % роботи з відсортованими
    ;   Op = diff      -> ord_subtract(L1, L2, ResL)     % списками (Library ordsets)
    ),
    build_tree(ResL, ResTree).

% -----------------------------------------------------------------
% split(++Tree, ++Pivot, --L, --Found, --R)
% Розбиває дерево на два: елементи менші за Pivot та більші.
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
% Пряме об'єднання двох АВЛ-дерев.
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
% Пряме перетин двох АВЛ-дерев.
% Мультипризначеність: (++, ++, --) - створює дерево з спільних елементів.
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
    ;   join(NL, NR, Res) % Якщо V немає в T2, зшиваємо ліве і праве піддерева
    ).

% -----------------------------------------------------------------
% avl_diff(++T1, ++T2, --ResTree)
% Різниця множин (T1 \ T2): елементи, що є в T1, але немає в T2.
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
    ;   join(NL, NR, Res) % Якщо V знайдено, видаляємо його з результату
    ).

% -----------------------------------------------------------------
% join(++L, ++R, --Tree)
% Допоміжний предикат для об'єднання двох дерев після видалення кореня.
% Знаходить максимальний елемент у лівому дереві та ставить його корінням.
% -----------------------------------------------------------------
join(empty, R, R).
join(L, empty, L).
join(L, R, NewTree) :-
    extract_max(L, MaxVal, NewL),
    update_height(NewL, R, NH),
    balance(node(MaxVal, NH, NewL, R), NewTree).

% -----------------------------------------------------------------
% extract_max(++Tree, --Max, --NewTree)
% Видаляє найбільший елемент для перестановки при балансуванні.
% -----------------------------------------------------------------
extract_max(node(V, _, L, empty), V, L).
extract_max(node(V, _, L, R), Max, NewTree) :-
    extract_max(R, Max, NR),
    update_height(L, NR, NH),
    balance(node(V, NH, L, NR), NewTree).

% =================================================================
% ВІЗУАЛІЗАЦІЯ
% =================================================================

% -----------------------------------------------------------------
% print_tree(++Tree)
% Головний предикат для запуску друку. Починає з нульового відступу.
% Мультипризначеність: (++) - лише виведення існуючого дерева в консоль.
% Інші комбінації не мають сенсу, оскільки предикат виконує Side-effect (друк).
% -----------------------------------------------------------------
print_tree(Tree) :-
    print_tree(Tree, 0).

% -----------------------------------------------------------------
% print_tree(++Tree, ++Indent)
% Рекурсивно друкує дерево: спочатку праве піддерево, потім корінь, потім ліве.
% tab/1 - вбудований предикат SWI-Prolog для друку N пробілів.
% -----------------------------------------------------------------
print_tree(empty, _) :- !. % Відсікання (cut) для зупинки рекурсії на порожньому вузлі.
print_tree(node(Val, _, L, R), Indent) :-
    NewIndent is Indent + 6, % Крок відступу для кожного рівня
    print_tree(R, NewIndent), % Рекурсія вправо
    tab(Indent),              % Друкуємо відступ
    format('~w~n', [Val]),    % format/2 - вивід значення з переходом на новий рядок
    print_tree(L, NewIndent). % Рекурсія вліво

/** <examples>
?- build_tree([10, 5, 15, 3, 7, 12, 20], T), print_tree(T).
*/
% =================================================================
% ПОРІВНЯННЯ РЕАЛІЗАЦІЙ
% =================================================================
% -----------------------------------------------------------------
% run_full_benchmark(++Size)
% Проводить комплексний тест швидкості для union, intersect та diff.
% -----------------------------------------------------------------
run_full_benchmark(Size) :-
    % Підготовка даних
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
% Внутрішній предикат для заміру часу конкретної операції.
% -----------------------------------------------------------------
benchmark_op(T1, T2, ListOp, DirectPred, Label) :-
    % Замір List-based
    statistics(runtime, [S1|_]),
    set_op_list(T1, T2, ListOp, _),
    statistics(runtime, [E1|_]),
    T_List is E1 - S1,

    % Замір Direct
    statistics(runtime, [S2|_]),
    call(DirectPred, T1, T2, _), % call/3 - динамічний виклик предиката
    statistics(runtime, [E2|_]),
    T_Direct is E2 - S2,

    format('~w: List-based = ~w ms | Direct = ~w ms~n', [Label, T_List, T_Direct]).

    :- begin_tests(avl_set_operations).

% -----------------------------------------------------------------
% test_data(--T1, --T2)
% Допоміжний предикат для створення тестових дерев.
% Мультипризначеність: (-- , --) - генерує два фіксованих дерева.
% -----------------------------------------------------------------
test_data(T1, T2) :-
    build_tree([10, 20, 30], T1),
    build_tree([20, 30, 40], T2).

% -----------------------------------------------------------------
% Тести для UNION (Об'єднання)
% -----------------------------------------------------------------
test(union_list) :-
    test_data(T1, T2),
    set_op_list(T1, T2, union, Res),
    tree_to_list(Res, List),
    List == [10, 20, 30, 40]. % Очікуємо об'єднання без дублікатів

test(union_direct) :-
    test_data(T1, T2),
    avl_union(T1, T2, Res),
    tree_to_list(Res, List),
    List == [10, 20, 30, 40].

% -----------------------------------------------------------------
% Тести для INTERSECT (Перетин)
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
% Тести для DIFF (Різниця)
% -----------------------------------------------------------------
test(diff_list) :-
    test_data(T1, T2),
    set_op_list(T1, T2, diff, Res),
    tree_to_list(Res, List),
    List == [10]. % Тільки те, що є в T1 і немає в T2

test(diff_direct) :-
    test_data(T1, T2),
    avl_diff(T1, T2, Res),
    tree_to_list(Res, List),
    List == [10].

% -----------------------------------------------------------------
% Тест на порожні множини
% -----------------------------------------------------------------
test(empty_operations) :-
    build_tree([1, 2, 3], T1),
    avl_intersect(T1, empty, Res),
    Res == empty.

:- end_tests(avl_set_operations).

% Запуск усіх тестів:
% ?- run_tests.
