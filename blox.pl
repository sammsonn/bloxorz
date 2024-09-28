:- dynamic detailed_mode_disabled/0.
:- dynamic debug_moves/0.
:- ensure_loaded('files.pl').


%% TODO
% empty_state/1
% empty_state(-SNew)
% Construiește o stare goală (fără nicio informație), care va fi dată
% primului apel set/4
empty_state([]).



%%%%%%
% coordonata (0, 0) este coltul din stanga/sus (chiar dacă nu există un
% tile acolo)

%% TODO
% set_tile/3
% set_tile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află o pătrățică normală.
set_tile(S, Pos, [(tile, Pos)|S]).



%% TODO
% set_blank/3
% set_blank(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S.
% Va fi apelat de tester doar pentru pozițiile fără pătrățele de la
% coordonate unde pentru același x și y mai mare, sau pentru același y
% și x mai mare, există pătrățele. Puteți să nu faceți nimic în acest
% predicat - depinde de cum vă reprezentați intern starea.
set_blank(S, _, S).



%% TODO
% set_target/3
% set_target(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află gaura (scopul).
set_target(S, Pos, [(target, Pos)|S]).



%% TODO
% set_fragile/3
% set_fragile(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se o pătrățică fragilă, pe care
% blocul nu poate sta în picioare.
set_fragile(S, Pos, [(fragile, Pos)|S]).



%% TODO
% set_block_initial/3
% set_block_initial(+S, +Pos, -SNew)
% Construiește starea SNew, care conține aceleași informații ca și S
% și în plus faptul că la poziția Pos se află inițial blocul, plasat în
% picioare.
set_block_initial(S, Pos, [(tile, Pos), (block, Pos)|S]).



%% TODO
% get_b_pos/2
% get_b_pos(+S, -BlockPos)
% Obtine pozitia sau pozitiile blocului (în funcție de dacă blocul este
% în picioare sau culcat, ca (X, Y) sau ca [(X1, Y1), (X2, Y2)]
get_b_pos(S, BlockPos) :-
    member((block, BlockPos), S).



%% TODO
% get_bounds/5
% get_bounds(+S, -Xmin, -Xmax, -Ymin, -Ymax).
% Obtine coordonatele limită de pe hartă la care exită celule.
% pentru switch-uri
get_bounds([(Switch, Func, Positions, (X, Y))|T], Xmin, Xmax, Ymin, Ymax) :-
    get_bounds(T, X, X, Y, Y, Xmin, Xmax, Ymin, Ymax).

% pentru patratele normale
get_bounds([(Type, (X, Y))|T], Xmin, Xmax, Ymin, Ymax) :-
    get_bounds(T, X, X, Y, Y, Xmin, Xmax, Ymin, Ymax).

% cazul de baza
get_bounds([], Xmin, Xmax, Ymin, Ymax, Xmin, Xmax, Ymin, Ymax).

% pentru switch-uri
get_bounds([(Switch, Func, Positions, (X, Y))|T], Xmin0, Xmax0, Ymin0, Ymax0, Xmin, Xmax, Ymin, Ymax) :-
    Xmin1 is min(X, Xmin0),
    Xmax1 is max(X, Xmax0),
    Ymin1 is min(Y, Ymin0),
    Ymax1 is max(Y, Ymax0),
    get_bounds(T, Xmin1, Xmax1, Ymin1, Ymax1, Xmin, Xmax, Ymin, Ymax).

% pentru patratele normale
get_bounds([(Type, (X, Y))|T], Xmin0, Xmax0, Ymin0, Ymax0, Xmin, Xmax, Ymin, Ymax) :-
    Xmin1 is min(X, Xmin0),
    Xmax1 is max(X, Xmax0),
    Ymin1 is min(Y, Ymin0),
    Ymax1 is max(Y, Ymax0),
    get_bounds(T, Xmin1, Xmax1, Ymin1, Ymax1, Xmin, Xmax, Ymin, Ymax).



%% TODO
% get_cell/3
% get_cell(S, Pos, Type).
% Leagă Type la tipul pătrățelei de la poziția Pos. Type trebuie legat
% la:
% tile - pentru o pătrățică obișnuită.
% fragile - pentru o pătrățică fragilă.
% target - pentru scop (gaura).
% oswitch - pentru switch de tip o.
% xswitch - pentru switch de tip x.
%
% Dacă la poziția respectivă nu se află nimic, sau este în afara
% limitelor date de get_bounds, predicatul întoarce false.
get_cell(S, Pos, Type) :-
    member((Type, Pos), S);
    member((Type, _, _, Pos), S).



%% TODO
% move/3
% move(S, Move, SNext)
% Calculează în SNext starea care rezultă din realizarea mutării Move în
% starea S.
% Mutarea este una dintre d, u, l, r.
% Întoarce false dacă mutarea duce la căderea blocului în vid (nu dacă
% blocul a ajuns la scop).
move(S, Move, SNext) :-
    get_b_pos(S, BlockPos),
    (   % cazul 1: blocul este in picioare si va ajunge culcat
        BlockPos = (X, Y),
        % gasesc vecinii blocului, ii verific, mut blocul si actualizez podul daca e switch
        neighbor((X, Y), Move, (XNext1, YNext1)),
        neighbor2((X, Y), Move, (XNext2, YNext2)),
        both_positions_valid(S, [(XNext1, YNext1), (XNext2, YNext2)]),
        update_state(S, (X, Y), [(XNext1, YNext1), (XNext2, YNext2)], S1),
        (   is_switch(S1, (XNext1, YNext1), laying, Func, Positions),
            update_bridge(S1, Func, Positions, SNext)
        ;   is_switch(S1, (XNext2, YNext2), laying, Func, Positions),
            update_bridge(S1, Func, Positions, SNext)
        ;   SNext = S1
        ), !
    ;   % cazul 2: blocul este culcat si se va ridica
        BlockPos = [(X1, Y1), (X2, Y2)],
        % gasesc vecinii blocului, ii verific, mut blocul si actualizez podul daca e switch
        (   % blocul este intins vertical si se misca in sus sau in jos
            X1 =:= X2, 
            vertical_move(Move, X1, Y1, X2, Y2, XNext, YNext)
        ;   % blocul este intins orizontal si se misca la stanga sau la dreapta
            Y1 =:= Y2, 
            horizontal_move(Move, X1, Y1, X2, Y2, XNext, YNext)
        ),
        get_cell(S, (XNext, YNext), Type),
        (Type = fragile -> !, fail; true),
        update_state(S, [(X1, Y1), (X2, Y2)], (XNext, YNext), S1),
        (   is_switch(S1, (XNext, YNext), standing, Func, Positions),
            update_bridge(S1, Func, Positions, SNext)
        ;   SNext = S1
        ), !
    ;   % cazul 3: blocul este culcat si va continua sa stea culcat
        BlockPos = [(X1, Y1), (X2, Y2)],
        % gasesc vecinii blocului, ii verific, mut blocul si actualizez podul daca e switch
        neighbor((X1, Y1), Move, (XNext1, YNext1)),
        neighbor((X2, Y2), Move, (XNext2, YNext2)),
        both_positions_valid(S, [(XNext1, YNext1), (XNext2, YNext2)]),
        update_state(S, [(X1, Y1), (X2, Y2)], [(XNext1, YNext1), (XNext2, YNext2)], S1),
        (   is_switch(S1, (XNext1, YNext1), laying, Func, Positions),
            update_bridge(S1, Func, Positions, SNext)
        ;   is_switch(S1, (XNext2, YNext2), laying, Func, Positions),
            update_bridge(S1, Func, Positions, SNext)
        ;   SNext = S1
        )
    ).

% gaseste vecinul blocului pentru miscarea verticala
vertical_move(Move, X1, Y1, X2, Y2, XNext, YNext) :-
    (   % daca miscarea e in sus, iau vecinul pozitiei care e mai sus
        Move = 'u' -> (Y1 =< Y2 -> (neighbor((X1, Y1), Move, (XNext, YNext))); (neighbor((X2, Y2), Move, (XNext, YNext))));
        % daca miscarea e in jos, iau vecinul pozitiei care e mai jos
        Move = 'd' -> (Y1 =< Y2 -> (neighbor((X2, Y2), Move, (XNext, YNext))); (neighbor((X1, Y1), Move, (XNext, YNext))))
    ).

% gaseste vecinul blocului pentru miscarea orizontala
horizontal_move(Move, X1, Y1, X2, Y2, XNext, YNext) :-
    (   % daca miscarea e la stanga, iau vecinul pozitiei care e mai la stanga
        Move = 'l' -> (X1 =< X2 -> (neighbor((X1, Y1), Move, (XNext, YNext))); (neighbor((X2, Y2), Move, (XNext, YNext))));
        % daca miscarea e la dreapta, iau vecinul pozitiei care e mai la dreapta
        Move = 'r' -> (X1 =< X2 -> (neighbor((X2, Y2), Move, (XNext, YNext))); (neighbor((X1, Y1), Move, (XNext, YNext))))
    ).

% verifica daca Pos1 si Pos2 sunt ambele sau niciuna fragile
both_positions_valid(S, [Pos1, Pos2]) :-
    get_cell(S, Pos1, Type1),
    get_cell(S, Pos2, Type2),
    (Type1 = fragile, Type2 = fragile ; Type1 \= fragile, Type2 \= fragile).

% actualizeaza starea prin stergerea pozitiei vechi a blocului si adaugarea celei noi
update_state(S, Pos, PosNext, SNext) :-
    delete(S, (block, Pos), S1),
    SNext = [(block, PosNext)|S1].

% verifica daca la pozitia Pos se afla un switch care poate fi activat
is_switch(S, Pos, BlockState, Func, Positions) :-
    member((Switch, Func, Positions, Pos), S),
    (   (Switch = oswitch)
    ;   (Switch = xswitch, BlockState = standing)
    ).

% actualizeaza starea podului in functie de functia switch-ului
update_bridge(S, Func, Positions, SNew) :-
    % daca functia switch-ului este 'uponly', podul este activat
    (   Func = uponly, update_positions(S, Positions, tile, SNew)
    % daca functia switch-ului este 'dnonly', podul este dezactivat
    ;   Func = dnonly, update_positions(S, Positions, blank, SNew)
    % daca functia switch-ului este 'switch', podul este comutat
    ;   Func = switch, toggle_positions(S, Positions, SNew)
    ).

% actualizeaza starea podului la pozitiile date
update_positions(S, [], _, S).
update_positions(S, [Pos|Positions], Type, SNew) :-
    (   Type = tile, delete(S, (_, Pos), S1), S2 = [(Type, Pos)|S1]
    ;   Type = blank, delete(S, (_, Pos), S1), S2 = S1
    ),
    update_positions(S2, Positions, Type, SNew).

% comuta starea podului de la pozitiile date
toggle_positions(S, [], S).
toggle_positions(S, [Pos|Positions], SNew) :-
    (   member((tile, Pos), S), delete(S, (tile, Pos), S1)
    ;   \+ member((tile, Pos), S), S1 = [(tile, Pos)|S]
    ),
    toggle_positions(S1, Positions, SNew).



%% TODO
% is_final/1
% is_final(S)
% Întoarce adevărat dacă în starea S blocul este în picioare, pe aceeași
% poziție cu gaura (scopul).
is_final(S) :-
    get_b_pos(S, BlockPos),
    member((target, BlockPos), S).



%%%%%%%%%% Etapa 2

%% TODO
% set_switch/6
% set_switch(+S, +Pos, +Switch, +Func, +Positions, -SNew)
% Leagă starea SNew la o stare cu aceleași informații ca și S, și în
% plus un switch la poziția Pos, cu parametrii dați.
%
% Switch: oswitch sau xswitch.
% Func: switch, uponly sau dnonly.
% Positions: pozițiile podului.
set_switch(S, Pos, Switch, Func, Positions, [(Switch, Func, Positions, Pos)|S]).



%% TODO
% solve/2
% solve(+S, -Moves)
% Solve găsește o soluție pentru problema din starea S. Soluția este
% reprezentată ca secvența de mutări Moves.
%
% Pentru a fi soluție, mutările din Moves trebuie să ducă blocul în
% picioare pe poziția scop (target).
solve(S, Moves) :-
    (   
        get_b_pos(S, BlockPos),
        member((xswitch, _, _, _), S),
        block_search(S, [BlockPos], [], Moves)
    );
    state_search(S, [S], [], Moves).

% cazul final
state_search(S, Visited, Moves, MovesRes) :-
    is_final(S),
    reverse(Moves, MovesRes).

% cautare in adancime
state_search(S, Visited, Moves, MovesRes) :-
    % genereaza toate mutarile posibile
    findall((D, M), (member(M, [u, d, l, r]), move(S, M, SNext), \+ member(SNext, Visited), distance(SNext, D)), DMs),
    % sorteaza mutarile dupa distanta Manhattan
    msort(DMs, SortedDMs),
    % alege mutarea cu cea mai mica distanta Manhattan
    member((_, Move), SortedDMs),
    move(S, Move, SNext),
    state_search(SNext, [SNext|Visited], [Move|Moves], MovesRes).

% cazul final
block_search(S, Visited, Moves, MovesRes) :-
    is_final(S),
    reverse(Moves, MovesRes).

% cautare in adancime
block_search(S, Visited, Moves, MovesRes) :-
    % genereaza toate mutarile posibile
    findall((D, M), (member(M, [u, d, l, r]), move(S, M, SNext), get_b_pos(SNext, BlockPos), \+ member(BlockPos, Visited), distance(SNext, D)), DMs),
    % sorteaza mutarile dupa distanta Manhattan
    msort(DMs, SortedDMs),
    % alege mutarea cu cea mai mica distanta Manhattan
    member((_, Move), SortedDMs),
    move(S, Move, SNext),
    get_b_pos(SNext, BlockPos),
    block_search(SNext, [BlockPos|Visited], [Move|Moves], MovesRes).

% calculeaza distanta Manhattan a blocului fata de gaura
distance(S, Dist) :-
    get_b_pos(S, BlockPos),
    member((target, TargetPos), S),
    (   % daca blocul e in picioare
        BlockPos = (X1, Y1),
        TargetPos = (X2, Y2),
        manhattan_distance((X1, Y1), TargetPos, Dist)
    ;   % daca blocul e culcat
        BlockPos = [(X1, Y1), (X3, Y3)],
        TargetPos = (X2, Y2),
        manhattan_distance((X1, Y1), TargetPos, D1),
        manhattan_distance((X3, Y3), TargetPos, D2),
        Dist is min(D1, D2)
    ).

% calculeaza distanta Manhattan intre doua puncte
manhattan_distance((X1, Y1), (X2, Y2), Dist) :-
    Dist is abs(X1 - X2) + abs(Y1 - Y2).	
