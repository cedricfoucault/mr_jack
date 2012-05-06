:- include('plateau').
:- foreign(init).
:- foreign(quit).
:- foreign(draw_board).
:- foreign(show_screen).
:- foreign(lock_board).
:- foreign(unlock_board).
:- foreign(set_board_hex(+string, +integer, +integer, +integer)).
:- foreign(finish_set).
:- dynamic(is_jack/1).


new_game :-
    randomize,
    choose_jack,
    play_turn(1).
    
play_turn(N) :-
    /* odd-numbered turn */
    (N mod 2) =:= 1,
    format('Tour ~d\n', [N]),
    choose_characters(ChosenOnes, Others),
    format('Personnages jouables : ~a, ~a, ~a, ~a\n', ChosenOnes),
    write('— INSPECTEUR : Quel personnage voulez-vous jouer ?\n'),
    choose(ChosenOnes, C1Chosen, Rest1),
    play(C1Chosen),
    format('Personnages jouables : ~a, ~a, ~a\n', Rest1),
    write('— JACK : Quel personnage voulez-vous jouer ?\n'),
    choose(Rest1, C2Chosen, Rest2),
    play(C2Chosen),
    format('Personnages jouables : ~a, ~a\n', Rest2),
    write('— JACK : Quel personnage voulez-vous jouer ?\n'),
    choose(Rest2, C3Chosen, [C4Chosen]),
    play(C3Chosen),
    format('— INSPECTEUR : Veuillez-jouer ~a\n', [C4Chosen]),
    play(C4Chosen),
    Next is N + 1,
    play_turn(Next, Others).

play_turn(N, Characters) :-
    /* even-numbered turn */
    (N mod 2) =:= 0,
    format('Tour ~d\n', [N]),
    format('Personnages jouables : ~a, ~a, ~a, ~a\n', Characters),
    write('— JACK : Quel personnage voulez-vous jouer ?\n'),
    choose(Characters, C1Chosen, Rest1),
    play(C1Chosen),
    format('Personnages jouables : ~a, ~a, ~a\n', Rest1),
    write('— INSPECTEUR : Quel personnage voulez-vous jouer ?\n'),
    choose(Rest1, C2Chosen, Rest2),
    play(C2Chosen),
    format('Personnages jouables : ~a, ~a\n', Rest2),
    write('— INSPECTEUR : Quel personnage voulez-vous jouer ?\n'),
    choose(Rest2, C3Chosen, [C4Chosen]),
    play(C3Chosen),
    format('— JACK : Veuillez-jouer ~a\n', [C4Chosen]),
    play(C4Chosen),
    (   N < 8 ->
        Next is N + 1,
        play_turn(Next)
    ;   true
    ).
    
choose(List, Character, Rest) :-
    read_atom(AtomRead),
    (   extract_from_list(List, AtomRead, Rest) ->
        /* character read found in list */
        Character = AtomRead
    ;   /* character not found */
        write('Je n\'ai pas compris, '), 
        write('merci de rentrer le nom du personnage en minuscules :\n'),
        choose(List, Character, Rest)
    ).
    
extract_from_list([Hd | Tl], Hd, Tl).
extract_from_list([Hd | Tl], Elem, [Hd | Rest]) :-
    extract_from_list(Tl, Elem, Rest).
    
play(_).

choose_jack :-
    random(1, 9, N),
    character_of_number(N, Character),
    asserta(is_jack(Character)).

nth_rest(1, [Hd | Tl], Hd, Tl).
nth_rest(N, [Hd | Tl], Elem, [Hd | Rest]) :-
    N > 1,
    Next is N - 1,
    nth_rest(Next, Tl, Elem, Rest).
    
choose_characters([C1, C2, C3, C4], Rest) :-
    /* 4 x [1...8] == 4 x 3bits == 12 bits == 2^12 == 4 096*/
/*    random(1, 4096, N),*/
    characters(CList),
    random(1, 9, N1),
    nth_rest(N1, CList, Rest1),
    random(1, 8, N2),
    nth_rest(N2, Rest1, Rest2),
    random(1, 7, N3),
    nth_rest(N3, Rest2, Rest3),
    random(1, 6, N4),
    nth_rest(N4, Rest3, _),
    character_of_number(N1, C1),
    character_of_number(N2, C2),
    character_of_number(N3, C3),
    character_of_number(N4, C4),
    findall(X, 
        (character(X), X \= C1, X \= C2, X \= C3, X \= C4),
        Rest
    ).

character_of_number(1, maigret).
character_of_number(2, poirot).
character_of_number(3, lumiere).
character_of_number(4, legoutier).
character_of_number(5, gerber).
character_of_number(6, cruchot).
character_of_number(7, freud).
character_of_number(8, discrete).

%% new_position(+X:integer, +Y:integer, +Direction:atom 
%%    -NewX:integer, -NewY:integer) is det. 
%
% Computes the new (X, Y) for the given direction
%
new_position(X, Y, n, NewX, NewY) :-
    NewX is X,
    NewY is Y + 1.
new_position(X, Y, s, NewX, NewY) :-
    NewX is X,
    NewY is Y - 1.
new_position(X, Y, nw, NewX, NewY) :-
    NewX is X - 1,
    NewY is Y.
new_position(X, Y, se, NewX, NewY) :-
    NewX is X + 1,
    NewY is Y.
new_position(X, Y, sw, NewX, NewY) :-
    NewX is X - 1,
    NewY is Y - 1.
new_position(X, Y, ne, NewX, NewY) :-
    NewX is X + 1,
    NewY is Y + 1.

%% street_hex(+X:integer, +Y:integer) is semidet. 
%
% Succeeds if there is no obstacle at (X, Y).
% (either empty "vide" or uncovered manhole "egout(0)").
%
street_hex(X, Y) :-
    (   case(X, Y, vide)
    ;   case(X, Y, egout(_))
    ).
free_hex(X, Y) :-
    (   case(X, Y, vide)
    ;   case(X, Y, obstacle)
    ;   case(X, Y, egout(_))
    ;   case(X, Y, lampe(_))
    ).

%% can_move(+Character:term, +Direction:atom) is semidet. 
%
% Succeeds if the given can be moved in the given direction.
%
can_move(discrete, Direction) :- !,
    case(X, Y, discrete),
    new_position(X, Y, Direction, NewX, NewY),
    free_hex(NewX, NewY).
can_move(Character, Direction) :-
    case(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    street_hex(NewX, NewY).

%% move(+Character:term, +Direction:atom) is det. 
%
% Moves Character in the given direction.
%    
move(Character, Direction) :-
    case(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    case(NewX, NewY, Content),
    /* switch character and content */
    retract(case(X, Y, Character)),
    retract(case(NewX, NewY, Content)),
    asserta(case(NewX, NewY, Character)),
    asserta(case(X, Y, Content)).

%% witness is semidet
%
% Succeeds if there is a witness on the current board 
% (i.e. Jack is seen by someone).
%   
witness :-
    is_jack(Character),
    is_seen(Character).
  
%% update_lamp_counter() is det. 
%
% Decrements the counter of all lit lamps.
%
update_lamp_counter :-
    forall(lit_lamp(X, Y, N), update_lamp_counter(X, Y, N)).
    
update_lamp_counter(X, Y, N) :-
    retract(case(X, Y, lampe(N))),
    NewN is N - 1,
    asserta(case(X, Y, lampe(NewN))).  

%% update_turn_counter() is det. 
%
% Decrements the turn counter of the current game.
%
update_turn_counter :-
    turn(C),
    NewC is C - 1,
    retract(turn(C)),
    asserta(turn(NewC)).
 
%% character(?Content:term) is semidet
%
% Succeeds if Content is a character.
%  
character(maigret).
character(poirot).
character(lumiere).
character(legoutier).
character(gerber).
character(cruchot).
character(freud).
character(discrete).

characters([maigret, poirot, lumiere, legoutier, gerber, cruchot, freud, discrete]).

%% adjacent(?Content1:term, ?Content2:term) is nondet
%
% Succeeds if Content1 and Content2 are adjacent.
%
adjacent(Content1, Content2) :-
    case(X1, Y1, Content1),
    case(X2, Y2, Content2),
    (   X1 is X2 ->
        (   Y1 is Y2 + 1
        ;   Y1 is Y2 - 1
        )
    ;   X1 is X2 + 1 ->
        (   Y1 is Y2
        ;   Y1 is Y2 + 1
        )
    ;   X1 is X2 - 1 ->
        (   Y1 is Y2
        ;   Y1 is Y2 - 1
        )
    ).

%% lit_lamp(?X:integer, ?Y:integer, ?N:integer) is semidet
%
% Succeeds if there is "lampe(N)" at (X, Y), where N > 0.
%
lit_lamp(X, Y, N) :-
    case(X, Y, lampe(N)),
    N > 0.

%% lit_lamp(+Content:term) is semidet
%
% Succeeds if the given content is a lit lamp.
%
lit_lamp(lampe(N)) :-
    N > 0.

%% is_seen(+Character:term) is semidet
%
% Succeeds Character can be seen.
% There are two reasons for this:
% - The character is next to another character
% - The character stands in an illuminated hex 
%  (next to a lamp post, or illuminated by Maigret's lamp)
%
is_seen(Character) :-
    (   adjacent(Character, Content),
        (   character(Content) /* next to a character */
        ;   lit_lamp(Content) /* next to a lamp post */
        )
    ;   illuminated_by_lantern(Character) /* in Maigret's lantern path */
    ).
    
%% illuminated_by_lantern(+Character:term) is semidet
%
% Succeeds if the character is lit by Maigret's lantern.
%
illuminated_by_lantern(Character) :-
    case(X, Y, Character),
    case(MX, MY, maigret),
    lantern_direction(Direction),
    illuminated_by_lantern_aux(X, Y, MX, MY, Direction).

illuminated_by_lantern_aux(X, Y, MX, MY, n) :-
    X is MX,
    Y > MY.
illuminated_by_lantern_aux(X, Y, MX, MY, s) :-
    X is MX,
    Y < MY.
illuminated_by_lantern_aux(X, Y, MX, MY, nw) :-
    Y is MY,
    X < MX.
illuminated_by_lantern_aux(X, Y, MX, MY, se) :-
    Y is MY,
    X > MX.
illuminated_by_lantern_aux(X, Y, MX, MY, sw) :-
    DiffX is X - MX,
    DiffY is Y - MY,
    DiffX is DiffY,
    DiffX < 0.
illuminated_by_lantern_aux(X, Y, MX, MY, ne) :-
    DiffX is X - MX,
    DiffY is Y - MY,
    DiffX is DiffY,
    DiffX > 0.
    
update_board :- 
    lock_board,
    forall(case(I, J, Content), set_board(Content, I, J)),
    finish_set,
    unlock_board.

set_board(vide, I, J) :- set_board_hex('vide', 0, I, J).
set_board(obstacle, I, J) :- set_board_hex('obstacle', 0, I, J).
set_board(egout(Label), I, J) :- set_board_hex('egout', Label, I, J).
set_board(lampe(Label), I, J) :- set_board_hex('lampe', Label, I, J).
set_board(maigret, I, J) :- set_board_hex('maigret', 0, I, J).
set_board(poirot, I, J) :- set_board_hex('poirot', 0, I, J).
set_board(lumiere, I, J) :- set_board_hex('lumiere', 0, I, J).
set_board(legoutier, I, J) :- set_board_hex('legoutier', 0, I, J).
set_board(gerber, I, J) :- set_board_hex('gerber', 0, I, J).
set_board(cruchot, I, J) :- set_board_hex('cruchot', 0, I, J).
set_board(freud, I, J) :- set_board_hex('freud', 0, I, J).
set_board(discrete, I, J) :- set_board_hex('discrete', 0, I, J).
