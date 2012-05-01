:- include('plateau').
:- dynamic(case\3).

%% new_position(+X:integer, +Y:integer, +Direction:atom 
%%    -NewX:integer, -NewY:integer) is det. 
%
% Computes the new (X, Y) for the given direction
%
new_position(X, Y, n, NewX, NewY) :-
    NewX is X + 1,
    NewY is Y.
new_position(X, Y, s, NewX, NewY) :-
    NewX is X - 1,
    NewY is Y.
new_position(X, Y, nw, NewX, NewY) :-
    NewX is X,
    NewY is Y - 1.
new_position(X, Y, se, NewX, NewY) :-
    NewX is X,
    NewY is Y + 1.
new_position(X, Y, sw, NewX, NewY) :-
    NewX is X - 1,
    NewY is Y - 1.
new_position(X, Y, ne, NewX, NewY) :-
    NewX is X + 1,
    NewY is Y + 1.

%% street_case(+X:integer, +Y:integer) is semidet. 
%
% Succeeds if the content at (X, Y) is not an obstacle
% (either empty "vide" or uncovered manhole "egout(0)").
%
street_case(X, Y) :-
    (   case(X, Y, vide)
    ;   case(X, Y, egout(0))
    ).


%% can_move(+Character:term, +Direction:atom) is semidet. 
%
% Succeeds if the given can be moved in the given direction.
%
can_move(Character, Direction) :-
    case(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    street_case(NewX, NewY).
    
%% move(+Character:term, +Direction:atom) is det. 
%
% Moves Character in the given direction.
%    
move(Character, Direction) :-
    case(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY)
    case(NewX, NewY, Content),
    /* switch character and content */
    retract(case(X, Y, Character)),
    retract(case(NewX, NewY, Content)),
    asserta(case(NewX, NewY, Character)),
    asserta(case(X, Y, Content)).
  
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
    Y is MY,
    X > MX.
illuminated_by_lantern_aux(X, Y, MX, MY, s) :-
    Y is MY,
    X < MX.
illuminated_by_lantern_aux(X, Y, MX, MY, nw) :-
    X is MX,
    Y < MY.
illuminated_by_lantern_aux(X, Y, MX, MY, se) :-
    X is MX,
    Y > MY.
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

%% decrement_lamp_counter(+X:integer, +Y:integer, +N:integer) is det
%
% Decrements the counter of the lamp located at (X, Y).
%    
decrement_lamp_counter(X, Y, N) :-
    retract(case(X, Y, lampe(N))),
    NewN is N - 1
    asserta(case(NewX, Y, lampe(NewN))).

%% decrement_lamp_counter() is det. 
%
% Decrements the counter of all lit lamps.
%
decrement_lamp_counter :-
    forall(lit_lamp(X, Y, N), decrement_lamp_counter(X, Y, N)).



    


