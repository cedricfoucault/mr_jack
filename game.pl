:- include('plateau').
:- foreign(init, [return(none)]).
:- foreign(quit, [return(none)]).
:- foreign(draw_board, [return(none)]).
:- foreign(show_screen, [return(none)]).
/*:- foreign(lock_board).*/
/*:- foreign(unlock_board).*/
:- foreign(
    set_board_hex(+string, +integer, +integer, +boolean, +integer, +integer),
    [return(none)]
    ).
:- foreign(finish_set, [return(none)]).
:- dynamic(is_jack/1).
:- dynamic(innocent/1).
:- dynamic(alibi/1).
:- dynamic(witness_card/1).

new_game :-
    init,
    randomize,
    choose_jack,
    catch(play_turn(1), 'fin', fin)/* ; fin*/.
    
choose_jack :-
    random(1, 9, N),
    character_of_number(N, Character),
    asserta(is_jack(Character)),
    findall(C, (character(C), \+ is_jack(C)), AlibiList),
    asserta(alibi(AlibiList)).
    
play_turn(N) :-
    /* odd-numbered turn */
    (N mod 2) =:= 1,
    update_board,
    format('-------------------- TOUR ~d --------------------\n', [N]),
    declare_innocent,
    choose_characters(ChosenOnes, Others),
    format('Personnages jouables : ~a, ~a, ~a, ~a\n', ChosenOnes),
    write('— INSPECTEUR : Quel personnage voulez-vous jouer ? '),
    choose(ChosenOnes, C1Chosen, Rest1),
    play(C1Chosen),
    format('Personnages jouables : ~a, ~a, ~a\n', Rest1),
    write('— JACK : Quel personnage voulez-vous jouer ? '),
    choose(Rest1, C2Chosen, Rest2),
    play(C2Chosen),
    format('Personnages jouables : ~a, ~a\n', Rest2),
    write('— JACK : Quel personnage voulez-vous jouer ? '),
    choose(Rest2, C3Chosen, [C4Chosen]),
    play(C3Chosen),
    format('— INSPECTEUR : Veuillez-jouer ~a\n', [C4Chosen]),
    play(C4Chosen),
    call_for_witness,
    update_lamp_counter,
    Next is N + 1,
    play_turn(Next, Others).

play_turn(N, Characters) :-
    /* even-numbered turn */
    (N mod 2) =:= 0,
    update_board,
    format('-------------------- TOUR ~d --------------------\n', [N]),
    declare_innocent,
    format('Personnages jouables : ~a, ~a, ~a, ~a\n', Characters),
    write('— JACK : Quel personnage voulez-vous jouer ? '),
    choose(Characters, C1Chosen, Rest1),
    play(C1Chosen),
    format('Personnages jouables : ~a, ~a, ~a\n', Rest1),
    write('— INSPECTEUR : Quel personnage voulez-vous jouer ? '),
    choose(Rest1, C2Chosen, Rest2),
    play(C2Chosen),
    format('Personnages jouables : ~a, ~a\n', Rest2),
    write('— INSPECTEUR : Quel personnage voulez-vous jouer ? '),
    choose(Rest2, C3Chosen, [C4Chosen]),
    play(C3Chosen),
    format('— JACK : Veuillez-jouer ~a\n', [C4Chosen]),
    play(C4Chosen),
    call_for_witness,
    update_lamp_counter,
    (   N < 8 ->
        Next is N + 1,
        play_turn(Next)
    ;   quit
    ).

fin :- 
    write('---------------------- FIN ----------------------\n'),
    quit.
    
play(Character) :-
    write('De combien de cases souhaitez-vous vous déplacer (1 - 3) ? '),
    read_integer(1, 3, N),
    character(X, Y, Character),
    ask_move(N, Character, X, Y).
/*    catch(ask_move(N, Character, X, Y), 'recommencer', (
        retractall(character(_, _, Character)),
        asserta(character(X, Y, Character))
        play(Character)
        )
    ).*/
    
declare_innocent :-
    write('Ont été innocentés :'),
    forall(innocent(Character), format(' ~a', [Character])),
    write('.\n').

ask_manhole(Character, N) :-
    write('Par quelle bouche d\'égouts voulez vous sortir ? nº'),
    read_integer(1, 8, N_),
    (   case(_, _, egout(0, N_)) ->
        N is N_
    ;   write('Ceci est une bouche d\'égouts fermée, '),
        write('veuillez saisir une bouche d\'égouts ouverte.\n'),
        ask_manhole(Character, N)
    ).

ask_move(1, Character, OldX, OldY) :-
    (   
        (   is_on_open_manhole(Character),
            write('Voulez-vous passer les égouts (oui/non) ? '),
            read_ouinon
        ) ->
        /* go through the sewers */
        ask_manhole(Character, N),
        (   can_last_move(Character, N, OldX, OldY) ->
            move(Character, N),
            update_seen_tags,
            update_board
        ;   format('~a ne peut pas aller sur cette case, ', [Character]),
            write('veuillez recommencer.\n'),
            ask_move(1, Character, OldX, OldY)  
        )
    ;   /* regular move */
        write('1 - Dans quelle direction souhaitez vous aller '),
        write('(e, ne, n, nw, w, sw, s, se) ? '),
        read_direction(Direction),
        (   is_exit_move(Character, Direction) ->
            exit
        ;   is_accusation_move(Character, Direction, AccusedChar) ->
            /* the move is an accusation */
            format('Êtes vous-sûr de vouloir accuser ~a (oui/non) ? ',
                        [AccusedChar]),
            (   read_ouinon ->
                /* oui */
                accuse(AccusedChar)
            ;   /* non */
                ask_move(N, Character, OldX, OldY)
            )
        ;   can_last_move(Character, Direction, OldX, OldY) ->
            move(Character, Direction),
            update_seen_tags,
            update_board
        ;   /* can't move in this direction */
            format('~a ne peut pas aller sur cette case, ', [Character]),
            write('veuillez recommencer.\n'),
            ask_move(1, Character, OldX, OldY)
        )
    ).
ask_move(N, Character, OldX, OldY) :-
    N > 1,
    (
        (   is_on_open_manhole(Character),
            write('Voulez-vous passer les égouts (oui/non) ? '),
            read_ouinon
        ) ->
        /* go through the sewers */
        ask_manhole(Character, ManholeId),
        (   can_last_move(Character, ManholeId, OldX, OldY) ->
            move(Character, ManholeId),
            update_seen_tags,
            update_board,
            Next is N - 1,
            ask_move(Next, Character, OldX, OldY)
        ;   format('~a ne peut pas aller sur cette case, ', [Character]),
            write('veuillez recommencer.\n'),
            ask_move(N, Character, OldX, OldY)  
        )
    ;   /* regular move */
        format('~d - Dans quelle direction souhaitez vous aller ', [N]),
        write('(e, ne, n, nw, w, sw, s, se) ? '),
        read_direction(Direction),
        (   can_move(Character, Direction) ->
            move(Character, Direction),
            update_seen_tags,
            update_board,
            Next is N - 1,
            ask_move(Next, Character, OldX, OldY)
        ;   /* can't move in this direction */
            format('~a ne peut pas aller sur cette case, ', [Character]),
            write('veuillez recommencer.\n'),
            ask_move(N, Character, OldX, OldY)
        )
    ).
    
ask_move_cruchot(1, Character, OldX, OldY) :-
    write('1 - Dans quelle direction souhaitez le déplacer'),
    write('(e, ne, n, nw, w, sw, s, se) ? '),
    read_direction(Direction),
    (   can_be_last_moved(Character, Direction, OldX, OldY) ->
        move(Character, Direction),
        update_seen_tags,
        update_board
    ;   /* can't move in this direction */
        format('~a ne peut pas être affecté à cette position, ', [Character]),
        write('veuillez recommencer.\n'),
        ask_move_cruchot(1, Character, OldX, OldY)
    ).
ask_move_cruchot(N, Character, OldX, OldY) :-
    N > 1,
    format('~d - Dans quelle direction souhaitez le déplacer ', [N]),
    write('(e, ne, n, nw, w, sw, s, se) ? '),
    read_direction(Direction),
    (   can_be_moved(Character, Direction) ->
        move(Character, Direction),
        update_seen_tags,
        update_board,
        Next is N - 1,
        ask_move_cruchot(Next, Character, OldX, OldY)
    ;   /* can't move in this direction */
        format('~a ne peut pas être déplacé sur cette case, ', [Character]),
        write('veuillez recommencer.\n'),
        ask_move_cruchot(N, Character, OldX, OldY)
    ).
    
special_ability(poirot) :-
    write('Capacité spéciale :\n'),
    draw_alibi(Character),
    format('~a a un alibi : il est innocent !\n', [Character]).

special_ability(maigret) :-
    write('Choisissez la direction éclairée par la lanterne de maigret '),
    write('(e, ne, n, nw, sw, s, se) : '),
    read_direction(Direction),
    retract(lantern_direction(_)),
    asserta(lantern_direction(Direction)).
    
special_ability(lumiere).

special_ability(gerber) :-
    write('Capacité spéciale : bouger un barrage de police.\n'),
    write('Veuillez échanger une sortie avec un barrage de police :\n'),
    write('sortie (1/2) ? '),
    read_integer(1, 2, NS),
    write('barrage (1/2) ? '),
    read_integer(1, 2, NC),
    case(XS, YS, sortie(NS)),
    case(XC, YC, barrage(NC)),
    retract(case(XS, YS, sortie(NS))),
    asserta(case(XC, YC, sortie(NS))),
    retract(case(XC, YC, barrage(NC))),
    asserta(case(XS, YS, barrage(NC))).    
    
special_ability(cruchot) :-
    write('Capacité spéciale : sifflet !\n'),
    write('Choisissez un personnage à déplacer : '),
    read_character(Character1),
    write('De combien de cases souhaitez-vous le déplacer (1 - 3) ? '),
    read_integer(1, 3, N1),
    character(OldX1, OldY1, Character1),
    ask_move_cruchot(N1, Character1, OldX1, OldY1),
    Nleft is 3 - N1,
    (   Nleft =:= 1
    ;   write('Choisissez un personnage à déplacer : '),
        read_character(Character2),
        format('De combien de cases souhaitez-vous le déplacer (1 - ~d) ? ',
            [Nleft]),
        read_integer(1, Nleft, N2),
        character(OldX2, OldY2, Character2),
        ask_move_cruchot(N2, Character2, OldX2, OldY2),
        Nleft2 is 3 - N2,
        (   Nleft2 =:= Nleft - N2
        ;   write('Choisissez un personnage à déplacer : '),
            read_character(Character3),
            character(OldX3, OldY3, Character3),
            ask_move_cruchot(1, Character3, OldX3, OldY3)
        )
    ).
    
special_ability(freud) :-
    write('Voulez-vous utiliser la capacité spéciale de freud : '),
    write('échange de position (oui/non) ? '),
    character(X1, Y1, freud),
    (   read_ouinon ->
        write('Avec qui voulez-vous que freud échange de position ? '),
        read_character(Character),
        character(X2, Y2, Character),
        retract(character(X1, Y1, freud)),
        asserta(character(X2, Y2, freud)),
        retract(character(X2, Y2, Character)),
        asserta(character(X1, Y1, Character))
    ;   ask_move(3, freud, X1, Y1)
    ).
    
exit :-
    write('Jack quitte le quartier !!!\nBravo Jack !\n'),
    throw('fin').
    
accuse(Character) :- 
    format('L\'enquêteur accuse ~a !!!\n', [Character]),
    /* supsens */
    sleep(1), write('.'), flush_output,
    sleep(1), write('.'), flush_output,
    sleep(1), write('.'), flush_output,
    sleep(2),
    (   is_jack(Character) ->
        write('Jack est découvert !!!\n'), flush_output,
        sleep(0.5),
        write('Bravo Inspecteur !\n'), flush_output,
        sleep(1)
    ;   write('Non !! L\'accusé est innocent !\n'), flush_output,
        sleep(0.5),
        write('Bravo Jack !\n'), flush_output,
        sleep(1)
    ),
    throw('fin').

read_integer(Nmin, Nmax, N) :-
    read_token(Nread),
    (   (integer(Nread), Nread >= Nmin, Nread =< Nmax) ->
        N is Nread
    ;   Nread = q ->
        write('quitter…\n'),
/*        quit,*/
        throw('fin')
    ;   Nread = r ->
        write('recommencer\n'),
        throw('recommencer')
    ;   format('Je n\'ai pas compris, entrez un nombre de ~d à ~d : ',
            [Nmin, Nmax]),
        read_integer(Nmin, Nmax, N)
    ).

read_direction(Direction) :-
    read_token(A),
    (   (   atom(A), 
            (A = e; A = ne; A = n; A = nw; A = w; A = sw; A = s; A = se)
        ) ->
        Direction = A
    ;   A = q ->
        write('quitter…\n'),
/*        quit,*/
        throw('fin')
    ;   A = r ->
        write('recommencer\n'),
        throw('recommencer')
    ;   /* token written isn't a direction */
        write('Je n\'ai pas compris, '), 
        write('entrez la direction choisie '),
        write('(e, ne, n, nw, w, sw, s, se) : '),
        read_direction(Direction)
    ).
    
read_character(Character) :-
    read_atom(A),
    (   character(A) ->
        Character = A
    ;   A = q ->
        write('quitter…\n'),
        throw('fin')
    ;   A = r ->
        write('recommencer\n'),
        throw('recommencer')
    ;   /* not a character */
        write('Je n\'ai pas compris, '), 
        write('merci de rentrer le nom du personnage en minuscules :\n'),
        read_character(Character)
    ).

read_ouinon :-
    read_atom(A),
    (   A = oui ->
        true
    ;   A = non ->
        fail
    ;   write('Je n\'ai pas compris, merci d\'entrer "oui" ou "non" : '), 
        read_ouinon
    ).

choose(List, Character, Rest) :-
    read_token(AtomRead),
    (   (atom(AtomRead), extract_from_list(List, AtomRead, Rest)) ->
        /* character read found in list */
        Character = AtomRead
    ;   AtomRead = q ->
        write('quitter…\n'),
/*        quit,*/
        throw('fin')
    ;   /* character not found */
        write('Je n\'ai pas compris, '), 
        write('merci de rentrer le nom du personnage en minuscules :\n'),
        choose(List, Character, Rest)
    ).

distance(X1, Y1, X2, Y2, D) :-
    /* distance formula between two points in a hex grid: */
    /* if (sign(dx) == sign(dy)) then max(abs(dx), abs(dy)) */
    /* else abs(dx) + abs(dy) */
    DX is X1 - X2,
    DY is Y1 - Y2,
    abs(DX, DXA),
    abs(DY, DYA),
    sign(DX, SX),
    sign(DY, SY),
    (   SX =:= SY ->
        max(DXA, DYA, D)
    ;   D is DXA + DYA
    ).
abs(X, XA) :-
    (   X > 0 ->
        XA is X
    ;   XA is  -X
    ).
sign(X, SX) :-
    (   X > 0 ->
        SX is 1
    ;   X =:= 0 ->
        SX is 0
    ;   SX is -1

    ).
max(D1, D2, D) :-
    (   D1 > D2 ->
        D is D1
    ;   D is D2
    ).
    

extract_from_list([Hd | Tl], Hd, Tl).
extract_from_list([Hd | Tl], Elem, [Hd | Rest]) :-
    extract_from_list(Tl, Elem, Rest).

draw_alibi(Character) :-
    alibi(AlibiList),
    length(AlibiList, Length),
    random(1, Length, N),
    extract_nth(N, AlibiList, Character, AlibiListRest),
    retract(alibi(AlibiList)),
    asserta(alibi(AlibiListRest)).

choose_characters([C1, C2, C3, C4], Rest) :-
    characters(CList),
    random(1, 9, N1),
    extract_nth(N1, CList, C1, Rest1),
    random(1, 8, N2),
    extract_nth(N2, Rest1, C2, Rest2),
    random(1, 7, N3),
    extract_nth(N3, Rest2, C3, Rest3),
    random(1, 6, N4),
    extract_nth(N4, Rest3, C4, _),
    findall(X, 
        (character(X), X \= C1, X \= C2, X \= C3, X \= C4),
        Rest
    ).

extract_nth(1, [Hd | Tl], Hd, Tl).
extract_nth(N, [Hd | Tl], Elem, [Hd | Rest]) :-
    N > 1,
    Next is N - 1,
    extract_nth(Next, Tl, Elem, Rest).

character_of_number(1, maigret).
character_of_number(2, poirot).
character_of_number(3, lumiere).
character_of_number(4, legoutier).
character_of_number(5, gerber).
character_of_number(6, cruchot).
character_of_number(7, freud).
character_of_number(8, discrete).

int_of_direction(n,  1).
int_of_direction(nw, 2).
int_of_direction(sw, 3).
int_of_direction(s,  4).
int_of_direction(se, 5).
int_of_direction(ne, 6).

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
new_position(_, _, N, NewX, NewY) :-
    /* go through the sewers */
    integer(N),
    case(NewX, NewY, egout(0, N)).

%% free_street_hex(+X:integer, +Y:integer) is semidet. 
%
% Succeeds if there is no obstacle at (X, Y).
% (either empty "vide" or uncovered manhole "egout(0)").
%
free_street_hex(X, Y) :-
    (   case(X, Y, vide)
    ;   case(X, Y, egout(_, _))
    ).
    
%% free_street_hex(+X:integer, +Y:integer) is semidet. 
%
% Succeeds if the hex (X, Y) is free.
%
free_hex(X, Y) :-
    case(X, Y, _).

%% is_accusation_move(+Character:term, +Direction:atom, -AccusedChar:atom) 
%%  is semidet. 
%
% Succeeds if the move correspond to a valid exit move of Jack.
% 
is_exit_move(Character, Direction) :-
    is_jack(Character),
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    case(NewX, NewY, sortie(_)).
    

%% is_accusation_move(+Character:term, +Direction:atom, -AccusedChar:atom) 
%%  is semidet. 
%
% Succeeds if the given direction means Character confronts AccusedChar.
%    
is_accusation_move(Character, Direction, AccusedChar) :-
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    character(NewX, NewY, AccusedChar).

%% can_move(+Character:term, +Direction:atom) is semidet. 
%
% Succeeds if the given can be moved in the given direction.
%
can_move(discrete, Direction) :- !,
    character(X, Y, discrete),
    new_position(X, Y, Direction, NewX, NewY),
    free_hex(NewX, NewY).
can_move(Character, Direction) :-
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    free_street_hex(NewX, NewY).

%% can_last_move(+Character:term, +Direction:atom, +OldX:integer, +OldY:integer) is semidet. 
%
% Succeeds if the given can be moved and set (last move)
% in the given direction.
%
can_last_move(Character, Direction, OldX, OldY) :-
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    /* the character must not come back to its previous position */
    (OldX =\= NewX ; OldY =\= NewY),
    /* there must be no one else on this hex */
    \+ character(NewX, NewY, _),
    /* the location must be a free street hex */
    free_street_hex(NewX, NewY).

/* when used by cruchot */
can_be_moved(Character, Direction) :-
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    free_street_hex(NewX, NewY).
can_be_last_moved(Character, Direction, OldX, OldY) :-
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    /* the character must not come back to its previous position */
    (OldX =\= NewX ; OldY =\= NewY),
    /* the character must be closer to cruchot than before */
    character(Xcruchot, Ycruchot, cruchot),
    distance(Xcruchot, Ycruchot, OldX, OldY, OldD),
    distance(Xcruchot, Ycruchot, NewX, NewY, NewD),
    NewD =< OldD,
    /* there must be no one else on this hex */
    \+ character(NewX, NewY, _),
    /* the location must be a free street hex */
    free_street_hex(NewX, NewY).

%% move(+Character:term, +Direction:atom) is det. 
%
% Moves Character in the given direction.
%    
move(Character, Direction) :-
    character(X, Y, Character),
    new_position(X, Y, Direction, NewX, NewY),
    retract(character(X, Y, Character)),
    asserta(character(NewX, NewY, Character)).

is_on_open_manhole(Character) :-
    character(X, Y, Character),
    case(X, Y, egout(0, _)).

call_for_witness :-
    write('Appel à Témoin\n'),
    flush_output,
    /* supsens */
    sleep(0.5), write('.'), flush_output,
    sleep(0.5), write('.'), flush_output,
    sleep(0.5), write('.'), flush_output,
    sleep(1),
/*    at_end_of_stream,
    get_char(_),*/
    (   witness ->
        /* there is a witness */
        write('Il n\'y a pas de témoin !\n'),
        write('Personnages innocentés :'),
        forall((seen(Character), not_innocent(Character)), (
                asserta(innocent(Character)),
                format(' ~a', [Character])
            )
        ),
        write('.\n'),
        /* put the witness card */
        (   witness_card(no) ->
            retract(witness_card(no)),
            asserta(witness_card(yes))
        ;   true
        )
    ;   /* there is no witness */
        write('Il y a un témoin !\n'),
        write('Personnages innocentés :'),
        forall((not_seen(Character), not_innocent(Character)), (
                asserta(innocent(Character)),
                format(' ~a', [Character])
            )
        ),
        write('.\n'),
        /* put the no witness card */
        (   witness_card(yes) ->
            retract(witness_card(yes)),
            asserta(witness_card(no))
        ; true
        )
    ).
    
/* the witness / no-witness card (updated each turn) */
witness_card(yes).

%% witness is semidet
%
% Succeeds if there is a witness on the current board 
% (i.e. Jack is seen by someone).
%   
witness :-
    is_jack(Character),
    seen(Character).
  
%% update_lamp_counter() is det. 
%
% Decrements the counter of all lit lamps.
%
update_lamp_counter :-
    forall(lit_lamp(X, Y, N), update_lamp_counter(X, Y, N)).
    
update_lamp_counter(X, Y, N) :-
    retract(case(X, Y, lampe(N, Id))),
    NewN is N - 1,
    asserta(case(X, Y, lampe(NewN, Id))).  

/*%% update_turn_counter() is det. 
%
% Decrements the turn counter of the current game.
%
update_turn_counter :-
    turn(C),
    NewC is C - 1,
    retract(turn(C)),
    asserta(turn(NewC)).*/
 
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

%% adjacent(+X1:integer, +Y1:integer, ?X2:integer, ?Y2:integer) is semidet
%
% Succeeds if the (X, Y) coordinates are adjacent.
%
adjacent(X1, Y1, X2, Y2) :-
    (   X2 is X1, 
        (   Y2 is Y1 + 1
        ;   Y2 is Y1 - 1
        )
    ;   X2 is X1 + 1,
        (   Y2 is Y1
        ;   Y2 is Y1 + 1
        )
    ;   X2 is X1 - 1,
        (   Y2 is Y1
        ;   Y2 is Y1 - 1
        )
    ).
/*    (   X2 is X1 ->
        (   Y2 is Y1 + 1
        ;   Y2 is Y1 - 1
        )
    ;   X2 is X1 + 1 ->
        (   Y2 is Y1
        ;   Y2 is Y1 + 1
        )
    ;   X2 is X1 - 1 ->
        (   Y2 is Y1
        ;   Y2 is Y1 - 1
        )
    ).*/

%% adjacent_character(+Character:atom) is nondet
%
% Succeeds the given character is next to another.
%
adjacent_character(Character) :-
    character(X1, Y1, Character),
    adjacent(X1, Y1, X2, Y2),
    character(X2, Y2, _).

%% adjacent_character(+Character:atom) is nondet
%
% Succeeds the given character is next to a lit lamp.
%   
adjacent_lit_lamp(Character) :-
    character(X1, Y1, Character),
    adjacent(X1, Y1, X2, Y2),
    lit_lamp(X2, Y2, _).
    

%% lit_lamp(?X:integer, ?Y:integer, ?N:integer) is semidet
%
% Succeeds if there is "lampe(N)" at (X, Y), where N > 0.
%
lit_lamp(X, Y, N) :-
    case(X, Y, lampe(N, _)),
    N > 0.

%% lit_lamp(+Content:term) is semidet
%
% Succeeds if the given content is a lit lamp.
%
lit_lamp(lampe(N, _)) :-
    N > 0.
    
update_seen_tags :-
    retractall(seen(_)),
    forall(character(Character),
        (   is_seen(Character) ->
            asserta(seen(Character))
        ;   true
        )
    ).

write_seen_characters :-
    forall(seen(Character), format(' ~a', [Character])).
    
not_seen(Character) :-
    character(Character),
    \+ seen(Character).

not_innocent(Character) :-
    character(Character),
    \+ innocent(Character).

%% is_seen(+Character:term) is semidet
%
% Succeeds Character can be seen.
% There are two reasons for this:
% - The character is next to another character
% - The character stands in an illuminated hex 
%  (next to a lamp post, or illuminated by Maigret's lamp)
%
is_seen(Character) :-
    (   adjacent_character(Character)
    ;   adjacent_lit_lamp(Character)
    ;   illuminated_by_lantern(Character) /* in Maigret's lantern path */
    ).
    
%% illuminated_by_lantern(+Character:term) is semidet
%
% Succeeds if the character is lit by Maigret's lantern.
%
illuminated_by_lantern(Character) :-
    character(X, Y, Character),
    character(MX, MY, maigret),
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

%% hex(+X:integer, +Y:integer, -Content:term) is det
%
% Gives the content at (X, Y) : 
% if there is a character at this position, it is returned
% else the 'case' content is returned ()
%
hex(X, Y, Content) :-
    case(X, Y, CaseContent),
    (   character(X, Y, Character) ->
        Content = Character
    ;   Content = CaseContent
    ).
    
seen_tag(Content, Tag) :-
    (   seen(Content) ->
        Tag = true
    ;   Tag = false
    ).

update_board :- 
    forall(
        (hex(I, J, Content), seen_tag(Content, Tag)),
        set_board(Content, Tag, I, J)
    ),
    finish_set,
    draw_board,
    show_screen.

set_board(vide, Seen, I, J) :-
    set_board_hex('vide', 0, 0, Seen, I, J).
set_board(obstacle, Seen, I, J) :-
    set_board_hex('obstacle', 0, 0, Seen, I, J).
set_board(egout(Label, Id), Seen, I, J) :-
    set_board_hex('egout', Label, Id, Seen, I, J).
set_board(lampe(Label, Id), Seen, I, J) :-
    set_board_hex('lampe', Label, Id, Seen, I, J).
set_board(sortie(Id), Seen, I, J) :-
    set_board_hex('sortie', 0, Id, Seen, I, J).
set_board(barrage(Id), Seen, I, J) :-
    set_board_hex('barrage', 0, Id, Seen, I, J).
set_board(poirot, Seen, I, J) :-
    set_board_hex('poirot', 0, 0, Seen, I, J).
set_board(lumiere, Seen, I, J) :-
    set_board_hex('lumiere', 0, 0, Seen, I, J).
set_board(legoutier, Seen, I, J) :-
    set_board_hex('legoutier', 0, 0, Seen, I, J).
set_board(gerber, Seen, I, J) :-
    set_board_hex('gerber', 0, 0, Seen, I, J).
set_board(cruchot, Seen, I, J) :-
    set_board_hex('cruchot', 0, 0, Seen, I, J).
set_board(freud, Seen, I, J) :-
    set_board_hex('freud', 0, 0, Seen, I, J).
set_board(discrete, Seen, I, J) :-
    set_board_hex('discrete', 0, 0, Seen, I, J).
set_board(maigret, Seen, I, J) :-
    lantern_direction(Direction),
    int_of_direction(Direction, Label),
    set_board_hex('maigret', Label, 0, Seen, I, J).

