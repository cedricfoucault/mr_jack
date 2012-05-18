% case(X,Y,Contenu), 
% où X est la colonne, Y la "diagonale" de haut-gauche à bas-droite
% et Contenu peut valoir :
% vide,
% [lampe,N] (N nombre de tours avant extinction,
% 0 pour éteinte, 8 pour toujours allumée),
% [egout,B] (B = 1 si la plaque bloque l'accès et 0 sinon),
% [Perso,Suspect] (Perso est le nom du suspect,
% Suspect = 1 s'il n'est pas encore innocenté et 0 sinon).
% Au passage, on ajoute un paramètre à Hercule Maigret indiquant
% la direction de la lanterne,
% de 1 à 6 dans le sens des aiguilles d'une montre avec 1 -> vers le haut.
% sortie(X,Y,B) représente une case reliée à une sortie,
% en X,Y avec B = 1 si la sortie est barrée et 0 sinon.
:- dynamic(case/3).
:- dynamic(character/3).
:- dynamic(seen/1).
:- dynamic(sortie/3).
:- dynamic(lantern_direction/1).

case(1,1,vide).
case(2,1,vide).
case(1,2,vide).
case(2,2,lampe(1, 1)).
case(3,2,egout(1, 1)).
/*case(1,3,maigret).*/
case(1,3,vide). %% modif
case(2,3,vide).
case(2,4,obstacle). %% modif
case(2,5,obstacle). %% modif
case(3,3,vide).
case(4,3,vide).
case(4,4,obstacle). %% modif
case(5,3,vide).
case(6,3,vide).
case(6,4,obstacle). %% modif
case(7,3,vide).
case(8,3,egout(0, 2)).
case(1,4,vide).
case(3,4,vide).
case(3,5,obstacle). %% modif
case(5,4,vide).
case(7,4,vide).
case(8,4,lampe(0, 2)).
case(8,5,obstacle). %% modif
/*case(9,4,discrete).*/
case(9,4,vide). %% modif
case(1,5,egout(0, 3)).
case(4,5,vide).
case(4,6,obstacle). %% modif
/*case(5,5,gerber).*/
case(5,5,vide). %% modif
case(6,5,lampe(8, 3)).
case(6,6,obstacle). %% modif
/*case(7,5,poirot).*/
case(7,5,vide). %% modif
case(9,5,vide).
case(2,6,vide).
case(3,6,vide).
case(5,6,vide).
case(7,6,vide).
case(8,6,egout(0, 4)).
case(8,7,obstacle). %% modif
case(9,6,vide).
case(10,5,obstacle). %% modif
case(10,6,vide).
case(10,7,obstacle). %% modif
case(11,6,lampe(4, 4)).
case(12,6,vide).
case(2,7,vide).
case(3,7,lampe(3, 5)).
case(4,7,vide).
case(4,8,obstacle). %% modif
case(5,7,vide).
case(6,7,egout(0, 5)).
case(6,8,obstacle). %% modif
case(7,7,vide).
case(9,7,vide).
case(11,7,vide).
case(11,8,obstacle). %% modif
case(12,7,vide).
case(12,8,obstacle). %% modif
case(12,9,obstacle). %% modif
case(5,8,vide).
/*case(7,8,lumiere).*/
case(7,8,vide). %% modif
case(8,8,lampe(8, 6)).
case(8,9,obstacle). %% modif
/*case(9,8,legoutier).*/
case(9,8,vide). %% modif
case(10,8,vide).
case(10,9,obstacle). %% modif
case(13,8,vide).
/*case(5,9,freud).*/
case(5,9,vide). %% modif
case(6,9,lampe(0, 7)).
case(7,9,vide).
case(9,9,vide).
case(11,9,vide).
case(13,9,egout(0, 6)).
case(6,10,egout(0, 7)).
case(7,10,vide).
case(8,10,vide).
case(9,10,vide).
case(10,10,vide).
case(11,10,vide).
case(12,10,vide).
/*case(13,10,cruchot).*/
case(13,10,vide). %% modif
case(11,11,vide).
case(12,11,lampe(2, 8)).
case(13,11,vide).
case(12,12,egout(1, 8)).
case(13,12,vide).
case(2,0,barrage(1)).
/*sortie(12,6,0).*/
case(12,6,sortie(1)).
/*sortie(2,7,0).*/
case(1,7,sortie(2)).
/*sortie(12,12,1).*/
case(12,13,barrage(2)).
/*sortie(2,1,1).*/

character(1,3,maigret).
character(9,4,discrete).
character(5,5,gerber).
character(7,5,poirot).
character(7,8,lumiere).
character(9,8,legoutier).
character(5,9,freud).
character(13,10,cruchot).

/* The turn counter for the game */
/*turn(0).*/

/* Maigret's lantern direction */
lantern_direction(n).
