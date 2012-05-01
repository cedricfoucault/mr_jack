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

case(1,1,vide).
case(2,1,vide).
case(1,2,vide).
case(2,2,lampe(1)).
case(3,2,egout(1)).
case(1,3,maigret).
case(2,3,vide).
case(3,3,vide).
case(4,3,vide).
case(5,3,vide).
case(6,3,vide).
case(7,3,vide).
case(8,3,egout(0)).
case(1,4,vide).
case(3,4,vide).
case(5,4,vide).
case(7,4,vide).
case(8,4,lampe(0)).
case(9,4,discrete).
case(1,5,egout(0)).
case(4,5,vide).
case(5,5,gerber).
case(6,5,lampe(8)).
case(7,5,poirot).
case(9,5,vide).
case(2,6,vide).
case(3,6,vide).
case(5,6,vide).
case(7,6,vide).
case(8,6,egout(0)).
case(9,6,vide).
case(10,6,vide).
case(11,6,lampe(4)).
case(12,6,vide).
case(2,7,vide).
case(3,7,lampe(3)).
case(4,7,vide).
case(5,7,vide).
case(6,7,egout(0)).
case(7,7,vide).
case(9,7,vide).
case(11,7,vide).
case(12,7,vide).
case(5,8,vide).
case(7,8,lumiere).
case(8,8,lampe(8)).
case(9,8,legoutier).
case(10,8,vide).
case(13,8,vide).
case(5,9,freud).
case(6,9,lampe(0)).
case(7,9,vide).
case(9,9,vide).
case(11,9,vide).
case(13,9,egout(0)).
case(6,10,egout(0)).
case(7,10,vide).
case(8,10,vide).
case(9,10,vide).
case(10,10,vide).
case(11,10,vide).
case(12,10,vide).
case(13,10,cruchot).
case(11,11,vide).
case(12,11,lampe(2)).
case(13,11,vide).
case(12,12,egout(1)).
case(13,12,vide).

seen(maigret).
seen(poirot).
seen(lumiere).
seen(legoutier).
seen(gerber).
seen(cruchot).
seen(freud).
seen(discrete).

/* Maigret's lantern direction */
lantern_direction(up).

sortie(2,1,1).
sortie(9,6,0).
sortie(2,7,0).
sortie(12,12,1).
