#ifndef _HEX_H
#define _HEX_H

#include <SDL/SDL.h>
#include <SDL/SDL_ttf.h>

enum Content_ {
    VIDE,
    OBSTACLE,
    EGOUT,
    LAMPE,
    SORTIE,
    BARRAGE,
    MAIGRET,
    POIROT,
    LUMIERE,
    LEGOUTIER,
    GERBER,
    CRUCHOT,
    FREUD,
    DISCRETE
};

typedef enum Content_ Content;

struct Hex_ {
    Content content;
    int label;
    int id;
    int seen;
};

typedef struct Hex_ Hex;

// extern short a, b, c;

void draw_hex(SDL_Surface *screen, Hex c, int i, int j, TTF_Font *font);
Hex string_to_hex(char *content, int label, int id, int seen);
Hex lampe(int label);
Hex egout(int label);

#endif
