#include <stdio.h>
#include <stdbool.h>
#include <SDL/SDL.h>
#include <SDL/SDL_gfxPrimitives.h>
#include <SDL/SDL_ttf.h>
#include "hex.h"

const int WINDOW_WIDTH = 1024;
const int WINDOW_HEIGHT = 768;
const char* WINDOW_TITLE = "SDL Start";


int main(int argc, char **argv)
{
    SDL_Init( SDL_INIT_VIDEO );
    TTF_Init();
    atexit(TTF_Quit);
    
    // load font
    TTF_Font *font;
    font = TTF_OpenFont("arial_black.ttf", 8);
    if(!font) {
        fprintf(stderr, "TTF_OpenFont: %s\n", TTF_GetError());
    }
    
    SDL_Surface* screen = SDL_SetVideoMode( WINDOW_WIDTH, WINDOW_HEIGHT, 0, 
        SDL_HWSURFACE | SDL_DOUBLEBUF );
    SDL_WM_SetCaption( WINDOW_TITLE, 0 );

    SDL_Event event;
    bool gameRunning = true;

    while (gameRunning)
    {
        if (SDL_PollEvent(&event))
        {
            if (event.type == SDL_QUIT)
            {
                gameRunning = false;
            }
        }
        
        // short c = 20;
        // short a = c / 2;
        // short b = (short) (0.866 * c);
        
        Hex vide = { VIDE };
        Hex obstacle = { OBSTACLE };
        // Hex egout_ouvert = { EGOUT, 0 };
        // Hex egout_ferme = { EGOUT, 1 };
        // Hex lampe_allumee = { LAMPE, 2 };
        // Hex lampe_eteinte = { LAMPE, 0 };
        Hex maigret = { MAIGRET };
        Hex poirot = { POIROT };
        Hex lumiere = { LUMIERE };
        Hex legoutier = { LEGOUTIER };
        Hex gerber = { GERBER };
        Hex cruchot = { CRUCHOT };
        Hex freud = { FREUD };
        Hex discrete = { DISCRETE };
        
        draw_hex(screen, 1,  1,  vide,      font);
        draw_hex(screen, 2,  1,  vide,      font);
        draw_hex(screen, 1,  2,  vide,      font);
        draw_hex(screen, 2,  2,  lampe(1),  font);
        draw_hex(screen, 3,  2,  egout(1),  font);
        draw_hex(screen, 1,  3,  maigret,   font);
        draw_hex(screen, 2,  3,  vide,      font);
        draw_hex(screen, 2,  4,  obstacle,  font); // modif
        draw_hex(screen, 2,  5,  obstacle,  font); // modif
        draw_hex(screen, 3,  3,  vide,      font);
        draw_hex(screen, 4,  3,  vide,      font);
        draw_hex(screen, 4,  4,  obstacle,  font); // modif
        draw_hex(screen, 5,  3,  vide,      font);
        draw_hex(screen, 6,  3,  vide,      font);
        draw_hex(screen, 6,  4,  obstacle,  font); // modif
        draw_hex(screen, 7,  3,  vide,      font);
        draw_hex(screen, 8,  3,  egout(0),  font);
        draw_hex(screen, 1,  4,  vide,      font);
        draw_hex(screen, 3,  4,  vide,      font);
        draw_hex(screen, 3,  5,  obstacle,  font); // modif
        draw_hex(screen, 5,  4,  vide,      font);
        draw_hex(screen, 7,  4,  vide,      font);
        draw_hex(screen, 8,  4,  lampe(0),  font);
        draw_hex(screen, 8,  5,  obstacle,  font); // modif
        draw_hex(screen, 9,  4,  discrete,  font);
        draw_hex(screen, 1,  5,  egout(0),  font);
        draw_hex(screen, 4,  5,  vide,      font);
        draw_hex(screen, 4,  6,  obstacle,  font); // modif
        draw_hex(screen, 5,  5,  gerber,    font);
        draw_hex(screen, 6,  5,  lampe(8),  font);
        draw_hex(screen, 6,  6,  obstacle,  font); // modif
        draw_hex(screen, 7,  5,  poirot,    font);
        draw_hex(screen, 9,  5,  vide,      font);
        draw_hex(screen, 2,  6,  vide,      font);
        draw_hex(screen, 3,  6,  vide,      font);
        draw_hex(screen, 5,  6,  vide,      font);
        draw_hex(screen, 7,  6,  vide,      font);
        draw_hex(screen, 8,  6,  egout(0),  font);
        draw_hex(screen, 8,  7,  obstacle,  font); // modif
        draw_hex(screen, 9,  6,  vide,      font);
        draw_hex(screen, 10, 5,  obstacle,  font); // modif
        draw_hex(screen, 10, 6,  vide,      font);
        draw_hex(screen, 10, 7,  obstacle,  font); // modif
        draw_hex(screen, 11, 6,  lampe(4),  font);
        draw_hex(screen, 12, 6,  vide,      font);
        draw_hex(screen, 2,  7,  vide,      font);
        draw_hex(screen, 3,  7,  lampe(3),  font);
        draw_hex(screen, 4,  7,  vide,      font);
        draw_hex(screen, 4,  8,  obstacle,  font); // modif
        draw_hex(screen, 5,  7,  vide,      font);
        draw_hex(screen, 6,  7,  egout(0),  font);
        draw_hex(screen, 6,  8,  obstacle,  font); // modif
        draw_hex(screen, 7,  7,  vide,      font);
        draw_hex(screen, 9,  7,  vide,      font);
        draw_hex(screen, 11, 7,  vide,      font);
        draw_hex(screen, 11, 8,  obstacle,  font); // modif
        draw_hex(screen, 12, 7,  vide,      font);
        draw_hex(screen, 12, 8,  obstacle,  font); // modif
        draw_hex(screen, 12, 9,  obstacle,  font); // modif
        draw_hex(screen, 5,  8,  vide,      font);
        draw_hex(screen, 7,  8,  lumiere,   font);
        draw_hex(screen, 8,  8,  lampe(8),  font);
        draw_hex(screen, 8,  9,  obstacle,  font); // modif
        draw_hex(screen, 9,  8,  legoutier, font);
        draw_hex(screen, 10, 8,  vide,      font);
        draw_hex(screen, 10, 9,  obstacle,  font); // modif
        draw_hex(screen, 13, 8,  vide,      font);
        draw_hex(screen, 5,  9,  freud,     font);
        draw_hex(screen, 6,  9,  lampe(0),  font);
        draw_hex(screen, 7,  9,  vide,      font);
        draw_hex(screen, 9,  9,  vide,      font);
        draw_hex(screen, 11, 9,  vide,      font);
        draw_hex(screen, 13, 9,  egout(0),  font);
        draw_hex(screen, 6,  10, egout(0),  font);
        draw_hex(screen, 7,  10, vide,      font);
        draw_hex(screen, 8,  10, vide,      font);
        draw_hex(screen, 9,  10, vide,      font);
        draw_hex(screen, 10, 10, vide,      font);
        draw_hex(screen, 11, 10, vide,      font);
        draw_hex(screen, 12, 10, vide,      font);
        draw_hex(screen, 13, 10, cruchot,   font);
        draw_hex(screen, 11, 11, vide,      font);
        draw_hex(screen, 12, 11, lampe(2),  font);
        draw_hex(screen, 13, 11, vide,      font);
        draw_hex(screen, 12, 12, egout(1),  font);
        draw_hex(screen, 13, 12, vide,      font);
        
        // draw_hex_grid(screen, font, 0, 0, 10, 10, a, b, c);

        SDL_Flip(screen);
    }
    
    TTF_CloseFont(font);
    SDL_Quit();

    return 0;
}
