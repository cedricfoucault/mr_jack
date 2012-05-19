#include <stdio.h>
#include <stdbool.h>
#include <SDL/SDL.h>
#include <SDL/SDL_gfxPrimitives.h>
#include <SDL/SDL_ttf.h>
#include <SDL/SDL_mutex.h>
#include "hex.h"
#include "board.h"
#include <gprolog.h>

const int WINDOW_WIDTH = 1024;
const int WINDOW_HEIGHT = 768;
const char* WINDOW_TITLE = "Jack";

// is true while the game is running
bool running = false;
// the surface for the screen
SDL_Surface* screen;
// the font used to for the hexes
TTF_Font *font;
// counter to compute the size of the board
int board_counter;
// the board of the game
Board board;
// used to solve conflict between threads read/writes of the board
SDL_mutex *board_mutex;
// the thread drawing the board on screen
SDL_Thread *drawing_thread;
// the time interval between 2 frames of the drawing loop
const int FRAME_INTERVAL = 100; // 10 FPS

static int Main_Wrapper(int argc, char *argv[]) {
    int functor;

    Pl_Start_Prolog(argc, argv);
    functor = Pl_Find_Atom("new_game");

    Pl_Query_Begin(PL_FALSE);
    Pl_Query_Call(functor, 0, NULL);
    Pl_Query_End(PL_CUT);
    
    Pl_Stop_Prolog();
    
    return 0;
}

int main(int argc, char *argv[]) {    
    return Main_Wrapper(argc, argv);
}

void init() {
    SDL_Init( SDL_INIT_VIDEO );
    TTF_Init();
    atexit(TTF_Quit);
    // board_mutex = SDL_CreateMutex();
    
    // load font
    font = TTF_OpenFont("arial_black.ttf", 8);
    if(!font) {
        fprintf(stderr, "TTF_OpenFont: %s\n", TTF_GetError());
    }
    
    // init screen
    screen = SDL_SetVideoMode( WINDOW_WIDTH, WINDOW_HEIGHT, 0, 
        SDL_HWSURFACE | SDL_DOUBLEBUF );
    SDL_WM_SetCaption( WINDOW_TITLE, 0 );
    board_counter = 0;
}

void show_screen() {
    SDL_Flip(screen);
}

void lock_board() {
    SDL_mutexP(board_mutex);
}

void unlock_board() {
    SDL_mutexV(board_mutex);
}

void draw_board() {
    // lock_board();
    int size = board.size;
    int i;
    SDL_FillRect(screen, NULL, 0);
    for (i = 0; i < size; i++) {
        BoardHex bh = board.hexes[i];
        draw_hex(screen, bh.hex, bh.x, bh.y, font);
    }
    // unlock_board();
}

// void drawing_loop() {
//     int lastLoopTime = SDL_GetTicks();
//     while (running) {
//         draw_board();
//         show_screen();
//         printf("loop\n");
//         fflush(stdout);
//         int time_left = FRAME_INTERVAL - (SDL_GetTicks() - lastLoopTime);
//         SDL_Delay(time_left);
//         lastLoopTime = SDL_GetTicks();
//     }
// }

void delay(PlLong time_delay) {
    SDL_Delay(time_delay);
}

// int drawing_function(void *data) {
//     drawing_loop();
//     return 0;
// }

// void start() {
//     init();
//     running = true;
//     drawing_thread = SDL_CreateThread(drawing_function, NULL);
// }

void set_board_hex(
    char *content, PlLong label, PlLong id, PlLong seen, PlLong x, PlLong y
) {     
    board.hexes[board_counter].hex = string_to_hex(content, label, id, seen);
    board.hexes[board_counter].x = x;
    board.hexes[board_counter].y = y;
    board_counter++;
}

void finish_set() {
    board.size = board_counter;
    board_counter = 0;
}

void quit() {
    running = false;
    SDL_KillThread(drawing_thread);
    SDL_DestroyMutex(board_mutex);
    TTF_CloseFont(font);
    SDL_Quit();
}
