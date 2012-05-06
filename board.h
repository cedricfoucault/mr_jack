#ifndef _BOARD_H
#define _BOARD_H

#include "hex.h"

typedef struct BoardHex_ {
    Hex hex;
    int x;
    int y;
} BoardHex;

typedef struct Board_ {
    BoardHex hexes[100];
    int size  ;
} Board;

#endif
