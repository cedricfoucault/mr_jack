#include "hex.h"
#include <string.h>
#include <stdbool.h>
#include <SDL/SDL.h>
#include <SDL/SDL_gfxPrimitives.h>
#include <SDL/SDL_ttf.h>

short c = 40;
// short a = c / 2;
short a = 20;
// short b = (short) (0.866 * c);
short b = 35;

void draw_hex(SDL_Surface *screen, Hex h, int i, int j, TTF_Font *font) {
    int r, g, blue, alpha;
    alpha = 255;
    SDL_Color text_color = {255, 255, 255};
    char text_label[16];
    
    switch (h.content) {
        case VIDE: {
            r = 150;
            g = 150;
            blue = 150;
            strcpy(text_label, "VIDE");
            break;
        }
        case OBSTACLE: {
            r = 90;
            g = 90;
            blue = 90;
            strcpy(text_label, "OBSTACLE");
            break;
        }
        case EGOUT: {
            if (h.label == 0) {
                r = 130;
                g = 142;
                blue = 35;
                sprintf(text_label, "(O) EGOUT %d", h.id);
            } else if (h.label == 1) {
                r = 140;
                g = 154;
                blue = 110;
                sprintf(text_label, "(F) EGOUT %d", h.id);
            }
            break;
        }
        case LAMPE: {
            if (h.label > 0) {
                // yellow (lit lamp post)
                r = 255;
                g = 255;
                blue = 0;
                text_color.r = 0;
                text_color.g = 0;
                text_color.b = 0;
            } else {
                // gray
                r = 150;
                g = 150;
                blue = 110;   
            }
            sprintf(text_label, "(%d) LAMPE %d", h.label, h.id);
            break;
        }
        case SORTIE: {
            r = 255;
            g = 255;
            blue = 255;
            text_color.r = 0;
            text_color.g = 0;
            text_color.b = 0;
            sprintf(text_label, "SORTIE %d", h.id);
            break;
        }
        case BARRAGE: {
            r = 25;
            g = 25;
            blue = 25;
            sprintf(text_label, "BARRAGE %d", h.id);
            break;
        }
        case MAIGRET: {
            // brown
            r = 139;
            g = 69;
            blue = 19;
            strcpy(text_label, "MAIGRET");
            break;
        }
        case POIROT: {
            // red
            r = 255;
            g = 0;
            blue = 0;
            strcpy(text_label, "POIROT");
            break;
        }
        case LUMIERE: {
            // yellow
            r = 0;
            g = 255;
            blue = 255;
            text_color.r = 0;
            text_color.g = 0;
            text_color.b = 0;
            strcpy(text_label, "LUMIERE");
            break;
        }
        case LEGOUTIER: {
            // orange
            r = 255;
            g = 140;
            blue = 0;
            text_color.r = 0;
            text_color.g = 0;
            text_color.b = 0;
            strcpy(text_label, "LEGOUTIER");
            break;
        }
        case GERBER: {
            // blue
            r = 0;
            g = 0;
            blue = 255;
            strcpy(text_label, "GERBER");
            break;
        }
        case CRUCHOT: {
            // dark blue
            r = 0;
            g = 0;
            blue = 128;
            strcpy(text_label, "CRUCHOT");
            break;
        }
        case FREUD: {
            // purple
            r = 160;
            g = 32;
            blue = 240;
            text_color.r = 0;
            text_color.g = 0;
            text_color.b = 0;
            strcpy(text_label, "FREUD");
            break;
        }
        case DISCRETE: {
            // green
            r = 34;
            g = 139;
            blue = 34;
            strcpy(text_label, "DISCRETE");
            break;
        }
    }
    
    if (h.seen == 1) {
        text_color.r = 255;
        text_color.g = 255;
        text_color.b = 0;
    }
    
    int xi = 0, yi = 500;
    // int x = xi + (a + c) * i;
    int x = xi + (a + c) * i;
    int y = yi - 2 * b * j + b * i;
    short s[6] = { x, x + a, x + a + c, x + 2 * c, x + a + c, x + a }; 
    short t[6] = { y + b, y, y, y + b, y + 2 * b, y + 2 * b };
    
    filledPolygonRGBA(screen, 
        s, t,
        6,
        r, g, blue, alpha);
    
    polygonRGBA(screen, 
        s, t,
        6,
        255, 255, 255, 255);
        
    // compute the metrics of the text to display
    int width, height;
    TTF_SizeUTF8(font, text_label, &width, &height);
    // compute the corresponding text
    SDL_Rect rect;
    rect.x = x + (2 * c - width) / 2;
    rect.y = y + (2 * b - height) / 2;
    // draw the text
    SDL_Surface *text_surface;
    if(!(text_surface = TTF_RenderText_Solid(font, text_label, text_color))) {
        fprintf(stderr, "TTF_RenderText_Solid: %s\n", TTF_GetError());
    } else {
        SDL_BlitSurface(text_surface, NULL, screen, &rect);
    }
    SDL_FreeSurface(text_surface);
    
    if (h.content == MAIGRET) {
        // draw the arrow indicating the lantern's direction
        int ybase, ynorth, ywest, yeast;
        int xbase, xnorth, xwest, xeast;
        
        switch(h.label) {
            case 1: {
                xbase = x + c;
                ybase = y + (3 * b) / 4;
                ynorth = ybase - b / 2;
                xnorth = xbase;
                xwest = xnorth - 6;
                ywest = ynorth + 6;
                xeast = xnorth + 6;
                yeast = ywest;
                break;
            }
            case 2: {
                ybase = y + (3 * b) / 4;
                xbase = x + c;
                ynorth = ybase - b / 4;
                xnorth = xbase - (b * 87) / 200;
                ywest = ynorth + 6;
                xwest = xnorth + 2;
                yeast = ynorth - 2;
                xeast = xnorth + 6;
                break;
            }
            case 3: {
                xbase = x + c;
                ybase = y + (5 * b) / 4;
                ynorth = ybase + b / 4;
                xnorth = xbase - (b * 87) / 200;
                xwest = xnorth + 6;
                ywest = ynorth + 2;
                xeast = xnorth + 2;
                yeast = ynorth - 6;
                break;
            }
            case 4: {
                xbase = x + c;
                ybase = y + (5 * b) / 4;
                ynorth = ybase + b / 2;
                xnorth = xbase;
                xwest = xnorth - 6;
                ywest = ynorth - 6;
                xeast = xnorth + 6;
                yeast = ywest;
                break;
            }
            case 5: {
                xbase = x + c;
                ybase = y + (5 * b) / 4;
                ynorth = ybase + b / 4;
                xnorth = xbase + (b * 87) / 200;
                xwest = xnorth - 2;
                ywest = ynorth - 6;
                xeast = xnorth - 6;
                yeast = ynorth + 2;
                break;
            }
            case 6: {
                ybase = y + (3 * b) / 4;
                xbase = x + c;
                ynorth = ybase - b / 4;
                xnorth = xbase + (b * 87) / 200;
                ywest = ynorth - 2;
                xwest = xnorth - 6;
                yeast = ynorth + 6;
                xeast = xnorth - 2;
                break;
            }
        }
        
        // main line
        lineRGBA(screen, xbase, ybase, xnorth, ynorth,
            255, 255, 0, 255);
        // west part
        lineRGBA(screen, xwest, ywest, xnorth, ynorth,
            255, 255, 0, 255);
        // east part
        lineRGBA(screen, xeast, yeast, xnorth, ynorth,
            255, 255, 0, 255);
    }
}

Hex string_to_hex(char *content, int label, int id, int seen) {
    Hex h;
    h.label = label;
    h.id = id;
    h.seen = seen;
    
    if (strcmp(content, "vide") == 0) {
        h.content = VIDE;
    } else if (strcmp(content, "obstacle") == 0) {
        h.content = OBSTACLE;
    } else if (strcmp(content, "egout") == 0) {
        h.content = EGOUT;
    } else if (strcmp(content, "lampe") == 0) {
        h.content = LAMPE;
    } else if (strcmp(content, "sortie") == 0) {
        h.content = SORTIE;
    } else if (strcmp(content, "barrage") == 0) {
        h.content = BARRAGE;
    } else if (strcmp(content, "maigret") == 0) {
        h.content = MAIGRET;
    } else if (strcmp(content, "poirot") == 0) {
        h.content = POIROT;
    } else if (strcmp(content, "lumiere") == 0) {
        h.content = LUMIERE;
    } else if (strcmp(content, "legoutier") == 0) {
        h.content = LEGOUTIER;
    } else if (strcmp(content, "gerber") == 0) {
        h.content = GERBER;
    } else if (strcmp(content, "cruchot") == 0) {
        h.content = CRUCHOT;
    } else if (strcmp(content, "freud") == 0) {
        h.content = FREUD;
    } else if (strcmp(content, "discrete") == 0) {
        h.content = DISCRETE;
    }
    
    return h;
}

Hex lampe(int label) {
    Hex lampe_ = { LAMPE, label };
    return lampe_;
}
Hex egout(int label) {
    Hex egout_ = { EGOUT, label };
    return egout_;
}
