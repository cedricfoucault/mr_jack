test:
	gcc hex.c sdltest.c -lSDL -lSDLmain -lSDL_gfx -lSDL_ttf -framework cocoa -o sdltest

game: board.h hex.h hex.c wrap.c plateau.pl game.pl
	gplc -L -lSDL -L -lSDLmain -L -lSDL_gfx -L -lSDL_ttf -L -framework -L cocoa hex.c wrap.c game.pl -o game
