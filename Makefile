mac: board.h hex.h hex.c wrap.c plateau.pl game.pl
	gplc -L -lSDL -L -lSDLmain -L -lSDL_gfx -L -lSDL_ttf -L -framework -L cocoa hex.c wrap.c game.pl -o mrjack

linux: board.h hex.h hex.c wrap.c plateau.pl game.pl
	gplc -L -lSDL -L -lSDLmain -L -lSDL_gfx -L -lSDL_ttf hex.c wrap.c game.pl -o game mrjack