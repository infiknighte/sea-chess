#include "chess.h"
#include "raylib.h"

typedef struct {
  uint16_t width;
  uint16_t height;
} window_t;

typedef struct {
  bool run;
  coord_t selected;
  coord_t promotion;
  window_t window;
  chess_t chess;
  Texture2D textures[2][PIECE_KIND_COUNT + 1];
} game_t;

void game_init(game_t *game);
void game_load(game_t *game);
void game_update(game_t *game);
void game_quit(game_t *game);
