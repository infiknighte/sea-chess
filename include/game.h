#include "chess.h"
#include "raylib.h"

#define MAX_SOUNDS 16

enum {
  SOUND_CAPTURE,
  SOUND_CASTLE,
  SOUND_CLICK,
  SOUND_DRAWOFFER,
  SOUND_DRAW,
  SOUND_GAME_END,
  SOUND_GAME_LOSE,
  SOUND_GAME_WIN,
  SOUND_GAME_START,
  SOUND_ILLEGAL,
  SOUND_MOVE_CHECK,
  SOUND_MOVE_SELF,
  SOUND_MOVE_OPPONENT,
  SOUND_PREMOVE,
  SOUND_PROMOTE,
  SOUND_TENSECONDS,
};

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
  Sound sounds[MAX_SOUNDS];
} game_t;

void game_init(game_t *game);
void game_load(game_t *game);
void game_update(game_t *game);
void game_quit(game_t *game);
