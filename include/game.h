#include "chess.h"
#include "raylib.h"

#define MAX_SOUNDS 8

enum {
  SOUND_CAPTURE,
  SOUND_CONFIRMATION,
  SOUND_ERROR,
  SOUND_GENERIC_NOTIFY,
  SOUND_LOW_TIME,
  SOUND_MOVE,
  SOUND_OUT_OF_BOUND,
  SOUND_SOCIAL_NOTIFY,
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
