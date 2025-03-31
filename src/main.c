#include "game.h"

#if defined(PLATFORM_WEB)
#include <emscripten/emscripten.h>

game_t game;
void g_game_update(void) { game_update(&game); }

#endif

int main(void) {
#ifdef PLATFORM_WEB
  game_init(&game);
  game_load(&game);
  emscripten_set_main_loop(g_game_update, 0, 1);
#else
  game_t game;
  game_init(&game);
  game_load(&game);
  SetTargetFPS(60);

  while (!WindowShouldClose()) {
    game_update(&game);
  }
#endif

  game_quit(&game);
  return 0;
}
