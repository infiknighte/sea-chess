#include "game.h"
#include "chess.h"
#include "common.h"
#include "raylib.h"
#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define INIT_WIDTH 800
#define INIT_HEIGHT 800

#define ASSETS_PATH "../assets"

#define MIN(a, b) ((a) < (b) ? (a) : (b))

static void _game_render(game_t *game);
static inline uint8_t _game_get_square_width(window_t window);
static inline coord_t _coord_from_mouse_pos(Vector2 mouse, uint8_t width);
static inline coord_t _coord_flip(coord_t coord, color_t player);
static inline coord_t _coord_change_to_perspective(coord_t coord,
                                                   color_t player);

void game_init(game_t *const game) {
  game->run = true;
  game->selected = COORD_UNDEFINED;
  game->promotion = COORD_UNDEFINED;
  game->window.width = INIT_WIDTH;
  game->window.height = INIT_HEIGHT;
  chess_init(&game->chess);
  InitWindow(INIT_WIDTH, INIT_HEIGHT, "chess");
  InitAudioDevice();
}

void game_load(game_t *const game) {
  memset(game->textures, 0, sizeof(game->textures));
  memset(game->sounds, 0, sizeof(game->sounds));
  const char pieces[PIECE_KIND_COUNT] = {'k', 'q', 'r', 'b', 'n', 'p'};
  const char colors[2] = {'b', 'w'};
  for (color_t color = COLOR_BLACK; color < 2; color++) {
    for (piece_kind_t kind = 1; kind <= PIECE_KIND_COUNT; kind++) {
      char path[sizeof(ASSETS_PATH) + 25];
      snprintf(path, sizeof(path), ASSETS_PATH "/images/%c%c.png",
               colors[color], pieces[kind - 1]);
      game->textures[color][kind] = LoadTexture(path);
    }
  }

  const char *sound_name[MAX_SOUNDS] = {
      "capture",  "confirmation", "error",        "generic-notify",
      "low-time", "move",         "out-of-bound", "social-notify"};
  for (uint8_t i = 0; i < MAX_SOUNDS; i++) {
    char path[sizeof(ASSETS_PATH) + 60];
    snprintf(path, sizeof(path), ASSETS_PATH "/sounds/%s.mp3", sound_name[i]);
    game->sounds[i] = LoadSound(path);
  }
}

void game_update(game_t *const game) {
  if (IsMouseButtonPressed(MOUSE_LEFT_BUTTON)) {
    const Vector2 mouse = GetMousePosition();
    const uint8_t width = _game_get_square_width(game->window);
    const color_t player = game->chess.player;
    if (game->chess.result == CHESS_RESULT_PROMOTION) {
      const uint16_t big_width = (width * 8) / 4;

      uint16_t mouse_min_bound = (player) ? game->window.height - big_width : 0;
      uint16_t mouse_max_bound = mouse_min_bound + big_width;

      if (IS_IN_BOUNDS(mouse.y, mouse_min_bound, mouse_max_bound + 1)) {
        piece_kind_t selected_kind =
            PIECE_KIND_QUEEN + floorf(mouse.x / big_width);
        chess_promote(&game->chess,
                      _coord_change_to_perspective(game->promotion, player),
                      selected_kind);
        PlaySound(game->sounds[SOUND_MOVE]);
        game->promotion = COORD_UNDEFINED;
      }

    } else {
      coord_t coord = _coord_from_mouse_pos(mouse, width);
      const piece_t piece = chess_get_piece_at(
          &game->chess, _coord_change_to_perspective(coord, player));

      if (game->selected == COORD_UNDEFINED) {
        if (!chess_is_empty_at(&game->chess,
                               _coord_change_to_perspective(coord, player)) &&
            game->chess.player == piece.color) {
          game->selected = coord;
        }
      } else {
        const piece_t selected_piece = chess_get_piece_at(
            &game->chess, _coord_change_to_perspective(game->selected, player));

        if (coord == game->selected) {
          game->selected = COORD_UNDEFINED;
        } else if (piece.kind != PIECE_KIND_NONE &&
                   selected_piece.color == piece.color &&
                   game->chess.player == piece.color) {
          game->selected = coord;

        } else {
          chess_make_move(&game->chess,
                          _coord_change_to_perspective(game->selected, player),
                          _coord_change_to_perspective(coord, player));
          switch (game->chess.result) {
          case CHESS_RESULT_ILLEGAL_MOVE:
            PlaySound(game->sounds[SOUND_ERROR]);
            break;
          case CHESS_RESULT_CHECKMATE:
          case CHESS_RESULT_STALEMATE:
            PlaySound(game->sounds[SOUND_GENERIC_NOTIFY]);
            break;
          case CHESS_RESULT_PROMOTION:
            game->promotion = coord;
            break;
          case CHESS_RESULT_CAPTURE:
            PlaySound(game->sounds[SOUND_CAPTURE]);
            break;
          case CHESS_RESULT_CHECK:
          case CHESS_RESULT_CASTLE:
          case CHESS_RESULT_OK:
            PlaySound(game->sounds[SOUND_MOVE]);
            break;
          }
          if (game->chess.result != CHESS_RESULT_ILLEGAL_MOVE) {
            game->selected = COORD_UNDEFINED;
          }
        }
      }
    }
  }
  BeginDrawing();
  ClearBackground(WHITE);
  _game_render(game);
  EndDrawing();
}

void game_quit(game_t *const game) {
  CloseWindow();
  CloseAudioDevice();
  for (color_t color = 0; color < 2; color++) {
    for (piece_kind_t kind = 1; kind <= PIECE_KIND_COUNT; kind++) {
      UnloadTexture(game->textures[color][kind]);
    }
  }
  for (uint8_t i = 0; i <= MAX_SOUNDS; i++) {
    UnloadSound(game->sounds[i]);
  }
}

static void _game_render_board(game_t *const game) {
  const uint8_t width = _game_get_square_width(game->window);
  const color_t player = game->chess.player;

  for (uint8_t i = 0; i < BOARD_WIDTH; i++) {
    for (uint8_t j = 0; j < BOARD_WIDTH; j++) {
      const coord_t coord = _coord_flip(i * 8 + j, player);
      DrawRectangle((coord % 8) * width, (coord / 8) * width, width, width,
                    (i + j) % 2 == 0 ? WHITE : DARKGREEN);
    }
  }
  if (game->selected != COORD_UNDEFINED) {
    DrawRectangle(game->selected % 8 * width, game->selected / 8 * width, width,
                  width, Fade(YELLOW, 0.3));
  }
}

void _draw_texture_as_square(const Texture2D texture, const uint16_t x,
                             const uint16_t y, const float width,
                             const Color tint) {
  const float tex_width = texture.width;
  const float tex_height = texture.height;
  const float scale =
      (tex_width > tex_height) ? (width / tex_width) : (width / tex_height);
  const float final_width = tex_width * scale;
  const float final_height = tex_height * scale;
  const Rectangle dest = {x, y, final_width, final_height};
  const Rectangle source = {0, 0, tex_width, tex_height};
  const Vector2 origin = {0, 0};

  DrawTexturePro(texture, source, dest, origin, 0.0f, tint);
}

static void _game_render_pieces(game_t *const game) {
  const uint8_t width = _game_get_square_width(game->window);
  const color_t player = game->chess.player;
  for (uint8_t i = 0; i < BOARD_WIDTH; i++) {
    for (uint8_t j = 0; j < BOARD_WIDTH; j++) {
      coord_t coord = _coord_change_to_perspective(i * 8 + j, player);
      const piece_t piece = chess_get_piece_at(&game->chess, i * 8 + j);
      if (piece.kind != PIECE_KIND_NONE) {
        _draw_texture_as_square(game->textures[piece.color][piece.kind],
                                coord % 8 * width, coord / 8 * width, width,
                                WHITE);
      }
    }
  }
}

static void _game_render_moves(game_t *const game) {
  if (game->selected != COORD_UNDEFINED) {
    return;
  }
  const color_t player = game->chess.player;
  moves_t moves = chess_legal_moves_of(
      &game->chess, _coord_change_to_perspective(game->selected, player));
  if (moves.ptr == NULL || moves.count == 0) {
    return;
  }

  const uint8_t width = _game_get_square_width(game->window);
  for (uint32_t i = 0; i < moves.count; i++) {
    const coord_t move = _coord_change_to_perspective(moves.ptr[i], player);
    DrawCircle((move % 8 * width) + (width / 2),
               (move / 8 * width) + (width / 2), (float)width / 4,
               Fade(BLACK, 0.7));
  }
  moves_free(&moves);
}

static void _game_render_promotion_choices(game_t *const game) {
  if (game->chess.result != CHESS_RESULT_PROMOTION) {
    return;
  }
  const uint8_t sqr_width = _game_get_square_width(game->window);
  uint16_t width = (sqr_width * 8) / 4;
  const color_t color = game->chess.player;
  const uint16_t pos_y = color ? game->window.height - width : 0;

  for (uint8_t i = 0; i < 4; i++) {
    const uint16_t pos_x = width * i;
    DrawRectangle(pos_x, pos_y, width, width, (i % 2) ? WHITE : DARKGREEN);
    _draw_texture_as_square(game->textures[color][PIECE_KIND_QUEEN + i], pos_x,
                            pos_y, width, WHITE);
  }
}

static void _game_render(game_t *const game) {
  _game_render_board(game);
  _game_render_pieces(game);
  _game_render_moves(game);
  _game_render_promotion_choices(game);
}

static inline uint8_t _game_get_square_width(window_t window) {
  return MIN(window.width, window.height) / BOARD_WIDTH;
}

static inline coord_t _coord_from_mouse_pos(Vector2 mouse, uint8_t width) {
  return floorf(mouse.y / (float)width) * 8 + floorf(mouse.x / (float)width);
}

static inline coord_t _coord_flip(coord_t coord, color_t player) {
  if (player != COLOR_WHITE) {
    int rank = coord / 8;
    int file = coord % 8;
    return (7 - rank) * 8 + (7 - file);
  }
  return coord;
}

static inline coord_t _coord_change_to_perspective(coord_t coord,
                                                   color_t player) {
  int rank = coord / 8;
  int file = coord % 8;

  if (player == COLOR_WHITE) {
    return (7 - rank) * 8 + file;
  } else {
    return rank * 8 + (7 - file);
  }
}
