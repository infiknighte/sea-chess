#ifndef CHESS_H
#define CHESS_H

#include "common.h"

#define BOARD_INIT_FEN                                                         \
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#define COORD_UNDEFINED 64

#define PIECE_KIND_COUNT 6

#define MAX_PIECE_COUNT 16

#define WHITE_KING_SIDE_CASTLE_RIGHT 0x1
#define WHITE_QUEEN_SIDE_CASTLE_RIGHT 0x2
#define BLACK_KING_SIDE_CASTLE_RIGHT 0x4
#define BLACK_QUEEN_SIDE_CASTLE_RIGHT 0x8

#define BOARD_AREA 64

#ifdef CHESS_DEBUG
#include <stdio.h>
#define CHESS_LOG(chess)                                                       \
  fprintf(stderr,                                                              \
          "Player: %s | Full Move: %d | Half Move: %d | En Passant: %s | "     \
          "Castling Rights: [WK:%d WQ:%d BK:%d BQ:%d]",                        \
          (chess).player == COLOR_WHITE ? "White" : "Black",                   \
          (chess).full_move, (chess).half_move,                                \
          ((chess).en_passant.rank < BOARD_WIDTH &&                            \
           (chess).en_passant.file < BOARD_WIDTH)                              \
              ? TextFormat("(%d, %d)", (chess).en_passant.rank,                \
                           (chess).en_passant.file)                            \
              : "None",                                                        \
          (chess).castle[COLOR_WHITE][0], (chess).castle[COLOR_WHITE][1],      \
          (chess).castle[COLOR_BLACK][0], (chess).castle[COLOR_BLACK][1])
#else
#define CHESS_LOG(chess) ()
#endif

typedef enum {
  CHESS_RESULT_ILLEGAL_MOVE,
  CHESS_RESULT_CHECK,
  CHESS_RESULT_CHECKMATE,
  CHESS_RESULT_STALEMATE,
  CHESS_RESULT_PROMOTION,
  CHESS_RESULT_CAPTURE,
  CHESS_RESULT_CASTLE,
  CHESS_RESULT_OK,
} chess_result_t;

typedef enum {
  COLOR_BLACK = 0,
  COLOR_WHITE = 1,
} color_t;

typedef enum {
  PIECE_KIND_NONE,
  PIECE_KIND_KING,
  PIECE_KIND_QUEEN,
  PIECE_KIND_ROOK,
  PIECE_KIND_BISHOP,
  PIECE_KIND_KNIGHT,
  PIECE_KIND_PAWN,
} piece_kind_t;

typedef struct {
  color_t color;
  piece_kind_t kind;
} piece_t;

typedef piece_t board_t[BOARD_AREA];

typedef int8_t coord_t;

typedef struct {
  coord_t *ptr;
  uint32_t count;
  uint32_t capacity;
} moves_t;

typedef uint64_t bitboard_t;

typedef struct {
  coord_t en_passant;
  uint8_t half_move;
  unsigned _BitInt(4) castle_rights;
  uint32_t full_move;
  color_t player;
  bitboard_t bitboards[2][PIECE_KIND_COUNT + 1];
  board_t board;
  chess_result_t result;
} chess_t;

void chess_from_fen(chess_t *chess, const char *fen);
void chess_make_move(chess_t *chess, coord_t piece_coord, coord_t move);
moves_t chess_legal_moves_of(chess_t *chess, coord_t piece_coord);
bool chess_is_in_check(chess_t *chess);
bool chess_promote(chess_t *chess, coord_t coord, piece_kind_t promotion_kind);
void moves_init(moves_t *moves);
void moves_push(moves_t *moves, coord_t move);
void moves_free(moves_t *moves);

static inline void chess_init(chess_t *const chess) {
  chess_from_fen(chess, BOARD_INIT_FEN);
}

static inline piece_t chess_get_piece_at(const chess_t *const chess,
                                         const coord_t coord) {
  return chess->board[coord];
}

static inline bool chess_is_empty_at(const chess_t *const chess,
                                     const coord_t coord) {
  return chess_get_piece_at(chess, coord).kind == PIECE_KIND_NONE;
}

static inline bool piece_is_equal(const piece_t a, const piece_t b) {
  return a.color == b.color && a.kind == b.kind;
}

#endif
