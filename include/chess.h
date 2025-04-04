#ifndef CHESS_H
#define CHESS_H

#include "common.h"

#define BOARD_INIT_FEN                                                         \
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

#define COORD_UNDEFINED 64

#define PIECE_KIND_COUNT 6

#define MAX_PIECE_COUNT 16

#define KING_SIDE_CASTLE 0
#define QUEEN_SIDE_CASTLE 1

#define MAX_ROOK_RELEVANT_BITS 12
#define MAX_ROOK_BLOCKER_PERM 4096

#define BOARD_AREA 64

#define RANK_1 0x00000000000000FFULL
#define RANK_2 0x000000000000FF00ULL
#define RANK_3 0x0000000000FF0000ULL
#define RANK_4 0x00000000FF000000ULL
#define RANK_5 0x000000FF00000000ULL
#define RANK_6 0x0000FF0000000000ULL
#define RANK_7 0x00FF000000000000ULL
#define RANK_8 0xFF00000000000000ULL

#define FILE_A 0x0101010101010101ULL
#define FILE_B 0x0202020202020202ULL
#define FILE_C 0x0404040404040404ULL
#define FILE_D 0x0808080808080808ULL
#define FILE_E 0x1010101010101010ULL
#define FILE_F 0x2020202020202020ULL
#define FILE_G 0x4040404040404040ULL
#define FILE_H 0x8080808080808080ULL

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
  bool castle[2][2];
  color_t player;
  coord_t en_passant;
  uint8_t half_move;
  uint32_t full_move;
  board_t board;
  struct {
    bitboard_t king[64];
    bitboard_t knight[64];
  } moves;
  bitboard_t bitboards[2][PIECE_KIND_COUNT + 1];
  bitboard_t attacks[2][PIECE_KIND_COUNT + 1][64];
  bitboard_t rook_mask[BOARD_AREA][MAX_ROOK_BLOCKER_PERM];
  chess_result_t result;
} chess_t;

void chess_from_fen(chess_t *chess, const char *fen);
void chess_make_move(chess_t *chess, coord_t piece_coord, coord_t move);
bitboard_t chess_legal_moves_of(chess_t *chess, coord_t piece_coord);
bool chess_is_in_check(chess_t *chess);
bool chess_promote(chess_t *chess, coord_t coord, piece_kind_t promotion_kind);
void moves_init(moves_t *moves);
void moves_push(moves_t *moves, coord_t move);
void moves_free(moves_t *moves);

static inline coord_t coord_new(coord_t rank, coord_t file) {
  return rank * BOARD_WIDTH + file;
}
static inline coord_t coord_rank(coord_t coord) { return coord / BOARD_WIDTH; }
static inline coord_t coord_file(coord_t coord) { return coord % BOARD_WIDTH; }
static inline coord_t coord_change_file(coord_t coord, coord_t file) {
  return coord += file;
}
static inline coord_t coord_change_rank(coord_t coord, coord_t rank) {
  return coord + rank * BOARD_WIDTH;
}
static inline coord_t coord_change(coord_t coord, coord_t rank, coord_t file) {
  return coord + rank * BOARD_WIDTH + file;
}
static inline bool coord_rank_is_equal(coord_t a, coord_t b) {
  return coord_rank(a) == coord_rank(b);
}

static inline void chess_init(chess_t *const chess) {
  chess_from_fen(chess, BOARD_INIT_FEN);
}

static inline bool chess_is_empty_at(const chess_t *const chess,
                                     const coord_t coord) {
  return chess->board[coord].kind == PIECE_KIND_NONE;
}

static inline bool piece_is_equal(const piece_t a, const piece_t b) {
  return a.color == b.color && a.kind == b.kind;
}

static inline bool coord_is_undefined(const coord_t coord) {
  return coord == COORD_UNDEFINED;
}

static inline bitboard_t
chess_occupied_squares_of(bitboard_t bitboards[static 2][PIECE_KIND_COUNT + 1],
                          color_t color) {
  bitboard_t result = 0;
  for (uint8_t i = 1; i <= PIECE_KIND_COUNT; i++) {
    result |= bitboards[color][i];
  }
  return result;
}

bitboard_t find_magic_number(coord_t square,
                             bitboard_t blockers[static MAX_ROOK_BLOCKER_PERM],
                             bitboard_t attacks[MAX_ROOK_BLOCKER_PERM],
                             uint8_t relevant);

#endif
