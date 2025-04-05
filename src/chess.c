#include "chess.h"
#include "common.h"
#include <ctype.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static inline void _chess_board_move_piece(chess_t *chess, coord_t from,
                                           coord_t to);
static void _chess_board_from_fen(chess_t *chess, const char **p_fen);
static void _chess_fen_parse_castle_rights(unsigned _BitInt(4) * castle_rights,
                                           const char **const p_fen);
static void _chess_fen_parse_en_passant(coord_t *en_passant,
                                        const char **p_fen);
static void _chess_fen_parse_half_move(uint8_t *const half_move,
                                       const char **p_fen);
static bitboard_t _chess_legal_moves_of_king(chess_t *chess, coord_t coord);
static bitboard_t _chess_legal_moves_of_queen(chess_t *chess, coord_t coord);
static bitboard_t _chess_legal_moves_of_rook(chess_t *chess, coord_t coord);
static bitboard_t _chess_legal_moves_of_bishop(chess_t *chess, coord_t coord);
static void _chess_update_castle_rights(chess_t *chess);
static bitboard_t _chess_moves_for_pawn(const bitboard_t pawn,
                                        const color_t color,
                                        const bitboard_t enemies);
static bitboard_t
_bitboard_merge(bitboard_t bitboards[static PIECE_KIND_COUNT + 1]);
static inline bool _coord_is_in_bounds(const coord_t coord);
void _chess_king_moves_init(void);
void _chess_knight_moves_init(void);

static bitboard_t g_KING_MOVES[BOARD_AREA];
static bitboard_t g_KNIGHT_MOVES[BOARD_AREA];

void chess_from_fen(chess_t *const chess, const char *fen) {
  memset(chess, 0, sizeof(chess_t));
  _chess_board_from_fen(chess, &fen);
  chess->player = (*fen == 'w') ? 1 : 0;
  fen += 2;
  _chess_fen_parse_castle_rights(&chess->castle_rights, &fen);
  _chess_fen_parse_en_passant(&chess->en_passant, &fen);
  _chess_fen_parse_half_move(&chess->half_move, &fen);
  chess->full_move = atoi(fen);

  _chess_king_moves_init();
  _chess_knight_moves_init();
}

void chess_make_move(chess_t *const chess, const coord_t piece_coord,
                     const coord_t move) {
  if (chess->result == CHESS_RESULT_PROMOTION) {
    return;
  }

  const piece_t piece = chess_get_piece_at(chess, piece_coord);
  if (piece.kind == PIECE_KIND_NONE || piece.color != chess->player) {
    chess->result = CHESS_RESULT_ILLEGAL_MOVE;
    return;
  }

  _chess_update_castle_rights(chess);

  bitboard_t legal_moves = chess_legal_moves_of(chess, piece_coord);
  if (!(legal_moves & (1ULL << move))) {
    chess->result = CHESS_RESULT_ILLEGAL_MOVE;
    return;
  }

  bool is_a_capture = !chess_is_empty_at(chess, move);

  _chess_board_move_piece(chess, piece_coord, move);

  if (chess->player == COLOR_BLACK) {
    chess->full_move++;
    chess->half_move++;
  }
  if (piece.kind == PIECE_KIND_PAWN || is_a_capture) {
    chess->half_move = 0;
  }

  if (piece.kind == PIECE_KIND_PAWN && move / 8 == (piece.color ? 7 : 0)) {
    chess->result = CHESS_RESULT_PROMOTION;
  }

  bool is_en_passant = chess->en_passant != COORD_UNDEFINED;
  if (is_en_passant) {
    if (move == chess->en_passant) {
      coord_t coord = piece_coord % 8 + (move % 8);
      chess->board[coord].kind = PIECE_KIND_NONE;
    }
    chess->en_passant = COORD_UNDEFINED;
  }
  if (piece.kind == PIECE_KIND_PAWN &&
      piece_coord == move + (piece.color ? -2 : 2)) {
    chess->en_passant = piece_coord + 8 * (piece.color ? 1 : -1);
  }

  bool is_king_side_castle = false;
  const int8_t diff = piece_coord % 8 - move % 8;
  if (piece.kind == PIECE_KIND_KING &&
      (diff == 2 || (is_king_side_castle = diff == -2))) {
    const coord_t rook_coord = move + (is_king_side_castle ? 1 : -2);
    const coord_t move_rook = move + (is_king_side_castle ? 5 : 3);

    _chess_board_move_piece(chess, rook_coord, move_rook);
    switch (piece.color) {
    case COLOR_BLACK:
      chess->castle_rights &=
          ~(BLACK_KING_SIDE_CASTLE_RIGHT | BLACK_QUEEN_SIDE_CASTLE_RIGHT);
      break;
    case COLOR_WHITE:
      chess->castle_rights &=
          ~(WHITE_KING_SIDE_CASTLE_RIGHT | WHITE_QUEEN_SIDE_CASTLE_RIGHT);
      break;
    }
    chess->result = CHESS_RESULT_CASTLE;
  }

  if (chess_is_in_check(chess)) {
    chess->player = !chess->player;
    chess->result = CHESS_RESULT_CHECK;
  }
  chess->player = !chess->player;
  chess->result = is_a_capture ? CHESS_RESULT_CAPTURE : CHESS_RESULT_OK;
}

bool chess_promote(chess_t *chess, coord_t coord, piece_kind_t promotion_kind) {
  const piece_t piece = chess_get_piece_at(chess, coord);
  if (chess->result != CHESS_RESULT_PROMOTION &&
      (piece.kind != PIECE_KIND_PAWN || coord / 8 != (piece.color ? 7 : 0))) {
    return false;
  }
  chess->board[coord] = (piece_t){piece.color, promotion_kind};
  chess->result = CHESS_RESULT_OK;
  chess->player = !chess->player;

  return true;
}

bitboard_t chess_legal_moves_of(chess_t *const chess, const coord_t coord) {
  const piece_t piece = chess_get_piece_at(chess, coord);
  const color_t color = piece.color;
  const bitboard_t bit = 1ULL << coord;
  const bitboard_t allies = _bitboard_merge(chess->bitboards[color]);
  const bitboard_t enemies = _bitboard_merge(chess->bitboards[!color]);

  bitboard_t moves = 0;
  switch (piece.kind) {
  case PIECE_KIND_KING:
    moves = g_KING_MOVES[coord];
    break;
  case PIECE_KIND_QUEEN:
    moves = _chess_legal_moves_of_queen(chess, coord);
    break;
  case PIECE_KIND_ROOK:
    moves = _chess_legal_moves_of_rook(chess, coord);
    break;
  case PIECE_KIND_BISHOP:
    moves = _chess_legal_moves_of_bishop(chess, coord);
    break;
  case PIECE_KIND_KNIGHT:
    moves = g_KNIGHT_MOVES[coord];
    break;
  case PIECE_KIND_PAWN:
    moves = _chess_moves_for_pawn(bit, color, enemies);
    break;
  default:
    break;
  }
  return moves & ~allies;
}

bool chess_is_in_check(chess_t *const chess) { return false; }

static inline void _chess_board_move_piece(chess_t *chess, coord_t from,
                                           coord_t to) {
  chess->board[to] = chess->board[from];
  chess->board[from].kind = PIECE_KIND_NONE;
}

static void _chess_update_attacks(chess_t *chess) {
  for (uint8_t i = 0; i < 2; i++) {
    for (uint8_t j = 0; j < PIECE_KIND_COUNT; j++) {
    }
  }
}

void _chess_king_moves_init(void) {
  for (uint8_t i = 0; i < 64; i++) {
    const bitboard_t king = (1ULL << i);

    g_KING_MOVES[i] |= (king << 8) | (king >> 8);

    g_KING_MOVES[i] |= (king << 1) & ~FILE_A;
    g_KING_MOVES[i] |= (king >> 1) & ~FILE_H;

    g_KING_MOVES[i] |= (king << 9) & ~FILE_A;
    g_KING_MOVES[i] |= (king << 7) & ~FILE_H;

    g_KING_MOVES[i] |= (king >> 7) & ~FILE_H;
    g_KING_MOVES[i] |= (king >> 9) & ~FILE_A;
  }
}

static bitboard_t _chess_legal_moves_of_queen(chess_t *const chess,
                                              const coord_t coord) {
  bitboard_t moves = 0;
  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
  const int8_t delta_file[8] = {-1, 0, 1, -1, 1, -1, 0, 1};

  for (uint8_t i = 0; i < 8; i++) {
    coord_t move = (coord + delta_rank[i] * 8) + (coord % 8 + delta_file[i]);

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves |= 1ULL << move;
      move += 8 * delta_rank[i];
      move += delta_file[i];
    }
    if (_coord_is_in_bounds(move) &&
        (!chess_is_empty_at(chess, move) &&
         chess_get_piece_at(chess, move).color != color)) {
      moves |= move;
    }
  }
  return moves;
}
static bitboard_t _chess_legal_moves_of_rook(chess_t *const chess,
                                             const coord_t coord) {
  bitboard_t moves = 0;

  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[4] = {-1, 1, 0, 0};
  const int8_t delta_file[4] = {0, 0, -1, 1};

  for (uint8_t i = 0; i < 4; i++) {
    coord_t move = coord / 8 + delta_rank[i] + coord % 8 + delta_file[i];

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves |= 1ULL << move;
      move += 8 * delta_rank[i];
      move += delta_file[i];
    }
    if (_coord_is_in_bounds(move) &&
        (!chess_is_empty_at(chess, move) &&
         chess_get_piece_at(chess, move).color != color)) {
      moves |= 1ULL << move;
    }
  }
  return moves;
}

static bitboard_t _chess_legal_moves_of_bishop(chess_t *const chess,
                                               const coord_t coord) {
  bitboard_t moves = 0;

  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[4] = {-1, -1, 1, 1};
  const int8_t delta_file[4] = {-1, 1, -1, 1};

  for (uint8_t i = 0; i < 4; i++) {
    coord_t move =
        ((coord / 8 + delta_rank[i]) * 8) + coord % 8 + delta_file[i];

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves |= 1ULL << move;
      move += 8 * delta_rank[i];
      move += delta_file[i];
    }

    if (_coord_is_in_bounds(move) &&
        (!chess_is_empty_at(chess, coord) &&
         chess_get_piece_at(chess, move).color != color)) {
      moves |= 1ULL << move;
    }
  }

  return moves;
}

void _chess_knight_moves_init(void) {
  for (uint8_t i = 0; i < 64; i++) {
    const bitboard_t knight = 1ULL << i;

    g_KNIGHT_MOVES[i] |= ((knight & ~(FILE_A | FILE_B)) << 6);
    g_KNIGHT_MOVES[i] |= ((knight & ~FILE_A) << 15);
    g_KNIGHT_MOVES[i] |= ((knight & ~(FILE_H | FILE_G)) << 10);
    g_KNIGHT_MOVES[i] |= ((knight & ~FILE_H) << 17);

    g_KNIGHT_MOVES[i] |= ((knight & ~(FILE_H | FILE_G)) >> 6);
    g_KNIGHT_MOVES[i] |= ((knight & ~FILE_H) >> 15);
    g_KNIGHT_MOVES[i] |= ((knight & ~(FILE_A | FILE_B)) >> 10);
    g_KNIGHT_MOVES[i] |= ((knight & ~FILE_A) >> 17);
  }
}

static bitboard_t _chess_moves_for_pawn(const bitboard_t pawn,
                                        const color_t color,
                                        const bitboard_t enemies) {
  if (color == COLOR_WHITE) {
    const bitboard_t single_push = (pawn << 8) & ~enemies;
    const bitboard_t double_push = ((single_push & RANK_3) << 8) & ~enemies;
    const bitboard_t capture = (pawn << 9 | pawn << 7) & enemies;
    return single_push | double_push | capture;
  } else {
    const bitboard_t single_push = (pawn >> 8) & ~enemies;
    const bitboard_t double_push = ((single_push & RANK_6) >> 8) & ~enemies;
    const bitboard_t capture = (pawn >> 9 | pawn >> 7) & enemies;
    return single_push | double_push | capture;
  }
}

static void _chess_update_castle_rights(chess_t *chess) {
  for (uint8_t color = 0; color < 2; color++) {
    const uint8_t rank = (color == COLOR_WHITE ? 0 : 7) * 8;
    const bool king_not_moved = piece_is_equal(
        chess->board[rank + 4], (piece_t){color, PIECE_KIND_KING});
    switch (color) {
    case COLOR_BLACK:
      chess->castle_rights &=
          ~BLACK_KING_SIDE_CASTLE_RIGHT |
          (king_not_moved && piece_is_equal(chess->board[rank + 7],
                                            (piece_t){color, PIECE_KIND_ROOK}));
      chess->castle_rights &= BLACK_QUEEN_SIDE_CASTLE_RIGHT | king_not_moved &&
                              piece_is_equal(chess->board[rank + 0],
                                             (piece_t){color, PIECE_KIND_ROOK});
      break;
    case COLOR_WHITE:
      chess->castle_rights &=
          ~WHITE_KING_SIDE_CASTLE_RIGHT |
          (king_not_moved && piece_is_equal(chess->board[rank + 7],
                                            (piece_t){color, PIECE_KIND_ROOK}));
      chess->castle_rights &= WHITE_QUEEN_SIDE_CASTLE_RIGHT | king_not_moved &&
                              piece_is_equal(chess->board[rank + 0],
                                             (piece_t){color, PIECE_KIND_ROOK});
      break;
    }
  }
}

static void _chess_board_from_fen(chess_t *chess, const char **const p_fen) {
  memset(chess->board, 0, sizeof(board_t));

  coord_t coord = 56;

  while (coord >= 0 && **p_fen && **p_fen != ' ') {
    if (**p_fen == '/') {
      coord -= 16;
    } else if (**p_fen >= '1' && **p_fen <= '8') {
      coord += **p_fen - '0';
    } else {
      piece_t piece = {.color = (**p_fen >= 'A' && **p_fen <= 'Z')
                                    ? COLOR_WHITE
                                    : COLOR_BLACK,
                       .kind = PIECE_KIND_NONE};

      switch (tolower(**p_fen)) {
      case 'p':
        piece.kind = PIECE_KIND_PAWN;
        break;
      case 'n':
        piece.kind = PIECE_KIND_KNIGHT;
        break;
      case 'b':
        piece.kind = PIECE_KIND_BISHOP;
        break;
      case 'r':
        piece.kind = PIECE_KIND_ROOK;
        break;
      case 'q':
        piece.kind = PIECE_KIND_QUEEN;
        break;
      case 'k':
        piece.kind = PIECE_KIND_KING;
        break;
      default:
        break;
      }

      if (piece.kind != PIECE_KIND_NONE && (coord % 8) < 8) {
        chess->board[coord] = piece;
        chess->bitboards[piece.color][piece.kind] |= 1ULL << coord;
      }

      coord++;
    }

    (*p_fen)++;
  }

  if (**p_fen == ' ')
    (*p_fen)++;
}

static void _chess_fen_parse_castle_rights(unsigned _BitInt(4) * castle_rights,
                                           const char **const p_fen) {
  while (**p_fen && **p_fen != ' ') {
    switch (**p_fen) {
    case 'K':
      *castle_rights |= WHITE_KING_SIDE_CASTLE_RIGHT;
      break;
    case 'Q':
      *castle_rights |= WHITE_QUEEN_SIDE_CASTLE_RIGHT;
      break;
    case 'k':
      *castle_rights |= BLACK_KING_SIDE_CASTLE_RIGHT;
      break;
    case 'q':
      *castle_rights |= BLACK_QUEEN_SIDE_CASTLE_RIGHT;
      break;
    }
    (*p_fen)++;
  }
  if (**p_fen == ' ')
    (*p_fen)++;
}

static void _chess_fen_parse_en_passant(coord_t *const en_passant,
                                        const char **const p_fen) {
  if (**p_fen == '-') {
    *en_passant = COORD_UNDEFINED;
    (*p_fen)++;
  } else {
    *en_passant = **p_fen - 'a';
    *en_passant += ('8' - *(*p_fen + 1)) * 8;
    (*p_fen) += 2;
  }
  if (**p_fen == ' ')
    (*p_fen)++;
}

static void _chess_fen_parse_half_move(uint8_t *const half_move,
                                       const char **const p_fen) {
  *half_move = atoi(*p_fen);
  while (**p_fen && **p_fen != ' ')
    (*p_fen)++;
}

static inline bool _coord_is_in_bounds(const coord_t coord) {
  return IS_IN_BOUNDS(coord / 8, 0, BOARD_WIDTH) &&
         IS_IN_BOUNDS(coord % 8, 0, BOARD_WIDTH);
}

static bitboard_t
_bitboard_merge(bitboard_t bitboards[static PIECE_KIND_COUNT + 1]) {
  return bitboards[PIECE_KIND_PAWN] | bitboards[PIECE_KIND_KNIGHT] |
         bitboards[PIECE_KIND_BISHOP] | bitboards[PIECE_KIND_ROOK] |
         bitboards[PIECE_KIND_QUEEN] | bitboards[PIECE_KIND_KING];
}
