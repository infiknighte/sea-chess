#include "chess.h"
#include "common.h"
#include <raylib.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

static inline void _chess_board_move_piece(chess_t *chess, coord_t from,
                                           coord_t to);
static void _chess_board_from_fen(chess_t *chess, const char **p_fen);
static void _chess_fen_parse_castle_rights(bool castle[static 2][2],
                                           const char **p_fen);
static void _chess_fen_parse_en_passant(coord_t *en_passant,
                                        const char **p_fen);
static void _chess_fen_parse_half_move(uint8_t *const half_move,
                                       const char **p_fen);
static moves_t _chess_legal_moves_of_king(chess_t *chess, coord_t coord);
static moves_t _chess_legal_moves_of_queen(chess_t *chess, coord_t coord);
static moves_t _chess_legal_moves_of_rook(chess_t *chess, coord_t coord);
static moves_t _chess_legal_moves_of_bishop(chess_t *chess, coord_t coord);
static moves_t _chess_legal_moves_of_knight(chess_t *chess, coord_t coord);
static moves_t _chess_legal_moves_of_pawn(chess_t *chess, coord_t coord);
static inline bool _coord_is_in_bounds(const coord_t coord);
static void _chess_update_castle_rights(chess_t *chess);

void chess_from_fen(chess_t *const chess, const char *fen) {
  memset(chess, 0, sizeof(chess_t));
  _chess_board_from_fen(chess, &fen);
  chess->player = (*fen == 'w') ? 1 : 0;
  fen += 2;
  _chess_fen_parse_castle_rights(chess->castle, &fen);
  _chess_fen_parse_en_passant(&chess->en_passant, &fen);
  _chess_fen_parse_half_move(&chess->half_move, &fen);
  chess->full_move = atoi(fen);
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

  bool move_is_legal = false;
  moves_t legal_moves = chess_legal_moves_of(chess, piece_coord);
  for (uint8_t i = 0; i < legal_moves.count; i++) {
    if (coord_is_equal(legal_moves.ptr[i], move)) {
      move_is_legal = true;
      break;
    }
  }
  moves_free(&legal_moves);
  if (!move_is_legal) {
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

  if (piece.kind == PIECE_KIND_PAWN && move.rank == (piece.color ? 7 : 0)) {
    chess->result = CHESS_RESULT_PROMOTION;
  }

  bool is_en_passant = !coord_is_undefined(chess->en_passant);
  if (is_en_passant) {
    if (coord_is_equal(move, chess->en_passant)) {
      chess->board[piece_coord.rank][move.file].kind = PIECE_KIND_NONE;
    }
    chess->en_passant = COORD_UNDEFINED;
  }
  if (piece.kind == PIECE_KIND_PAWN &&
      piece_coord.rank == move.rank + (piece.color ? -2 : 2)) {
    chess->en_passant =
        (coord_t){piece_coord.rank + (piece.color ? 1 : -1), piece_coord.file};
  }

  bool is_king_side_castle = false;
  const int8_t diff = piece_coord.file - move.file;
  if (piece.kind == PIECE_KIND_KING &&
      (diff == 2 || (is_king_side_castle = diff == -2))) {
    const coord_t rook_coord = {move.rank,
                                move.file + (is_king_side_castle ? 1 : -2)};
    const coord_t move_rook = {move.rank, is_king_side_castle ? 5 : 3};

    _chess_board_move_piece(chess, rook_coord, move_rook);
    chess->castle[piece.color][KING_SIDE_CASTLE] = false;
    chess->castle[piece.color][QUEEN_SIDE_CASTLE] = false;
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
      (piece.kind != PIECE_KIND_PAWN || coord.rank != (piece.color ? 7 : 0))) {
    return false;
  }
  chess->board[coord.rank][coord.file] = (piece_t){piece.color, promotion_kind};
  chess->result = CHESS_RESULT_OK;
  chess->player = !chess->player;

  return false;
}

moves_t chess_legal_moves_of(chess_t *const chess, const coord_t origin) {
  const piece_t piece = chess_get_piece_at(chess, origin);
  moves_t moves;
  moves_init(&moves);
  switch (piece.kind) {
  case PIECE_KIND_KING:
    moves = _chess_legal_moves_of_king(chess, origin);
    break;
  case PIECE_KIND_QUEEN:
    moves = _chess_legal_moves_of_queen(chess, origin);
    break;
  case PIECE_KIND_ROOK:
    moves = _chess_legal_moves_of_rook(chess, origin);
    break;
  case PIECE_KIND_BISHOP:
    moves = _chess_legal_moves_of_bishop(chess, origin);
    break;
  case PIECE_KIND_KNIGHT:
    moves = _chess_legal_moves_of_knight(chess, origin);
    break;
  case PIECE_KIND_PAWN:
    moves = _chess_legal_moves_of_pawn(chess, origin);
    break;
  default:
    break;
  }
  return moves;
}

bool chess_is_in_check(chess_t *const chess) { return false; }

void moves_init(moves_t *const moves) {
  moves->count = 0;
  moves->capacity = 0;
  moves->ptr = NULL;
}

void moves_push(moves_t *moves, coord_t move) {
  if (moves->count >= moves->capacity) {
    const uint32_t new_capacity = moves->capacity ? moves->capacity * 2 : 16;
    void *temp = realloc(moves->ptr, new_capacity * sizeof(coord_t));
    if (!temp) {
      perror("realocation failed");
      return;
    }
    moves->ptr = temp;
    moves->capacity = new_capacity;
  }
  moves->ptr[moves->count++] = move;
}

void moves_free(moves_t *const moves) {
  free(moves->ptr);
  moves_init(moves);
}

static inline void _chess_board_move_piece(chess_t *chess, coord_t from,
                                           coord_t to) {
  chess->board[to.rank][to.file] = chess->board[from.rank][from.file];
  chess->board[from.rank][from.file].kind = PIECE_KIND_NONE;
}

static void _chess_update_attacks(chess_t *chess) {
  for (uint8_t i = 0; i < 2; i++) {
    for (uint8_t j = 0; j < PIECE_KIND_COUNT; j++) {
    }
  }
}

static moves_t _chess_legal_moves_of_king(chess_t *const chess, coord_t coord) {
  moves_t moves;
  moves_init(&moves);
  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
  const int8_t delta_file[8] = {-1, 0, 1, -1, 1, -1, 0, 1};

  for (uint8_t i = 0; i < 8; i++) {
    coord_t move = {coord.rank + delta_rank[i], coord.file + delta_file[i]};
    if (_coord_is_in_bounds(move) &&
        (chess_is_empty_at(chess, move) ||
         chess_get_piece_at(chess, move).color != color)) {
      moves_push(&moves, move);
    }
  }

  const uint8_t rank = color ? 0 : 7;
  if (chess->castle[color][KING_SIDE_CASTLE] &&
      chess_is_empty_at(chess, (coord_t){rank, 6}) &&
      chess_is_empty_at(chess, (coord_t){rank, 5})) {
    moves_push(&moves, (coord_t){rank, 6});
  }
  if (chess->castle[color][QUEEN_SIDE_CASTLE] &&
      chess_is_empty_at(chess, (coord_t){rank, 1}) &&
      chess_is_empty_at(chess, (coord_t){rank, 2}) &&
      chess_is_empty_at(chess, (coord_t){rank, 3})) {

    moves_push(&moves, (coord_t){rank, 2});
  }

  return moves;
}

static moves_t _chess_legal_moves_of_queen(chess_t *const chess,
                                           const coord_t coord) {
  moves_t moves;
  moves_init(&moves);
  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
  const int8_t delta_file[8] = {-1, 0, 1, -1, 1, -1, 0, 1};

  for (uint8_t i = 0; i < 8; i++) {
    coord_t move = {coord.rank + delta_rank[i], coord.file + delta_file[i]};

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves_push(&moves, move);
      move.rank += delta_rank[i];
      move.file += delta_file[i];
    }
    if (_coord_is_in_bounds(move) &&
        (!chess_is_empty_at(chess, move) &&
         chess_get_piece_at(chess, move).color != color)) {
      moves_push(&moves, move);
    }
  }
  return moves;
}
static moves_t _chess_legal_moves_of_rook(chess_t *const chess,
                                          const coord_t coord) {
  moves_t moves;
  moves_init(&moves);

  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[4] = {-1, 1, 0, 0};
  const int8_t delta_file[4] = {0, 0, -1, 1};

  for (uint8_t i = 0; i < 4; i++) {
    coord_t move = {coord.rank + delta_rank[i], coord.file + delta_file[i]};

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves_push(&moves, move);
      move.rank += delta_rank[i];
      move.file += delta_file[i];
    }
    if (_coord_is_in_bounds(move) &&
        (!chess_is_empty_at(chess, move) &&
         chess_get_piece_at(chess, move).color != color)) {
      moves_push(&moves, move);
    }
  }
  return moves;
}

static moves_t _chess_legal_moves_of_bishop(chess_t *const chess,
                                            const coord_t coord) {
  moves_t moves;
  moves_init(&moves);

  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[4] = {-1, -1, 1, 1};
  const int8_t delta_file[4] = {-1, 1, -1, 1};

  for (uint8_t i = 0; i < 4; i++) {
    coord_t move = {coord.rank + delta_rank[i], coord.file + delta_file[i]};

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves_push(&moves, move);
      move.rank += delta_rank[i];
      move.file += delta_file[i];
    }

    if (_coord_is_in_bounds(move) &&
        (!chess_is_empty_at(chess, coord) &&
         chess_get_piece_at(chess, move).color != color)) {
      moves_push(&moves, move);
    }
  }

  return moves;
}

static moves_t _chess_legal_moves_of_knight(chess_t *const chess,
                                            const coord_t coord) {
  moves_t moves;
  moves_init(&moves);

  const color_t color = chess_get_piece_at(chess, coord).color;
  const int8_t delta_rank[8] = {-2, -2, -1, -1, 1, 1, 2, 2};
  const int8_t delta_file[8] = {-1, 1, -2, 2, -2, 2, -1, 1};

  for (uint8_t i = 0; i < 8; i++) {
    const coord_t move = {coord.rank + delta_rank[i],
                          coord.file + delta_file[i]};

    if (_coord_is_in_bounds(move) &&
        (chess_is_empty_at(chess, move) ||
         chess_get_piece_at(chess, move).color != color)) {
      moves_push(&moves, move);
    }
  }

  return moves;
}

static moves_t _chess_legal_moves_of_pawn(chess_t *const chess,
                                          const coord_t coord) {
  moves_t moves;
  moves_init(&moves);
  const color_t color = chess_get_piece_at(chess, coord).color;
  int8_t forward = color == COLOR_WHITE ? 1 : -1;
  coord_t move = {coord.rank + forward, coord.file};
  if (chess_is_empty_at(chess, move)) {
    moves_push(&moves, move);
    move.rank += forward;
    if (chess_is_empty_at(chess, move) &&
        ((color == COLOR_WHITE && coord.rank == 1) ||
         (color == COLOR_BLACK && coord.rank == 6))) {
      moves_push(&moves, move);
    }
  }
  int8_t delta[2] = {-1, 1};
  for (uint8_t i = 0; i < 2; i++) {
    const coord_t move = {coord.rank + forward, coord.file + delta[i]};
    if (_coord_is_in_bounds(move)) {
      const piece_t piece = chess_get_piece_at(chess, move);
      const color_t en_passant_color =
          (chess->en_passant.rank == 2) ? COLOR_WHITE : COLOR_BLACK;
      if ((coord_is_equal(move, chess->en_passant) &&
           en_passant_color != color) ||
          (piece.kind != PIECE_KIND_NONE && color != piece.color)) {
        moves_push(&moves, move);
      }
    }
  }

  return moves;
}

static void _chess_update_castle_rights(chess_t *chess) {
  for (uint8_t color = 0; color < 2; color++) {
    const uint8_t rank = color == COLOR_WHITE ? 0 : 7;
    const bool king_not_moved = piece_is_equal(
        chess->board[rank][4], (piece_t){color, PIECE_KIND_KING});
    chess->castle[color][KING_SIDE_CASTLE] &=
        king_not_moved && piece_is_equal(chess->board[rank][7],
                                         (piece_t){color, PIECE_KIND_ROOK});
    chess->castle[color][QUEEN_SIDE_CASTLE] &=
        king_not_moved && piece_is_equal(chess->board[rank][0],
                                         (piece_t){color, PIECE_KIND_ROOK});
  }
}

static void _chess_board_from_fen(chess_t *chess, const char **const p_fen) {
  memset(chess->board, 0, sizeof(board_t));
  coord_t coord = {7, 0};
  while (**p_fen && **p_fen != ' ') {
    if (**p_fen == '/') {
      coord.rank--;
      coord.file = 0;
    } else if (**p_fen >= '1' && **p_fen <= '8') {
      coord.file += **p_fen - '0';
    } else {
      piece_t piece = {.color = (**p_fen >= 'A' && **p_fen <= 'Z')
                                    ? COLOR_WHITE
                                    : COLOR_BLACK,
                       .kind = PIECE_KIND_NONE};
      switch (**p_fen) {
      case 'P':
      case 'p':
        piece.kind = PIECE_KIND_PAWN;
        break;
      case 'N':
      case 'n':
        piece.kind = PIECE_KIND_KNIGHT;
        break;
      case 'B':
      case 'b':
        piece.kind = PIECE_KIND_BISHOP;
        break;
      case 'R':
      case 'r':
        piece.kind = PIECE_KIND_ROOK;
        break;
      case 'Q':
      case 'q':
        piece.kind = PIECE_KIND_QUEEN;
        break;
      case 'K':
      case 'k':
        piece.kind = PIECE_KIND_KING;
        break;
      default:
        break;
      }

      if (piece.kind != PIECE_KIND_NONE) {
        chess->bitboards[piece.color][piece.kind] |=
            NTH_BIT(coord.rank * 8 + coord.file);
      }
      chess->board[coord.rank][coord.file] = piece;
      coord.file++;
    }
    (*p_fen)++;
  }
  if (**p_fen == ' ')
    (*p_fen)++;
}

static void _chess_fen_parse_castle_rights(bool castle[static 2][2],
                                           const char **const p_fen) {
  while (**p_fen && **p_fen != ' ') {
    switch (**p_fen) {
    case 'K':
      castle[COLOR_WHITE][KING_SIDE_CASTLE] = true;
      break;
    case 'Q':
      castle[COLOR_WHITE][QUEEN_SIDE_CASTLE] = true;
      break;
    case 'k':
      castle[COLOR_BLACK][KING_SIDE_CASTLE] = true;
      break;
    case 'q':
      castle[COLOR_BLACK][QUEEN_SIDE_CASTLE] = true;
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
    en_passant->file = **p_fen - 'a';
    en_passant->rank = '8' - *(*p_fen + 1);
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
  return IS_IN_BOUNDS(coord.rank, 0, BOARD_WIDTH) &&
         IS_IN_BOUNDS(coord.file, 0, BOARD_WIDTH);
}
