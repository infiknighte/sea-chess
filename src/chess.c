#include "chess.h"
#include "common.h"
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
static bitboard_t _chess_queen_moves_at(chess_t *chess, coord_t coord);
static bitboard_t _chess_rook_moves_at(chess_t *chess, coord_t coord);
static bitboard_t _chess_bishop_moves_at(chess_t *chess, coord_t coord);
static inline bool _coord_is_in_bounds(const coord_t coord);
static void _chess_update_castle_rights(chess_t *chess);
void _chess_precompute_king_moves(bitboard_t bitboards[64]);
void _chess_precompute_knight_moves(bitboard_t bitboards[64]);
bitboard_t _chess_moves_for_pawn_at(chess_t *chess, uint8_t coord);
static void _chess_update_attacks(chess_t *chess);
static bitboard_t _chess_pawn_attacks(bitboard_t pawn, color_t color,
                                      coord_t en_passant,
                                      bitboard_t opponent_occupied_squares);

void chess_from_fen(chess_t *const chess, const char *fen) {
  memset(chess, 0, sizeof(chess_t));
  _chess_board_from_fen(chess, &fen);
  chess->player = (*fen == 'w') ? 1 : 0;
  fen += 2;
  _chess_fen_parse_castle_rights(chess->castle, &fen);
  _chess_fen_parse_en_passant(&chess->en_passant, &fen);
  _chess_fen_parse_half_move(&chess->half_move, &fen);
  chess->full_move = atoi(fen);

  _chess_precompute_king_moves(chess->moves.king);
  _chess_precompute_knight_moves(chess->moves.knight);
  _chess_update_attacks(chess);
}

void chess_make_move(chess_t *const chess, const coord_t piece_coord,
                     const coord_t move) {
  if (chess->result == CHESS_RESULT_PROMOTION) {
    return;
  }

  const piece_t piece = chess->board[piece_coord];
  if (piece.kind == PIECE_KIND_NONE || piece.color != chess->player) {
    chess->result = CHESS_RESULT_ILLEGAL_MOVE;
    return;
  }

  _chess_update_attacks(chess);

  _chess_update_castle_rights(chess);

  bitboard_t legal_moves = chess_legal_moves_of(chess, piece_coord);
  const bool move_is_legal = legal_moves & NTH_BIT(move);

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

  bool is_en_passant = !coord_is_undefined(chess->en_passant);
  if (is_en_passant) {
    chess->en_passant = COORD_UNDEFINED;
  }

  if (piece.kind == PIECE_KIND_PAWN) {
    if (is_a_capture) {
      chess->half_move = 0;
    }

    // Fix en passant logic
    if (abs(coord_rank(piece_coord) - coord_rank(move)) == 2) {
      chess->en_passant =
          coord_change_rank(piece_coord, (piece.color ? 1 : -1));
    }

    _chess_update_attacks(chess);

    if (coord_rank(move) == (piece.color ? 7 : 0)) {
      chess->result = CHESS_RESULT_PROMOTION;
      return; // Avoid overwriting this result
    }
  }

  bool is_king_side_castle = false;
  const int8_t diff = coord_file(piece_coord) - coord_file(move);
  if (piece.kind == PIECE_KIND_KING &&
      (diff == 2 || (is_king_side_castle = diff == -2))) {
    const coord_t rook_coord =
        coord_change_file(piece_coord, is_king_side_castle ? 3 : -4);
    const coord_t move_rook =
        coord_change_file(move, is_king_side_castle ? -1 : 1);

    _chess_board_move_piece(chess, rook_coord, move_rook);
    chess->castle[piece.color][KING_SIDE_CASTLE] = false;
    chess->castle[piece.color][QUEEN_SIDE_CASTLE] = false;
    chess->result = CHESS_RESULT_CASTLE;
    return;
  }

  chess->player = !chess->player;

  if (chess_is_in_check(chess)) {
    chess->result = CHESS_RESULT_CHECK;
  } else if (is_a_capture) {
    chess->result = CHESS_RESULT_CAPTURE;
  } else {
    chess->result = CHESS_RESULT_OK;
  }
}

bool chess_promote(chess_t *chess, coord_t coord, piece_kind_t promotion_kind) {
  const piece_t piece = chess->board[coord];
  if (chess->result != CHESS_RESULT_PROMOTION &&
      (piece.kind != PIECE_KIND_PAWN ||
       coord_rank(coord) != (piece.color ? 7 : 0))) {
    return false;
  }
  chess->board[coord] = (piece_t){piece.color, promotion_kind};
  chess->result = CHESS_RESULT_OK;
  chess->player = !chess->player;

  return false;
}

bitboard_t chess_legal_moves_of(chess_t *const chess, const coord_t coord) {
  const piece_t piece = chess->board[coord];

  bitboard_t moves = piece.kind == PIECE_KIND_PAWN
                         ? _chess_moves_for_pawn_at(chess, coord)
                         : chess->attacks[piece.color][piece.kind][coord];

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

static inline void _chess_board_move_piece(chess_t *chess, coord_t origin,
                                           coord_t target) {
  piece_t *const current = &chess->board[origin];
  piece_t *const caputred = &chess->board[target];
  const bitboard_t target_bit = NTH_BIT(target);

  if (caputred->kind != PIECE_KIND_NONE) {
    chess->bitboards[caputred->color][caputred->kind] &= ~target_bit;
  }
  *caputred = *current;
  chess->bitboards[current->color][current->kind] &= ~NTH_BIT(origin);
  chess->bitboards[current->color][current->kind] |= target_bit;
  current->kind = PIECE_KIND_NONE;
}

static void _chess_update_attacks(chess_t *chess) {
  bitboard_t white_pieces =
      chess_occupied_squares_of(chess->bitboards, COLOR_WHITE);
  bitboard_t black_pieces =
      chess_occupied_squares_of(chess->bitboards, COLOR_BLACK);

  for (uint8_t i = 0; i < 64; i++) {
    piece_t piece = chess->board[i];
    if (piece.kind == PIECE_KIND_NONE) {
      continue;
    }

    color_t color = piece.color;
    bitboard_t ally_pieces =
        (color == COLOR_WHITE) ? white_pieces : black_pieces;
    bitboard_t enemy_pieces =
        (color == COLOR_BLACK) ? white_pieces : black_pieces;
    bitboard_t attack_mask = 0;

    switch (piece.kind) {
    case PIECE_KIND_PAWN:
      attack_mask = _chess_pawn_attacks(NTH_BIT(i), color, chess->en_passant,
                                        enemy_pieces);
      break;
    case PIECE_KIND_KNIGHT:
      attack_mask = chess->moves.knight[i] & ~ally_pieces;
      break;
    case PIECE_KIND_BISHOP:
      attack_mask = _chess_bishop_moves_at(chess, i);
      break;
    case PIECE_KIND_ROOK:
      attack_mask = _chess_rook_moves_at(chess, i);
      break;
    case PIECE_KIND_QUEEN:
      attack_mask = _chess_queen_moves_at(chess, i);
      break;
    case PIECE_KIND_KING:
      attack_mask = chess->moves.king[i] & ~ally_pieces;
      break;
    default:
      break;
    }
    chess->attacks[color][piece.kind][i] = attack_mask;
  }
}

static bitboard_t _chess_queen_moves_at(chess_t *const chess,
                                        const coord_t coord) {
  bitboard_t moves = 0;
  const color_t color = chess->board[coord].color;
  const int8_t delta_rank[8] = {-1, -1, -1, 0, 0, 1, 1, 1};
  const int8_t delta_file[8] = {-1, 0, 1, -1, 1, -1, 0, 1};

  for (uint8_t i = 0; i < 8; i++) {
    coord_t move = coord_change(coord, delta_rank[i], delta_file[i]);

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves |= NTH_BIT(move);
      move = coord_change(move, delta_rank[i], delta_file[i]);
    }

    if (!_coord_is_in_bounds(move))
      continue;

    if (chess->board[move].color != color) {
      moves |= NTH_BIT(move);
    }
  }
  return moves;
}

static bitboard_t _chess_rook_moves_at(chess_t *const chess,
                                       const coord_t coord) {
  bitboard_t moves = 0;
  const color_t color = chess->board[coord].color;
  const int8_t delta_rank[4] = {-1, 1, 0, 0};
  const int8_t delta_file[4] = {0, 0, -1, 1};

  for (uint8_t i = 0; i < 4; i++) {
    coord_t move = coord_change(coord, delta_rank[i], delta_file[i]);

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves |= NTH_BIT(move);
      move = coord_change(move, delta_rank[i], delta_file[i]);
    }
    if (_coord_is_in_bounds(move) && (!chess_is_empty_at(chess, move) &&
                                      chess->board[move].color != color)) {
      moves |= NTH_BIT(move);
    }
  }
  return moves;
}

static bitboard_t _chess_bishop_moves_at(chess_t *const chess,
                                         const coord_t coord) {
  bitboard_t moves = 0;
  const color_t color = chess->board[coord].color;
  const int8_t delta_rank[4] = {-1, -1, 1, 1};
  const int8_t delta_file[4] = {-1, 1, -1, 1};

  for (uint8_t i = 0; i < 4; i++) {
    coord_t move = coord_change(coord, delta_rank[i], delta_file[i]);

    while (_coord_is_in_bounds(move) && chess_is_empty_at(chess, move)) {
      moves |= NTH_BIT(move);
      move = coord_change(move, delta_rank[i], delta_file[i]);
    }

    if (_coord_is_in_bounds(move) && chess->board[move].color != color) {
      moves |= NTH_BIT(move);
    }
  }

  return moves;
}

static void _chess_update_castle_rights(chess_t *chess) {
  for (uint8_t color = 0; color < 2; color++) {
    const uint8_t rank = color == COLOR_WHITE ? 0 : 7;
    const bool king_not_moved = piece_is_equal(
        chess->board[rank + 4], (piece_t){color, PIECE_KIND_KING});
    chess->castle[color][KING_SIDE_CASTLE] &=
        king_not_moved && piece_is_equal(chess->board[rank + 7],
                                         (piece_t){color, PIECE_KIND_ROOK});
    chess->castle[color][QUEEN_SIDE_CASTLE] &=
        king_not_moved &&
        piece_is_equal(chess->board[rank], (piece_t){color, PIECE_KIND_ROOK});
  }
}

static void _chess_board_from_fen(chess_t *chess, const char **const p_fen) {
  memset(chess->board, 0, sizeof(board_t));
  coord_t coord = BOARD_AREA - BOARD_WIDTH; // Start from the top-left square

  while (**p_fen && **p_fen != ' ') {
    if (**p_fen == '/') {
      coord -= BOARD_WIDTH * 2 - coord_file(coord);
    } else if (**p_fen >= '1' && **p_fen <= '8') {
      coord += **p_fen - '0'; // Move right by the empty squares count
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
      }

      if (piece.kind != PIECE_KIND_NONE) {
        chess->bitboards[piece.color][piece.kind] |= NTH_BIT(coord);
      }
      chess->board[coord] = piece;
      coord++;
    }
    (*p_fen)++;
  }

  if (**p_fen == ' ') {
    (*p_fen)++;
  }
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
    *en_passant = coord_new(**p_fen - 'a', '8' - *(*p_fen + 1));
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
  return IS_IN_BOUNDS(coord, 0, 64);
}

bitboard_t _chess_moves_for_pawn_at(chess_t *chess, uint8_t coord) {
  const color_t color = chess->player;
  bitboard_t pawn = chess->bitboards[color][PIECE_KIND_PAWN] & NTH_BIT(coord);
  if (!pawn) {
    return 0;
  }

  const bitboard_t opponent_occupied_squares =
      chess_occupied_squares_of(chess->bitboards, !color);

  const bitboard_t empty_squares =
      ~(opponent_occupied_squares |
        chess_occupied_squares_of(chess->bitboards, color));

  bitboard_t pushes = (color ? pawn << 8 : pawn >> 8) & empty_squares;
  pushes |=
      (color ? (pawn & RANK_2) << 16 : (pawn & RANK_7) >> 16) & empty_squares;

  bitboard_t captures = chess->attacks[color][PIECE_KIND_PAWN][coord];

  return pushes | captures;
}

static bitboard_t _chess_pawn_attacks(bitboard_t pawn, color_t color,
                                      coord_t en_passant,
                                      bitboard_t opponent_occupied_squares) {
  const bitboard_t en_passant_bit =
      en_passant != COORD_UNDEFINED ? NTH_BIT(en_passant) : 0;
  bitboard_t captures = (color ? pawn << 7 : pawn >> 7) &
                        (opponent_occupied_squares | en_passant_bit) & ~FILE_H;
  captures |= (color ? pawn << 9 : pawn >> 9) &
              (opponent_occupied_squares | en_passant_bit) & ~FILE_A;
  return captures;
}

void _chess_precompute_king_moves(bitboard_t bitboards[64]) {
  memset(bitboards, 0, 64 * sizeof(bitboard_t));

  for (uint8_t i = 0; i < 64; i++) {
    bitboard_t pos = NTH_BIT(i);

    bitboards[i] |= (pos << 8) | (pos >> 8);

    bitboards[i] |= (pos << 1) & ~FILE_A;
    bitboards[i] |= (pos >> 1) & ~FILE_H;

    bitboards[i] |= (pos << 9) & ~FILE_A;
    bitboards[i] |= (pos << 7) & ~FILE_H;

    bitboards[i] |= (pos >> 7) & ~FILE_H;
    bitboards[i] |= (pos >> 9) & ~FILE_A;
  }
}

void _chess_precompute_knight_moves(bitboard_t bitboards[static 64]) {
  memset(bitboards, 0, 64 * sizeof(bitboard_t));

  for (uint8_t i = 0; i < 64; i++) {
    bitboard_t pos = NTH_BIT(i);

    bitboards[i] |= ((pos & ~(FILE_A | FILE_B)) << 6);
    bitboards[i] |= ((pos & ~FILE_A) << 15);
    bitboards[i] |= ((pos & ~(FILE_H | FILE_G)) << 10);
    bitboards[i] |= ((pos & ~FILE_H) << 17);

    bitboards[i] |= ((pos & ~(FILE_H | FILE_G)) >> 6);
    bitboards[i] |= ((pos & ~FILE_H) >> 15);
    bitboards[i] |= ((pos & ~(FILE_A | FILE_B)) >> 10);
    bitboards[i] |= ((pos & ~FILE_A) >> 17);
  }
}

bitboard_t _bitboard_generate_rook_movement_mask(coord_t coord) {

  const bitboard_t RANKS[8] = {RANK_1, RANK_2, RANK_3, RANK_4,
                               RANK_5, RANK_6, RANK_7, RANK_8};
  const bitboard_t FILES[8] = {FILE_A, FILE_B, FILE_C, FILE_D,
                               FILE_E, FILE_F, FILE_G, FILE_H};
  const bitboard_t EDGES = RANK_1 | RANK_8 | FILE_A | FILE_H;

  return (RANKS[coord_rank(coord)] ^ FILES[coord_file(coord)]) & ~EDGES;
}

uint8_t _bitboard_relevant_bits(bitboard_t b) {
  uint8_t r = 0;
  while (b) {
    r++;
    b &= b - 1;
  }
  return r;
}

static const uint8_t BIT_TABLE[64] = {
    63, 30, 3,  32, 25, 41, 22, 33, 15, 50, 42, 13, 11, 53, 19, 34,
    61, 29, 2,  51, 21, 43, 45, 10, 18, 47, 1,  54, 9,  57, 0,  35,
    62, 31, 40, 4,  49, 5,  52, 26, 60, 6,  23, 44, 46, 27, 56, 16,
    7,  39, 48, 24, 59, 14, 12, 55, 38, 28, 58, 20, 37, 17, 36, 8};

uint8_t pop_first_bit(bitboard_t *bb) {
  bitboard_t b = *bb ^ (*bb - 1);
  uint32_t fold = (uint32_t)((b & 0xffffffff) ^ (b >> 32));
  *bb &= (*bb - 1);
  return BIT_TABLE[(fold * 0x783a9b23) >> 26];
}

bitboard_t _bitboard_generate_rook_blockermask(coord_t index, uint8_t relevant,
                                               bitboard_t m) {
  uint8_t i, j;
  bitboard_t result = 0ULL;
  for (i = 0; i < relevant; i++) {
    j = pop_first_bit(&m);
    if (index & (1 << i))
      result |= (1ULL << j);
  }
  return result;
}

bitboard_t _bitboard_generate_rook_attack(coord_t sq, bitboard_t blockers) {
  bitboard_t attacks = 0ULL;

  coord_t rk = coord_rank(sq);
  coord_t fl = coord_file(sq);

  int8_t delta[2] = {-1, 1};

  for (uint8_t i = 0; i < 2; i++) {
    int8_t d = delta[i];
    coord_t s = 0;

    for (coord_t r = rk + d; r >= 0; r += d) {
      s = r * 8 + fl;
      bitboard_t sbit = NTH_BIT(s);
      attacks |= sbit;
      if (blockers & sbit)
        break;
    }

    for (coord_t f = fl + d; f >= 0; f += d) {
      s = rk * 8 + fl;
      bitboard_t sbit = NTH_BIT(s);
      attacks |= sbit;
      if (blockers & sbit)
        break;
    }
  }

  return attacks;
}

void _chess_setup_blockers_and_attacks_for_rook(
    bitboard_t blocker[BOARD_AREA][MAX_ROOK_BLOCKER_PERM],
    bitboard_t attacks[BOARD_AREA][MAX_ROOK_BLOCKER_PERM]) {

  for (uint8_t square = 0; square < BOARD_AREA; square++) {
    bitboard_t movement_mask = _bitboard_generate_rook_movement_mask(square);
    uint8_t relevant = _bitboard_relevant_bits(movement_mask);
    for (uint16_t index = 0; index < (1 << relevant); index++) {
      blocker[square][index] =
          _bitboard_generate_rook_blockermask(index, relevant, movement_mask);
      attacks[square][index] =
          _bitboard_generate_rook_attack(square, blocker[square][index]);
    }
  }
}

uint64_t rand64(void) {
  return (uint64_t)rand() | ((uint64_t)rand() << 15) |
         ((uint64_t)rand() << 30) | ((uint64_t)rand() << 45) |
         ((uint64_t)rand() << 60);
}

uint64_t randm(void) { return rand64() & rand64() & rand64(); }

bitboard_t find_magic_number(coord_t square,
                             bitboard_t blockers[static MAX_ROOK_BLOCKER_PERM],
                             bitboard_t attacks[MAX_ROOK_BLOCKER_PERM],
                             uint8_t relevant) {
  for (uint32_t attempt = 0; attempt < 1000000; attempt++) {
    bitboard_t magic = randm();

    bitboard_t used[MAX_ROOK_BLOCKER_PERM] = {0};

    bool fail = false;
    for (int i = 0; i < (1 << relevant); i++) {
      int index = (blockers[i] * magic) >> (64 - relevant);
      if (used[index] == 0) {
        used[index] = attacks[i];
      } else if (used[index] != attacks[i]) {
        fail = true;
        break;
      }
    }

    if (!fail) {
      return magic;
    }
  }

  return 0;
}
