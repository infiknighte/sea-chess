#ifndef COMMON_H
#define COMMON_H

#include <stdbool.h>
#include <stdint.h>

#define BOARD_WIDTH 8

#define IS_IN_BOUNDS(x, a, b) ((x) >= (a) && (x) < (b))

#define BIT(sq) ((bitboard_t)(1ULL << (sq)))

#endif
