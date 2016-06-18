#include <stdio.h>
#include <stdint.h>

#define FIXNUM_MASK 3
#define FIXNUM_TAG  0
#define BOOL_MASK   0b01111111
#define BOOL_TAG    0b00011111
#define CHAR_MASK   0b11111111
#define CHAR_TAG    0b00001111
#define PAIR_MASK   0b111
#define PAIR_TAG    0b001

#define IS_FIXNUM(x) ((x & FIXNUM_MASK) == FIXNUM_TAG)
#define CONVERT_FIXNUM(x) ((x >> 2))

#define IS_EMPTY_LIST(x) ((x == 0b00101111))

#define IS_BOOL(x) ((x & BOOL_MASK) == BOOL_TAG)
#define CONVERT_BOOL(x) ((x >> 7))

#define IS_CHAR(x) ((x & CHAR_MASK) == CHAR_TAG)
#define CONVERT_CHAR(x) ((x >> 8))

#define IS_PAIR(x) ((x & PAIR_MASK) == PAIR_TAG)
#define CAR(x) (*((int32_t*) (uintptr_t) (x & ~PAIR_MASK)))
#define CDR(x) (*(((int32_t*) (uintptr_t) (x & ~PAIR_MASK)) + 1))

char memory[1000];

int scheme_entry(void*);

void print(int);

void print(int val) {
  if (IS_FIXNUM(val)) {
    printf("%d", CONVERT_FIXNUM(val));
  }
  else if (IS_EMPTY_LIST(val)) {
    printf("()");
  }
  else if (IS_BOOL(val)) {
    if (CONVERT_BOOL(val)) {
      printf("#t");
    }
    else {
      printf("#f");
    }
  }
  else if (IS_CHAR(val)) {
    char c = CONVERT_CHAR(val);
    if (c == '\n') {
      printf("#\\newline");
    }
    else if (c == ' ') {
      printf("#\\space");
    }
    else {
      printf("#\\%c", (char) CONVERT_CHAR(val));
    }
  }
  else if (IS_PAIR(val)) {
    printf("(");
    print(CAR(val));
    printf(" ");
    print(CDR(val));
    printf(")");
  }
  else {
    printf("Heap location: %p\n", memory);
    printf("0x%x\n", val);
  }
}

int main() {
  int val = scheme_entry(memory);
  print(val);

  return 0;
}
