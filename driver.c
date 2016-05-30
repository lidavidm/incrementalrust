#include <stdio.h>

#define FIXNUM_MASK 3
#define FIXNUM_TAG  0
#define BOOL_MASK 0b1111111
#define BOOL_TAG 0b0011111

#define IS_FIXNUM(x) ((x & FIXNUM_MASK) == FIXNUM_TAG)
#define CONVERT_FIXNUM(x) ((x >> 2))

#define IS_EMPTY_LIST(x) ((x == 0b00101111))

#define IS_BOOL(x) ((x & BOOL_MASK) == BOOL_TAG)
#define CONVERT_BOOL(x) ((x >> 7))

int main(int argc, char **argv) {
  int val = scheme_entry();
  if (IS_FIXNUM(val)) {
    printf("%d\n", CONVERT_FIXNUM(val));
  }
  else if (IS_EMPTY_LIST(val)) {
    printf("()\n");
  }
  else if (IS_BOOL(val)) {
    if (CONVERT_BOOL(val)) {
      printf("#t\n");
    }
    else {
      printf("#f\n");
    }
  }
  return 0;
}
