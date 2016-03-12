#include <stdio.h>

#include "gb.h"

int main(int argc, char* argv[]) {
    GameBoy gb;
    printf("%x\n", gb.reg.a);
}
