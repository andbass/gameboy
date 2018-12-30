#pragma once

#include "types.h"

#define LCD_WIDTH 160
#define LCD_HEIGHT 144

typedef struct LCD {
    u8 pixels[LCD_HEIGHT][LCD_WIDTH];
} LCD;