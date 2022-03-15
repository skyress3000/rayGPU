'''

COE generator for framebuffer testing

This file generates a Xilinx COE memory configuration file used for testing display
output in conjunction iwth display_control_tester.vhd. The COE file should be used
as the configuration for the framebuffer RAM. When this file is run, it generates a
configuration file with an image with rows alternating between red, green, and blue,
with a gradient from full brightness at one end of the rows to black at the other. 
The .coe file is stored at the first command line argument passed to the program.
It can also be imported to call to_coe with a custom 8 bit RGB image to generate a
configuration file for that image.

Revision History:
    31 May 21  Ray Wendt         Initial revision.
    16 Jun 21  Ray Wendt         Added header comment.
  
'''

import numpy as np
import cv2 as cv
import sys

# 8 bit r, g, b values to packed 18 bit value (with 6 bits per channel)
def pack_18bit_color(r, g, b):
    # r into low 6 bits, g into middle 6 bits, b into high 6 bits
    return ((r >> 2) & 0b111111) + (((g >> 2) & 0b111111) << 6) + (((b >> 2) & 0b111111) << 12)

# export 8-bit RGB image to COE configuration
# assumes image is 160x128
def to_coe(img, filename):
    with open(filename, 'w') as coe_file:
        # write beginning of COE format, use hex radix
        coe_file.write('memory_initialization_radix = 16;\n')
        coe_file.write('memory_initialization_vector = ')

        # addressing in memory has y in the low bits
        vec_str = ''
        for x in range(160):
            for y in range(128):
                vec_str += f'{pack_18bit_color(*img[y,x]):05x} '

        # cut off last space and add a semicolon
        vec_str = vec_str[:-1] + ";"

        # write the entire memory config
        coe_file.write(vec_str)

# test image with horizontal gradients, alternating between color channels
def horiz_line_test():
    # generate empty image
    img = np.zeros((128,160,3), np.int8)

    # set relevant channel of each pixel with value scaled by x/160
    for x in range(160):
        for y in range(128):
            img[y,x,y % 3] = (x * 255) // 160

    return img

if __name__ == "__main__":
    to_coe(horiz_line_test(), sys.argv[1])