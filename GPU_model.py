'''

GPU_model.py

This file implements a full model of the GPU system in python as well as the triangle
and sphere demo programs described in gpu_control.ino. While it does not print a complete
set of outputs that the GPU can be tested against (which would not be accurate because
numpy floating point math is full IEEE 754), the intended use is to add print statements
at various stages to compare against simulation waveforms from rayGPU_tb.vhd. It can also
be used as a testbench for shader programs and vertex sets before programming the real
GPU with them. The file also includes a function test_inv_lookup to compare several methods
of computing reciprocals, and print the % error of the lookup table method against full
precision reciprocal.

Revision History:
     8 Apr 21  Ray Wendt        Initial Revision
    14 Apr 21  Ray Wendt        Added triangle rasterization
    15 Apr 21  Ray Wendt        Fixed triangle pixel loop indices
    15 Apr 21  Ray Wendt        Added rotating cube demo program + related 
                                utilities to run it
    18 Apr 21  Ray Wendt        Added fragment shading and triangle demo
    18 Apr 21  Ray Wendt        Fixed math in triangle rasterization
    18 Apr 21  Ray Wendt        Added (better) reciprocal calculation
    13 Jun 21  Ray Wendt        Added sphere demo
    16 Jun 21  Ray Wendt        Added header comment

'''

import numpy as np
import colorsys
from matplotlib import pyplot as plt

# --- demo program utilities ---

# multiplies the input coordinates in IN0 by a matrix with columns stored in the 
# constant registers, putting the result in OUT0
#
# to be used for rotation + perspective transformation
CUBE_DEMO_VERT_SHADER = [
    0b000000000000001_0100111001000_0000000000000_0001111, # MUL C0, IN0.xxxx -> R0
    0b000000000000001_0101111001000_0000010101010_0011111, # MUL C1, IN0.yyyy -> R1
    0b000000000000010_1001111001000_1000111001000_0001111, # ADD R1, R0       -> R0
    0b000000000000001_0110111001000_0000101010100_0011111, # MUL C2, IN0.zzzz -> R1
    0b000000000000010_1001111001000_1000111001000_0001111, # ADD R1, R0       -> R0
    0b000000000000001_0111111001000_0000111111110_0011111, # MUL C3, IN0.wwww -> R1
    0b000000000000010_1001111001000_1000111001000_1001111, # ADD R1, R0       -> OUT0
    0b000000000000101_0000000000000_0000000000000_0000000, # END
]

# same as cube demo vertex shader, but forwards the color attribute to the shaded vertex
TRI_DEMO_VERT_SHADER = [
    0b000000000000001_0100111001000_0000000000000_0001111, # MUL C0, IN0.xxxx -> R0
    0b000000000000001_0101111001000_0000010101010_0011111, # MUL C1, IN0.yyyy -> R1
    0b000000000000010_1001111001000_1000111001000_0001111, # ADD R1, R0       -> R0
    0b000000000000001_0110111001000_0000101010100_0011111, # MUL C2, IN0.zzzz -> R1
    0b000000000000010_1001111001000_1000111001000_0001111, # ADD R1, R0       -> R0
    0b000000000000001_0111111001000_0000111111110_0011111, # MUL C3, IN0.wwww -> R1
    0b000000000000010_1001111001000_1000111001000_1001111, # ADD R1, R0       -> OUT0
    0b000000000000000_0000000000000_0001111001000_1011111, # MOV IN1          -> OUT1
    0b000000000000101_0000000000000_0000000000000_0000000, # END
]

# same as cube demo vertex shader, but forwards the color and normal attributes to the shaded vertex
NORM_DEMO_VERT_SHADER = [
    0b000000000000001_0100111001000_0000000000000_0001111, # MUL C0, IN0.xxxx -> R0
    0b000000000000001_0101111001000_0000010101010_0011111, # MUL C1, IN0.yyyy -> R1
    0b000000000000010_1001111001000_1000111001000_0001111, # ADD R1, R0       -> R0
    0b000000000000001_0110111001000_0000101010100_0011111, # MUL C2, IN0.zzzz -> R1
    0b000000000000010_1001111001000_1000111001000_0001111, # ADD R1, R0       -> R0
    0b000000000000001_0111111001000_0000111111110_0011111, # MUL C3, IN0.wwww -> R1
    0b000000000000010_1001111001000_1000111001000_1001111, # ADD R1, R0       -> OUT0
    0b000000000000000_0000000000000_0001111001000_1011111, # MOV IN1          -> OUT1
    0b000000000000000_0000000000000_0010111001000_1101111, # MOV IN2          -> OUT2
    0b000000000000101_0000000000000_0000000000000_0000000, # END
]

# fragment shader program which outputs the color given by the vertex attribute at 
# index 1
VERT_COLOR_FRAG_SHADER = [
    0b000000000000000_0000000000000_0001111001000_1011111, # MOV IN1 -> OUT1
    0b000000000000101_0000000000000_0000000000000_0000000, # END
]

# fragment shader program which outputs the color given by the vertex attribute at 
# index 1
NORM_COLOR_FRAG_SHADER = [
    0b000000000000011_0100111001000_0001111001000_0001111, # DP3 C0,      IN1 -> R0
    0b000000000000001_1000000000000_0010111001000_1011111, # MUL R0.xxxx, IN2 -> OUT1
    0b000000000000101_0000000000000_0000000000000_0000000, # END
]

# fragment shader program which outputs the color given by constant reg 0
FLAT_COLOR_FRAG_SHADER = [
    0b000000000000000_0000000000000_0100111001000_1011111, # MOV C0 -> OUT1
    0b000000000000101_0000000000000_0000000000000_0000000, # END
]

# size of the demo cube as deviation from origin in each direction
# i.e. each edge is 2*cube_size long
cube_size = np.float16(0.1)

# raw vertices for drawing a cube as lines
# vertices have just coords
CUBE_DEMO_VERTS_LINES = [
    # top line 1
    [np.array([cube_size,  cube_size,  cube_size,  1], np.float16)],
    [np.array([cube_size,  cube_size,  -cube_size, 1], np.float16)],
    # top line 2
    [np.array([cube_size,  cube_size,  -cube_size, 1], np.float16)],
    [np.array([-cube_size, cube_size,  -cube_size, 1], np.float16)],
    # top line 3
    [np.array([-cube_size, cube_size,  -cube_size, 1], np.float16)],
    [np.array([-cube_size, cube_size,  cube_size,  1], np.float16)],
    # top line 4
    [np.array([-cube_size, cube_size,  cube_size,  1], np.float16)],
    [np.array([cube_size,  cube_size,  cube_size,  1], np.float16)],
    # bottom line 1
    [np.array([cube_size,  -cube_size, cube_size,  1], np.float16)],
    [np.array([cube_size,  -cube_size, -cube_size, 1], np.float16)],
    # bottom line 2
    [np.array([cube_size,  -cube_size, -cube_size, 1], np.float16)],
    [np.array([-cube_size, -cube_size, -cube_size, 1], np.float16)],
    # bottom line 3
    [np.array([-cube_size, -cube_size, -cube_size, 1], np.float16)],
    [np.array([-cube_size, -cube_size, cube_size,  1], np.float16)],
    # bottom line 4
    [np.array([-cube_size, -cube_size, cube_size,  1], np.float16)],
    [np.array([cube_size,  -cube_size, cube_size,  1], np.float16)],
    # vertical line 1
    [np.array([cube_size,  cube_size,  cube_size,  1], np.float16)],
    [np.array([cube_size,  -cube_size, cube_size,  1], np.float16)],
    # vertical line 2
    [np.array([cube_size,  cube_size,  -cube_size, 1], np.float16)],
    [np.array([cube_size,  -cube_size, -cube_size, 1], np.float16)],
    # vertical line 3
    [np.array([-cube_size, cube_size,  -cube_size, 1], np.float16)],
    [np.array([-cube_size, -cube_size, -cube_size, 1], np.float16)],
    # vertical line 4
    [np.array([-cube_size, cube_size,  cube_size,  1], np.float16)],
    [np.array([-cube_size, -cube_size, cube_size,  1], np.float16)],
]

# raw vertices for drawing two intersecting triangles
# vertices have: coords, color
TRI_DEMO_VERTS = [
    # "flat" triangle with warm colors
    [np.array([-0.1, -0.05, 0.1,  1], np.float16), np.array([1.0, 0,   0,   1], np.float16)],
    [np.array([0,    0,     -0.2, 1], np.float16), np.array([1.0, 0.5, 0,   1], np.float16)],
    [np.array([0.15, -0.05, 0.1,  1], np.float16), np.array([1.0, 1.0, 0,   1], np.float16)],
    # vertical triangle with cool colors
    [np.array([-0.1, -0.15, 0,    1], np.float16), np.array([0,   1.0, 0,   1], np.float16)],
    [np.array([0,    0.15,  0,    1], np.float16), np.array([0,   0,   1.0, 1], np.float16)],
    [np.array([0.1,  -0.15, 0,    1], np.float16), np.array([0.5, 0,   1.0, 1], np.float16)],
]

def gen_sphere_demo_verts():
    radius = np.float16(0.15)
    # 8 points around equator
    equator_pts = [np.array([radius*np.cos(np.pi/4*i), 0, radius*np.sin(np.pi/4*i), 1], np.float16) for i in range(8)]
    # 8 points around upper 45deg horizontal circle
    mid_up_pts = [np.array([radius*np.cos(np.pi/4*(i+0.5))/np.sqrt(2), radius/np.sqrt(2), radius*np.sin(np.pi/4*(i+0.5))/np.sqrt(2), 1], np.float16) for i in range(8)]
    # top of sphere
    top_pt = np.array([0.0, radius, 0.0, 1], np.float16)

    # vector to flip y coord
    flip_y = np.array([1.0, -1.0, 1.0, 1.0], np.float16)

    tri_verts = []
    # generate top to mid triangles
    
    for i in range(8):
        # color with hue according to angle
        color = np.array(colorsys.hsv_to_rgb((i+1)/8, 1, 1)+(1,), np.float16)
        #tri_verts.append([top_pt, top_pt/radius, color])
        #tri_verts.append([mid_up_pts[i], mid_up_pts[i]/radius, color])
        #tri_verts.append([mid_up_pts[(i+1)%8], mid_up_pts[(i+1)%8]/radius, color])
        # generate ones on the bottom hemisphere
        tri_verts.append([top_pt*flip_y, top_pt*flip_y/radius, color])
        tri_verts.append([mid_up_pts[i]*flip_y, mid_up_pts[i]*flip_y/radius, color])
        tri_verts.append([mid_up_pts[(i+1)%8]*flip_y, mid_up_pts[(i+1)%8]*flip_y/radius, color])
    
    # generate mid to equator triangles
    for i in range(8):
        # triangle with one mid pt and 2 equator points
        # color with hue according to angle
        color = np.array(colorsys.hsv_to_rgb((i+0.5)/8, 1, 1)+(1,), np.float16)
        #tri_verts.append([mid_up_pts[i], mid_up_pts[i]/radius, color])
        #tri_verts.append([equator_pts[i], equator_pts[i]/radius, color])
        #tri_verts.append([equator_pts[(i+1)%8], equator_pts[(i+1)%8]/radius, color])
        # bottom hemi
        tri_verts.append([mid_up_pts[i]*flip_y, mid_up_pts[i]*flip_y/radius, color])
        tri_verts.append([equator_pts[i], equator_pts[i]/radius, color])
        tri_verts.append([equator_pts[(i+1)%8], equator_pts[(i+1)%8]/radius, color])
        # triangle with one equator pt and 2 mid points
        # color with hue according to angle
        color = np.array(colorsys.hsv_to_rgb(i/8, 1, 1)+(1,), np.float16)
        #tri_verts.append([equator_pts[i], equator_pts[i]/radius, color])
        #tri_verts.append([mid_up_pts[i], mid_up_pts[i]/radius, color])
        #tri_verts.append([mid_up_pts[(i-1)%8], mid_up_pts[(i-1)%8]/radius, color])
        # top hemi
        tri_verts.append([equator_pts[i], equator_pts[i]/radius, color])
        tri_verts.append([mid_up_pts[i]*flip_y, mid_up_pts[i]*flip_y/radius, color])
        tri_verts.append([mid_up_pts[(i-1)%8]*flip_y, mid_up_pts[(i-1)%8]*flip_y/radius, color])

    return tri_verts

# makes a matrix to rotate by an angle around the Y axis
def y_rot_matrix(theta):
    return np.array([[np.cos(theta), 0, -np.sin(theta), 0],
                     [0,             1, 0,              0],
                     [np.sin(theta), 0, np.cos(theta),  0],
                     [0,             0, 0,              1]], np.float16)

# makes a matrix to rotate by an angle around the X axis
def x_rot_matrix(theta):
    return np.array([[1, 0,             0,              0],
                     [0, np.cos(theta), -np.sin(theta), 0],
                     [0, np.sin(theta), np.cos(theta),  0],
                     [0, 0,             0,              1]], np.float16)

# makes a matrix for perspective transformation from a frustum into clip space
# -n is the z-coordinate of the near plane
# -f is the z-coordinate of the far plane
# r is half the x-width of the near plane
# t is half the y-height of the near plane
#
# see http://www.songho.ca/opengl/gl_projectionmatrix.html for the derivation of this
# matrix
def persp_matrix(n, f, r, t):
    return np.array([[n/r, 0,   0,            0           ],
                     [0,   n/t, 0,            0           ],
                     [0,   0,   -(f+n)/(f-n), -2*f*n/(f-n)],
                     [0,   0,   -1,           0           ]], np.float16)

# makes a matrix to translate by an amount in each dimension
# 🏳️‍⚧️
def trans_matrix(x, y, z):
    return np.array([[1, 0, 0, x],
                     [0, 1, 0, y],
                     [0, 0, 1, z],
                     [0, 0, 0, 1]], np.float16)

# creates a matrix which transforms the demo cube as follows:
# rotate by an angle theta
# translate into middle of frustum
# apply perspective transformation for frustum 0.5 away from eye, 0.5 deep, and 1.0x1.0 near plane
def demo_cube_matrix(theta):
    return np.dot(persp_matrix(0.5, 1.0, 0.5, 0.5), np.dot(trans_matrix(0, 0, -0.75), y_rot_matrix(theta)))

# --- GPU components ---

class Framebuffer:
    def __init__(self):
        # set up RGB 8-bit image
        self.img = np.zeros((128,160,3), np.uint8)

    # write to a pixel (x,y) in the framebuffer, with 6-bit color channels (which matches
    # the 18-bit color mode of the TFT)
    def writepx(self, x, y, r, g, b):
        self.img.itemset((y,x,0), r << 2)
        self.img.itemset((y,x,1), g << 2)
        self.img.itemset((y,x,2), b << 2)

    # reset the pixels
    def clear(self):
        self.img = np.zeros((128,160,3), np.uint8)

    # display using opencv
    def showimg(self):
        plt.clf()
        plt.imshow(self.img)
        plt.draw()

class ShaderProcessor:
    # opcode constants
    MOV = 0
    MUL = 1
    ADD = 2
    DP3 = 3
    DP4 = 4
    END = 5

    def __init__(self):
        # intialize all registers
        self.in_regs = [np.zeros(4, np.float16) for i in range(4)]
        #self.const_regs = None # set as static variable of subclass
        self.scratch_regs = [np.zeros(4, np.float16) for i in range(4)]
        self.out_regs = [np.zeros(4, np.float16) for i in range(4)]

    # configure the input registers
    def set_inputs(self, reg_vals, data_size):
        self.in_regs[0:data_size] = reg_vals

    # get the output register values
    def get_outputs(self):
        return self.out_regs

    # instr format (as a multi-level dict, arrays as numpy arrays)
    #   - opcode    : instruction opcode using constants above
    #   - op1
    #       - reg   : index of source reg for operand 1
    #       - sw    : swizzle which gives [reg[sw[0]], reg[sw[1]], reg[sw[2]], reg[sw[3]]]
    #       - neg   : 1 to negate operand 1, 0 otherwise
    #   - op2
    #       - reg   : index of source reg for operand 2
    #       - sw    : swizzle which gives [reg[sw[0]], reg[sw[1]], reg[sw[2]], reg[sw[3]]]
    #       - neg   : 1 to negate operand 2, 0 otherwise
    #   - dest
    #       - reg   : index of destination register
    #       - mask  : 4-element array with 1 to write to that index, 0 otherwise
    def execute_instr(self, instr, p):
        # all regs in one list for convenience
        regs = self.in_regs + self.get_const_regs() + self.scratch_regs + self.out_regs
        # writable regs in one list to match dest reg indexing
        writable_regs = self.scratch_regs + self.out_regs
        # fetch operands, swizzle and negate
        op1 = np.float16(ShaderProcessor.swizzle(regs[instr["op1"]["reg"]], instr["op1"]["sw"])*((-1)**instr["op1"]["neg"]))
        op2 = np.float16(ShaderProcessor.swizzle(regs[instr["op2"]["reg"]], instr["op2"]["sw"])*((-1)**instr["op2"]["neg"]))

        if instr["opcode"] == ShaderProcessor.MOV:
            result = op1
        elif instr["opcode"] == ShaderProcessor.MUL:
            # componentwise multiply
            result = op1*op2
        elif instr["opcode"] == ShaderProcessor.ADD:
            # componentwise add
            result = op1+op2
        elif instr["opcode"] == ShaderProcessor.DP3:
            # 3-component dot product, into every component of result
            result = np.zeros(4) + np.dot(op1[0:3], op2[0:3])
        elif instr["opcode"] == ShaderProcessor.DP4:
            # 4-component dot product, into every component of result
            result = np.zeros(4) + np.dot(op1, op2)
        #elif instr["opcode"] == ShaderProcessor.END:
            #print(f'c {self.get_const_regs()[0]}')
            #print(f'r {self.scratch_regs[0]}')
            #print(self.in_regs[1])
            #print(self.in_regs[2])

        if p:
            print(f'op1    {op1[0].view(np.uint16):04x} {op1[1].view(np.uint16):04x} {op1[2].view(np.uint16):04x} {op1[3].view(np.uint16):04x}')
            print(f'op2    {op2[0].view(np.uint16):04x} {op2[1].view(np.uint16):04x} {op2[2].view(np.uint16):04x} {op2[3].view(np.uint16):04x}')
            print(f'result {result[0].view(np.uint16):04x} {result[1].view(np.uint16):04x} {result[2].view(np.uint16):04x} {result[3].view(np.uint16):04x}')

        # END is effectively a NOP, so don't write result
        if instr["opcode"] != ShaderProcessor.END:
            # get old destination value
            dest_old = writable_regs[instr["dest"]["reg"]]
            # get masked result or old value where appropriate
            result_masked = np.float16(result*instr["dest"]["mask"] + dest_old*(instr["dest"]["mask"]^1))

            # write result to register
            if instr["dest"]["reg"] < 4:
                self.scratch_regs[instr["dest"]["reg"]] = result_masked
            elif 4 <= instr["dest"]["reg"]:
                self.out_regs[instr["dest"]["reg"]-4] = result_masked

    # swizzles quad based on indices given in sw
    @staticmethod
    def swizzle(quad, sw):
        return quad[sw]

    # converts an encoded instruction as an int to a dict with the appropriate info extracted
    # see execute_instr for dict format
    @staticmethod
    def decode_instr(instr_bits):
        # initialize the dict
        instr = {}
        instr["op1"] = {}
        instr["op2"] = {}
        instr["dest"] = {}

        # dest is in low 7 bits
        instr["dest"]["mask"] = np.array([instr_bits & 0b1,
                                          (instr_bits & (0b1 << 1)) >> 1,
                                          (instr_bits & (0b1 << 2)) >> 2,
                                          (instr_bits & (0b1 << 3)) >> 3])
        instr["dest"]["reg"] = (instr_bits & (0b111 << 4)) >> 4

        # then operands are in following 2 13-bit values
        instr["op1"]["neg"] = (instr_bits & (0b1 << 7)) >> 7
        instr["op1"]["sw"] = np.array([(instr_bits & (0b11 << 8)) >> 8,
                                       (instr_bits & (0b11 << 10)) >> 10,
                                       (instr_bits & (0b11 << 12)) >> 12,
                                       (instr_bits & (0b11 << 14)) >> 14])
        instr["op1"]["reg"] = (instr_bits & (0b1111 << 16)) >> 16
        instr["op2"]["neg"] = (instr_bits & (0b1 << 20)) >> 20
        instr["op2"]["sw"] = np.array([(instr_bits & (0b11 << 21)) >> 21,
                                       (instr_bits & (0b11 << 23)) >> 23,
                                       (instr_bits & (0b11 << 25)) >> 25,
                                       (instr_bits & (0b11 << 27)) >> 27])
        instr["op2"]["reg"] = (instr_bits & (0b1111 << 29)) >> 29

        # opcode is in the upper 15 bits
        instr["opcode"] = (instr_bits & (0b11111111111111 << 33)) >> 33

        return instr

class VertexShader(ShaderProcessor):
    # shared by all vertex shaders
    vertex_const_regs = [np.zeros(4, np.float16) for i in range(4)]
    
    def __init__(self):
        # init
        super().__init__()

    # method to update constant registers for all shaders
    @staticmethod
    def set_const_regs(regs):
        VertexShader.vertex_const_regs = regs

    # method to get constant registers used by self
    def get_const_regs(self):
        return VertexShader.vertex_const_regs

class FragmentShader(ShaderProcessor):
    # shared by all fragment shaders
    fragment_const_regs = [np.zeros(4, np.float16) for i in range(4)]
    
    def __init__(self):
        # init
        super().__init__()

    # method to update constant registers for all shaders
    @staticmethod
    def set_const_regs(regs):
        FragmentShader.fragment_const_regs = regs

    # method to get constant registers used by self
    def get_const_regs(self):
        return FragmentShader.fragment_const_regs

# reciprocal lookup table for rasterizer
# here so it doesn't get recomputed every time

# how many bits of the mantissa to use for the lookup table
INV_RESOLUTION_BITS = 6
# lookup table from (high n bits of mantissa) -> 1/mantissa
inv_lookup = np.array([1/(1 + i*2**-INV_RESOLUTION_BITS) for i in range(2**INV_RESOLUTION_BITS)], np.float16)

class Rasterizer:
    def __init__(self):
        # depth buffer gets infinity everywhere to start
        self.depth_buffer = np.zeros((256,256),np.float16) + np.float16('inf')

    # reset every element of the depth buffer to infinity for a new frame
    def reset_depth(self):
        self.depth_buffer = np.zeros((256,256),np.float16) + np.float16('inf')

    # approximate reciprocal using newton's method
    # f should be a numpy float16
    # also returns a float16
    @staticmethod
    def approx_recip_newt(f):
        # initial guess is just the reciprocal of the exponent part
        init = np.float16(2.0**-(((f.view(np.int16) & 0b0111110000000000) >> 10) - 15))
        # two Newton's method iterations
        newt = np.float16(init*(2-f*init))
        return np.float16(newt*(2-f*newt))

    # approximate reciprocal using a lookup table
    # f should be a numpy float16
    # also returns a float16
    @staticmethod
    def approx_recip_lookup(f):
        # take inverse of the exponent by negating it
        exp_inv = np.float16(2.0**-(((f.view(np.int16) & 0b0111110000000000) >> 10) - 15))
        # extract the top n bits of the mantissa
        mantissa_bits = (f.view(np.int16) & ((2**INV_RESOLUTION_BITS-1) << (10-INV_RESOLUTION_BITS))) >> (10-INV_RESOLUTION_BITS)
        # return the exponent inverse multiplied by the value from the lookup table
        # (in hardware this is just a concatenation and not a real multiplication)
        return exp_inv*inv_lookup[mantissa_bits]


    # 
    # function tells us which side of a line (v1 to v2) a point (v3) is on
    @staticmethod
    def edge_function(v1, v2, v3):
        return (v3[0]-v1[0])*(v2[1]-v1[1]) - (v3[1]-v1[1])*(v2[0]-v1[0])

    # generates all the fragments for a single line
    @staticmethod
    def emit_line_frags(v1, v2):
        frags = []

        # convert clip space to NDC by taking (x,y,z,w) -> (x/w,y/w,z/w)
        v1_ndc = v1[0][0:3]*Rasterizer.approx_recip_lookup(v1[0][3])
        v2_ndc = v2[0][0:3]*Rasterizer.approx_recip_lookup(v2[0][3])

        # convert to pixel coordinates by (-1.0,1.0) -> (-128.0,128.0), then floor
        # the mantissa to an int (same as just cutting off bits which makes this 
        # easy in hardware)
        # note: pixel coordinates are 256x256 for convenience but only middle 
        # 128x160 are used
        v1_px = np.int32(np.fix(v1_ndc[0:2]*np.float16(128)))
        v2_px = np.int32(np.fix(v2_ndc[0:2]*np.float16(128)))

        # bresenham's line alg
        dx = np.abs(v2_px[0] - v1_px[0])
        sx = np.sign(v2_px[0] - v1_px[0])
        dy = -np.abs(v2_px[1] - v1_px[1])
        sy = np.sign(v2_px[1] - v1_px[1])

        err = dx + dy

        # emit fragment at first point
        frags = frags + [{"pixel_coords":np.copy(v1_px),"attrs":v1}]
        # emit until we reach the end of the line
        while not (v1_px[0] == v2_px[0] and v1_px[1] == v2_px[1]):
            # step and update error
            err2 = err << 1
            if err2 >= dy:
                err += dy
                v1_px[0] += sx
            if err2 <= dx:
                err += dx
                v1_px[1] += sy
            # emit fragment at new point
            frags = frags + [{"pixel_coords":np.copy(v1_px),"attrs":v1}]

        # return emitted fragments
        return frags

    # generates all the fragments for a single triangle
    def emit_triangle_frags(self, v1, v2, v3):
        frags = []

        # -- FSM state 1: load v1, v2, v3 from RAM --

        # start variables for NDC (normalized device coords) at the vertices
        v1_ndc = np.copy(v1[0])
        v2_ndc = np.copy(v2[0])
        v3_ndc = np.copy(v3[0])

        # -- FSM state 2 --

        # get reciprocals of w values
        v1_ndc[3] = Rasterizer.approx_recip_lookup(v1_ndc[3])
        v2_ndc[3] = Rasterizer.approx_recip_lookup(v2_ndc[3])
        v3_ndc[3] = Rasterizer.approx_recip_lookup(v3_ndc[3])

        # -- FSM state 3,4,5 --

        # convert clip space to NDC by taking (x,y,z,1/w) -> (x/w,y/w,z/w,1/w)
        v1_ndc = np.concatenate((v1_ndc[0:3]*v1_ndc[3], [v1_ndc[3]]))
        v2_ndc = np.concatenate((v2_ndc[0:3]*v2_ndc[3], [v2_ndc[3]]))
        v3_ndc = np.concatenate((v3_ndc[0:3]*v3_ndc[3], [v3_ndc[3]]))

        # -- FSM state 6 --

        # convert to pixel coordinates
        v1_px = np.int32(np.fix(v1_ndc[0:2]*np.float16(128)))
        v2_px = np.int32(np.fix(v2_ndc[0:2]*np.float16(128)))
        v3_px = np.int32(np.fix(v3_ndc[0:2]*np.float16(128)))

        #print(f'v1 {v1_ndc[0].view(np.uint16):04x} {v1_ndc[1].view(np.uint16):04x} {v1_ndc[2].view(np.uint16):04x} {v1_ndc[3].view(np.uint16):04x}')
        #print(f'v1 px {v1_px}')
        #print(f'v2 {v2_ndc[0].view(np.uint16):04x} {v2_ndc[1].view(np.uint16):04x} {v2_ndc[2].view(np.uint16):04x} {v2_ndc[3].view(np.uint16):04x}')
        #print(f'v2 px {v2_px}')
        #print(f'v3 {v3_ndc[0].view(np.uint16):04x} {v3_ndc[1].view(np.uint16):04x} {v3_ndc[2].view(np.uint16):04x} {v3_ndc[3].view(np.uint16):04x}')
        #print(f'v3 px {v3_px}')

        # -- FSM state 7,8,9,10 --

        # get the bounds of the rectangle around the triangle
        min_x = min(v1_px[0], v2_px[0], v3_px[0])
        max_x = max(v1_px[0], v2_px[0], v3_px[0])
        min_y = min(v1_px[1], v2_px[1], v3_px[1])
        max_y = max(v1_px[1], v2_px[1], v3_px[1])

        # -- FSM state 11,12,13 (each count over 3 clocks) --

        # scale attributes by w inverse in preparation for interpolation
        v1[1:4] = [a*v1_ndc[3] for a in v1[1:4]]
        v2[1:4] = [a*v2_ndc[3] for a in v2[1:4]]
        v3[1:4] = [a*v3_ndc[3] for a in v3[1:4]]

        #print(f'v1a1 {v1[1][0].view(np.uint16):04x} {v1[1][1].view(np.uint16):04x} {v1[1][2].view(np.uint16):04x} {v1[1][3].view(np.uint16):04x}')
        #print(f'v2a1 {v2[1][0].view(np.uint16):04x} {v2[1][1].view(np.uint16):04x} {v2[1][2].view(np.uint16):04x} {v2[1][3].view(np.uint16):04x}')
        #print(f'v3a1 {v3[1][0].view(np.uint16):04x} {v3[1][1].view(np.uint16):04x} {v3[1][2].view(np.uint16):04x} {v3[1][3].view(np.uint16):04x}')

        # -- FSM state 14,15 --

        # get reciprocal of triangle area, to be used later
        area = Rasterizer.edge_function(v1_ndc, v2_ndc, v3_ndc)
        area_recip = Rasterizer.approx_recip_lookup(area)

        # -- FSM state 16 (2 counters to loop over every pixel val) --

        # loop through each pixel in that rectangle
        for px_x in range(min_x,max_x+1):
            for px_y in range(min_y,max_y+1):
                # -- pipeline stage 1 --

                # convert pixel coords back to float
                px_f = np.float16(np.array([px_x, px_y])/128.0)

                # -- pipeline stage 2 --

                # edge function for each triangle edge
                b0 = Rasterizer.edge_function(v2_ndc, v3_ndc, px_f)
                b1 = Rasterizer.edge_function(v3_ndc, v1_ndc, px_f)
                b2 = Rasterizer.edge_function(v1_ndc, v2_ndc, px_f)

                # -- pipeline stage 3 --

                # if point inside triangle on either face
                if (b0 >= 0 and b1 >= 0 and b2 >= 0) or (b0 <= 0 and b1 <= 0 and b2 <= 0):
                    # -- pipeline stage 4 --

                    # normalize by area to get coords inside triangle
                    b0 = np.abs(b0*area_recip)
                    b1 = np.abs(b1*area_recip)
                    b2 = np.abs(b2*area_recip)

                    # -- pipeline stage 5 --

                    # linearly interpolate z
                    z = b0*v1_ndc[2]+b1*v2_ndc[2]+b2*v3_ndc[2]

                    # -- pipeline stage 6 --

                    # interpolate 1/w
                    w_inv = Rasterizer.approx_recip_lookup(b0*v1_ndc[3]+b1*v2_ndc[3]+b2*v3_ndc[3])

                    # -- pipeline stage 7,8,9,10,11,12 (x2/attr) --

                    # perform perspective-correct attribute interpolation by taking
                    # A = (b0*A1*v1_ndc[3] + b1*A2*v2_ndc[3] + b2*A3*v3_ndc[3])*w_inv
                    # (multiply by 1/w_n for each has already been performed)
                    attr_out = [b0*v1[i+1] + b1*v2[i+1] + b2*v3[i+1] for i in range(3)]
                    attr_out = [a*w_inv for a in attr_out]

                    # -- pipeline stage 13 --

                    # perform depth test
                    if z < self.depth_buffer[px_y,px_x]:
                        # -- pipeline stage 14 (final, emit fragment to FIFO) --

                        # in front of old fragment, update depth buffer and output fragment
                        self.depth_buffer[px_y,px_x] = z
                        # 0th attribute out is pixel coords for now (technically should be 4D for consistency)
                        frags = frags + [{"pixel_coords":np.array([px_x,px_y]),"attrs":[px_f] + attr_out}]

        return frags


class GPU:
    def __init__(self):
        # GPU components
        self.vert_shaders = [VertexShader() for i in range(4)]
        self.frag_shaders = [FragmentShader() for i in range(16)]
        self.rast = Rasterizer()
        # RAM sections
        self.fbuff = Framebuffer()
        self.raw_vertices = []
        self.shaded_vertices = []
        self.raw_fragments = []
        self.shaded_fragments = []
        # program memory
        self.vert_prog = []
        self.frag_prog = []

    # load vertices into RAM
    def load_vertices(self, vertices):
        self.raw_vertices = vertices

    # load shader programs into program memory

    def load_vertex_shader(self, prog):
        self.vert_prog = prog

    def load_fragment_shader(self, prog):
        self.frag_prog = prog

    # runs the loaded shader program in "parallel" to shade the loaded raw vertices
    def shade_vertices(self):
        # reset shaded vertices
        self.shaded_vertices = [0 for i in range(len(self.raw_vertices))]

        # vertex index
        i = 0
        while i < len(self.raw_vertices):
            # prepare shader inputs for the next 4 vertices, or rest if < 4 are left
            for j in range(min(4, len(self.raw_vertices)-i)):
                self.vert_shaders[j].set_inputs(self.raw_vertices[i+j], len(self.raw_vertices[i+j]))

            # run vertex program as SIMD
            for instr in self.vert_prog:
                for sh in self.vert_shaders:
                    sh.execute_instr(ShaderProcessor.decode_instr(instr), False)# self.vert_shaders.index(sh) == 1 and i >= 4)

            # extract outputs from each vertex shader
            for j in range(min(4, len(self.raw_vertices)-i)):
                self.shaded_vertices[i+j] = np.copy(self.vert_shaders[j].get_outputs())

            # increment to next set of vertices
            i += 4

    # runs the loaded shader program in "parallel" to shade the loaded raw fragments
    def shade_fragments(self):
        # reset shaded fragments
        self.shaded_fragments = [0 for i in range(len(self.raw_fragments))]

        # fragment index
        i = 0
        while i < len(self.raw_fragments):
            # prepare shader inputs for the next 16 fragments, or rest if < 16 are left
            for j in range(min(16, len(self.raw_fragments)-i)):
                self.frag_shaders[j].set_inputs(self.raw_fragments[i+j]["attrs"], len(self.raw_fragments[i+j]["attrs"]))

            # run vertex program as SIMD
            for instr in self.frag_prog:
                for sh in self.frag_shaders:
                    sh.execute_instr(ShaderProcessor.decode_instr(instr), False)

            # extract outputs from each vertex shader
            for j in range(min(4, len(self.raw_fragments)-i)):
                self.shaded_fragments[i+j] = {
                    "pixel_coords": self.raw_fragments[i+j]["pixel_coords"],
                    "attrs": np.copy(self.frag_shaders[j].get_outputs())
                }

            # increment to next set of vertices
            i += 4

    # rasterizes shaded vertices into lines
    def rasterize_lines(self):
        # reset fragments
        self.raw_fragments = []

        # vertex index
        i = 0
        # rasterize each two vertices as a line
        while i < len(self.shaded_vertices):
            self.raw_fragments += Rasterizer.emit_line_frags(self.shaded_vertices[i], self.shaded_vertices[i+1])
            i += 2

    # rasterizes shaded vertices into triangles
    def rasterize_triangles(self):
        # reset fragments
        self.raw_fragments = []
        # reset depth buffer
        self.rast.reset_depth()

        # vertex index
        i = 0
        # rasterize each three vertices as a triangle
        while i < len(self.shaded_vertices):
            self.raw_fragments += self.rast.emit_triangle_frags(self.shaded_vertices[i], self.shaded_vertices[i+1], self.shaded_vertices[i+2])
            i += 3

    # outputs shaded fragments to framebuffer
    def output_frame(self):
        # reset framebuffer
        self.fbuff.clear()

        for frag in self.shaded_fragments:
            x = frag["pixel_coords"][0] + 80
            y = frag["pixel_coords"][1] + 64
            if 0 <= x < 160 and 0 <= y < 128:
                # extract color and scale 0.0-1.0 to 0-64 (6-bit range)
                color = np.int32(np.fix(frag["attrs"][1][0:3]*np.float16(64)))
                # clamp color to 6-bit range
                color_clamped = np.maximum(np.minimum(color, 63), 0)
                self.fbuff.writepx(x,y,*color_clamped)

        self.fbuff.showimg()

# --- code to run tests ---

if __name__ == "__main__":

    #for i in CUBE_DEMO_VERT_SHADER:
    #    print(ShaderProcessor.decode_instr(i))

    # "CUBE" or "TRI" or "NORM"
    demo_mode = "NORM"

    # instantiate GPU
    gpu = GPU()

    if demo_mode == "CUBE":
        # load vertices to render the cube demo as lines
        gpu.load_vertices(CUBE_DEMO_VERTS_LINES)

        # load vertex shader for cube demo
        gpu.load_vertex_shader(CUBE_DEMO_VERT_SHADER)

        # load flat color output shader
        gpu.load_fragment_shader(FLAT_COLOR_FRAG_SHADER)

        # set fragment constant reg to red (this is used as color)
        FragmentShader.set_const_regs([np.array([1.0, 0, 0, 0], np.float16) for i in range(4)])

    elif demo_mode == "NORM":
        for a in gen_sphere_demo_verts():
            print(f'{a[0][0]:<8.4f}, {a[0][1]:<8.4f}, {a[0][2]:<8.4f}, {a[0][3]:<8.4f}, {a[1][0]:<8.4f}, {a[1][1]:<8.4f}, {a[1][2]:<8.4f}, {a[1][3]:<8.4f}, {a[2][0]:<8.4f}, {a[2][1]:<8.4f}, {a[2][2]:<8.4f}, {a[2][3]:<8.4f},')

        # load sphere vertices
        gpu.load_vertices(gen_sphere_demo_verts())

        # load vertex shader for normal demo
        gpu.load_vertex_shader(NORM_DEMO_VERT_SHADER)

        # load normal-sclaed color output shader
        gpu.load_fragment_shader(NORM_COLOR_FRAG_SHADER)

        # load light vector into constant 0
        FragmentShader.set_const_regs([np.array([1.0/np.sqrt(2), 1.0/np.sqrt(2), 1.0/np.sqrt(2), 0.0], np.float16) for i in range(4)])


    elif demo_mode == "TRI":
        # load vertices to render the two triangles
        gpu.load_vertices(TRI_DEMO_VERTS)

        # load vertex shader for triangle demo
        gpu.load_vertex_shader(TRI_DEMO_VERT_SHADER)

        # load vertex color output shader
        gpu.load_fragment_shader(VERT_COLOR_FRAG_SHADER)

    theta = np.float16(0.2)#-np.pi/2)
    while True: #for i in range(100):
        # get matrix for cube demo vertex transformation
        cube_xform = demo_cube_matrix(theta)
        # load columns of this matrix into vertex shader constant regs
        VertexShader.set_const_regs([cube_xform[:,i] for i in range(4)])

        #print(cube_xform)

        # run the vertex shader
        gpu.shade_vertices()

        #for f in gpu.shaded_vertices[0][0]:
        #    print(f'{f.view(np.uint16):04x}')

        if demo_mode == "CUBE":
            # rasterize as lines
            gpu.rasterize_lines()
        elif demo_mode == "TRI" or demo_mode == "NORM":
            # rasterize triangles
            gpu.rasterize_triangles()

        # shade rasterize fragments
        gpu.shade_fragments()

        # output rendered frame
        gpu.output_frame()

        theta += 0.1
        plt.pause(0.05)

        # stop if triangle mode for debugging
        #while demo_mode == "NORM":
        #    None

# makes a graph of different reciprocal options and computes error of the LUT reciprocal
def test_inv_lookup():
    # get every possible float16 mantissa
    x = np.array([1 + i*2**-10 for i in range(2**10)], np.float16)
    y_real = np.float16(1.0)/x
    y_lookup = np.array([Rasterizer.approx_recip_lookup(xi) for xi in x])
    y_newt = np.array([Rasterizer.approx_recip_newt(xi) for xi in x])

    print(f'Max lookup table error: {np.max((y_lookup - y_real)/y_real)*100}%')

    fig, ax = plt.subplots()
    ax.plot(x, y_real, label='full precision')
    ax.plot(x, y_lookup, label='lookup table')
    ax.plot(x, y_newt, label='newtons method')
    ax.legend()

    plt.show()

# old test from before GPU class existed
def old_test():
    # GPU components
    fbuff = Framebuffer()
    rast = Rasterizer()

    # line test
    v1 = [np.array([np.float16(-0.2),np.float16(-0.3),np.float16(0),np.float16(1)])]
    v2 = [np.array([np.float16(0.1),np.float16(0.4),np.float16(0),np.float16(1)])]
    #frags = Rasterizer.emit_line_frags(v1, v2)

    # triangle test
    v1 = [np.array([np.float16(-0.2),np.float16(-0.2),np.float16(0.2),np.float16(1)])]
    v2 = [np.array([np.float16(0.1),np.float16(0.4),np.float16(0.4),np.float16(1)])]
    v3 = [np.array([np.float16(-0.2),np.float16(0.3),np.float16(0.6),np.float16(1)])]
    frags = rast.emit_triangle_frags(v3, v2, v1)

    # output every fragment into the framebuffer for now
    for frag in frags:
        x = frag["pixel_coords"][0] + 80
        y = frag["pixel_coords"][1] + 64
        if 0 <= x < 160 and 0 <= y < 128:
            fbuff.writepx(x,y,0b111111,0,0)

    fbuff.showimg()



