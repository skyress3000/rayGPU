/******************************************************************************
*
*   STM32 demo GPU control code
*
*   This file contains an Arduino sketch to be run on a STM32 Nucleo board to
*   control the GPU for running a triangle demo and a half-sphere demo. The 
*   triangle demo consists of two triangles, one mostly horizontal with warm
*   colors, and one vertical with cool colors, which are intersecting and 
*   rotate as well as slowly floating up and down. Color is interpolated between
*   the vertices of each triangle. The half-sphere demo can be run by replacing
*   the call to init_tri_demo with init_sphere demo, and consists of a low-poly
*   rotating rainbow hemisphere. The sphere demo includes interpolation of 
*   normal vector attributes and shadows based on the normal vector.
*
*   Revision History:
*      11 Jun 21  Ray Wendt         Initial revision.
*      13 Jun 21  Ray Wendt         Added "sphere" demo.
*      16 Jun 21  Ray Wendt         Added header comment.
*
******************************************************************************/

// create float16 type since Arduino doesn't natively support it
typedef uint16_t float16;

// microsecond period for write clock
const int CLK_PERIOD_US = 20;
// pin for GPU data bus clock
const int WR_CLK_PIN = PA0;

// millisecond delay between frames
// 100 ms is generous but should ensure no issues
const int FRAMERATE_MS = 100;

// constants/macros for GPU register addresses, with indexing as appropriate
#define V_DATA_ADDR(addr)       0b0010000000000000 | ((addr) & 0b11111111111)
#define V_PROG_ADDR(addr, i)    0b0010100000000000 | (((addr) & 0b11111111) << 2) | ((i) & 0b11)
#define V_CONST_ADDR(i, j)      0b0011000000000000 | (((i) & 0b11) << 2) | ((j) & 0b11)
const uint16_t V_DATA_SIZE_ADDR = 0b0011100000000000;
const uint16_t V_DATA_BASE_ADDR = 0b0011100000000001;
const uint16_t V_COUNT_ADDR     = 0b0011100000000010;
#define F_PROG_ADDR(addr, i)    0b0100100000000000 | (((addr) & 0b11111111) << 2) | ((i) & 0b11)
#define F_CONST_ADDR(i, j)      0b0101000000000000 | (((i) & 0b11) << 2) | ((j) & 0b11)
const uint16_t CTRL_ADDR        = 0b1000000000000000;

// constants for control register bits
// bit 0 starts render
const uint16_t CTRL_RENDER      = 0b0000000000000001;
// bit 1 swaps framebuffers and outputs to display
const uint16_t CTRL_SWAP        = 0b0000000000000010;

// shader for triangle demo
// multiplies the input coords in IN0 by a matrix with columns stored in constant regs,
// and forwards the color attribute from IN1 to OUT1
const int TRI_DEMO_VPROG_SIZE = 9;
const uint16_t TRI_DEMO_VERT_SHADER[TRI_DEMO_VPROG_SIZE*3] = {
  0b0000000000000010, 0b1001110010000000, 0b0000000000001111, // MUL C0, IN0.xxxx -> R0
  0b0000000000000010, 0b1011110010000000, 0b0101010100011111, // MUL C1, IN0.yyyy -> R1
  0b0000000000000101, 0b0011110010001000, 0b1110010000001111, // ADD R1, R0       -> R0
  0b0000000000000010, 0b1101110010000000, 0b1010101000011111, // MUL C2, IN0.zzzz -> R1
  0b0000000000000101, 0b0011110010001000, 0b1110010000001111, // ADD R1, R0       -> R0
  0b0000000000000010, 0b1111110010000000, 0b1111111100011111, // MUL C3, IN0.wwww -> R1
  0b0000000000000101, 0b0011110010001000, 0b1110010001001111, // ADD R1, R0       -> OUT0
  0b0000000000000000, 0b0000000000000001, 0b1110010001011111, // MOV IN1          -> OUT1
  0b0000000000001010, 0b0000000000000000, 0b0000000000000000, // END
};

// same as triangle demo vertex shader, but forwards the color and normal attributes to the shaded vertex
// note that because we don't multiply the normal vector by the transformation matrix, we get the effect
// of moving the camera rather than the object
const int SPHERE_DEMO_VPROG_SIZE = 10;
const uint16_t SPHERE_DEMO_VERT_SHADER[SPHERE_DEMO_VPROG_SIZE*3] = {
  0b0000000000000010, 0b1001110010000000, 0b0000000000001111, // MUL C0, IN0.xxxx -> R0
  0b0000000000000010, 0b1011110010000000, 0b0101010100011111, // MUL C1, IN0.yyyy -> R1
  0b0000000000000101, 0b0011110010001000, 0b1110010000001111, // ADD R1, R0       -> R0
  0b0000000000000010, 0b1101110010000000, 0b1010101000011111, // MUL C2, IN0.zzzz -> R1
  0b0000000000000101, 0b0011110010001000, 0b1110010000001111, // ADD R1, R0       -> R0
  0b0000000000000010, 0b1111110010000000, 0b1111111100011111, // MUL C3, IN0.wwww -> R1
  0b0000000000000101, 0b0011110010001000, 0b1110010001001111, // ADD R1, R0       -> OUT0
  0b0000000000000000, 0b0000000000000001, 0b1110010001011111, // MOV IN1          -> OUT1
  0b0000000000000000, 0b0000000000000010, 0b1110010001101111, // MOV IN2          -> OUT2
  0b0000000000001010, 0b0000000000000000, 0b0000000000000000, // END
};

// simple fragment shader which outputs the color given by IN1 (attribute 1)
const int TRI_DEMO_FPROG_SIZE = 2;
const uint16_t VERT_COLOR_FRAG_SHADER[TRI_DEMO_FPROG_SIZE*3] = {
  0b0000000000000000, 0b0000000000000001, 0b1110010001011111, // MOV IN1 -> OUT1
  0b0000000000001010, 0b0000000000000000, 0b0000000000000000, // END
};

// fragment shader to perform normal-based shading
// takes dot product of light vector in constant reg and normal vector and scales color accordingly
const int SPHERE_DEMO_FPROG_SIZE = 3;
const uint16_t SPHERE_DEMO_FRAG_SHADER[SPHERE_DEMO_FPROG_SIZE*3] = {
  0b0000000000000110, 0b1001110010000001, 0b1110010000001111, // DP3 C0,      IN1 -> R0
  0b0000000000000011, 0b0000000000000010, 0b1110010001011111, // MUL R0.xxxx, IN2 -> OUT1
  0b0000000000001010, 0b0000000000000000, 0b0000000000000000, // END
};

// number of vertices in triangle demo
const int TRI_DEMO_V_COUNT = 6;
// triangle demo vertex data
const float TRI_DEMO_VERTS[TRI_DEMO_V_COUNT*8] = {
  // "flat" triangle with warm colors
  // coords               color attr
  -0.1, -0.05, 0.1, 1,    1.0, 0.0, 0.0, 1.0,
  0.0, 0.0, -0.2, 1,      1.0, 0.5, 0.0, 1.0,
  0.15, -0.05, 0.1, 1,    1.0, 1.0, 0.0, 1.0,
  // vertical triangle with cool colors
  -0.1, -0.15, 0.0, 1.0,  0.0, 1.0, 0.0, 1.0,
  0.0, 0.15, 0.0, 1.0,    0.0, 0.0, 1.0, 1.0,
  0.1, -0.15, 0.0, 1.0,   0.5, 0.0, 1.0, 1.0,
};

// number of vertices in sphere demo
const int SPHERE_DEMO_V_COUNT = 72;
// triangle vertex data for low-poly rainbow sphere with normal vectors
const float SPHERE_DEMO_VERTS[SPHERE_DEMO_V_COUNT*12] = {
  // coords                               normal vector                           color
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 1.0000  , 0.7500  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , 0.0406  , 1.0000  , 0.6533  , -0.7070 , 0.2705  , 6.6641  , 1.0000  , 0.7500  , 0.0000  , 1.0000  ,
  0.0406  , -0.1061 , 0.0980  , 1.0000  , 0.2705  , -0.7070 , 0.6533  , 6.6641  , 1.0000  , 0.7500  , 0.0000  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 0.5000  , 1.0000  , 0.0000  , 1.0000  ,
  0.0406  , -0.1061 , 0.0980  , 1.0000  , 0.2705  , -0.7070 , 0.6533  , 6.6641  , 0.5000  , 1.0000  , 0.0000  , 1.0000  ,
  -0.0406 , -0.1061 , 0.0980  , 1.0000  , -0.2705 , -0.7070 , 0.6533  , 6.6641  , 0.5000  , 1.0000  , 0.0000  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 0.0000  , 1.0000  , 0.2500  , 1.0000  ,
  -0.0406 , -0.1061 , 0.0980  , 1.0000  , -0.2705 , -0.7070 , 0.6533  , 6.6641  , 0.0000  , 1.0000  , 0.2500  , 1.0000  ,
  -0.0980 , -0.1061 , 0.0406  , 1.0000  , -0.6533 , -0.7070 , 0.2705  , 6.6641  , 0.0000  , 1.0000  , 0.2500  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 0.0000  , 1.0000  , 1.0000  , 1.0000  ,
  -0.0980 , -0.1061 , 0.0406  , 1.0000  , -0.6533 , -0.7070 , 0.2705  , 6.6641  , 0.0000  , 1.0000  , 1.0000  , 1.0000  ,
  -0.0980 , -0.1061 , -0.0406 , 1.0000  , -0.6533 , -0.7070 , -0.2705 , 6.6641  , 0.0000  , 1.0000  , 1.0000  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 0.0000  , 0.2500  , 1.0000  , 1.0000  ,
  -0.0980 , -0.1061 , -0.0406 , 1.0000  , -0.6533 , -0.7070 , -0.2705 , 6.6641  , 0.0000  , 0.2500  , 1.0000  , 1.0000  ,
  -0.0406 , -0.1061 , -0.0980 , 1.0000  , -0.2705 , -0.7070 , -0.6533 , 6.6641  , 0.0000  , 0.2500  , 1.0000  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 0.5000  , 0.0000  , 1.0000  , 1.0000  ,
  -0.0406 , -0.1061 , -0.0980 , 1.0000  , -0.2705 , -0.7070 , -0.6533 , 6.6641  , 0.5000  , 0.0000  , 1.0000  , 1.0000  ,
  0.0406  , -0.1061 , -0.0980 , 1.0000  , 0.2705  , -0.7070 , -0.6533 , 6.6641  , 0.5000  , 0.0000  , 1.0000  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 1.0000  , 0.0000  , 0.7500  , 1.0000  ,
  0.0406  , -0.1061 , -0.0980 , 1.0000  , 0.2705  , -0.7070 , -0.6533 , 6.6641  , 1.0000  , 0.0000  , 0.7500  , 1.0000  ,
  0.0980  , -0.1061 , -0.0406 , 1.0000  , 0.6533  , -0.7070 , -0.2705 , 6.6641  , 1.0000  , 0.0000  , 0.7500  , 1.0000  ,
  0.0000  , -0.1500 , 0.0000  , 1.0000  , 0.0000  , -1.0000 , 0.0000  , 6.6641  , 1.0000  , 0.0000  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , -0.0406 , 1.0000  , 0.6533  , -0.7070 , -0.2705 , 6.6641  , 1.0000  , 0.0000  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , 0.0406  , 1.0000  , 0.6533  , -0.7070 , 0.2705  , 6.6641  , 1.0000  , 0.0000  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , 0.0406  , 1.0000  , 0.6533  , -0.7070 , 0.2705  , 6.6641  , 1.0000  , 0.3750  , 0.0000  , 1.0000  ,
  0.1500  , 0.0000  , 0.0000  , 1.0000  , 1.0000  , 0.0000  , 0.0000  , 6.6641  , 1.0000  , 0.3750  , 0.0000  , 1.0000  ,
  0.1061  , 0.0000  , 0.1061  , 1.0000  , 0.7070  , 0.0000  , 0.7070  , 6.6641  , 1.0000  , 0.3750  , 0.0000  , 1.0000  ,
  0.1500  , 0.0000  , 0.0000  , 1.0000  , 1.0000  , 0.0000  , 0.0000  , 6.6641  , 1.0000  , 0.0000  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , 0.0406  , 1.0000  , 0.6533  , -0.7070 , 0.2705  , 6.6641  , 1.0000  , 0.0000  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , -0.0406 , 1.0000  , 0.6533  , -0.7070 , -0.2705 , 6.6641  , 1.0000  , 0.0000  , 0.0000  , 1.0000  ,
  0.0406  , -0.1061 , 0.0980  , 1.0000  , 0.2705  , -0.7070 , 0.6533  , 6.6641  , 0.8750  , 1.0000  , 0.0000  , 1.0000  ,
  0.1061  , 0.0000  , 0.1061  , 1.0000  , 0.7070  , 0.0000  , 0.7070  , 6.6641  , 0.8750  , 1.0000  , 0.0000  , 1.0000  ,
  0.0000  , 0.0000  , 0.1500  , 1.0000  , 0.0000  , 0.0000  , 1.0000  , 6.6641  , 0.8750  , 1.0000  , 0.0000  , 1.0000  ,
  0.1061  , 0.0000  , 0.1061  , 1.0000  , 0.7070  , 0.0000  , 0.7070  , 6.6641  , 1.0000  , 0.7500  , 0.0000  , 1.0000  ,
  0.0406  , -0.1061 , 0.0980  , 1.0000  , 0.2705  , -0.7070 , 0.6533  , 6.6641  , 1.0000  , 0.7500  , 0.0000  , 1.0000  ,
  0.0980  , -0.1061 , 0.0406  , 1.0000  , 0.6533  , -0.7070 , 0.2705  , 6.6641  , 1.0000  , 0.7500  , 0.0000  , 1.0000  ,
  -0.0406 , -0.1061 , 0.0980  , 1.0000  , -0.2705 , -0.7070 , 0.6533  , 6.6641  , 0.1250  , 1.0000  , 0.0000  , 1.0000  ,
  0.0000  , 0.0000  , 0.1500  , 1.0000  , 0.0000  , 0.0000  , 1.0000  , 6.6641  , 0.1250  , 1.0000  , 0.0000  , 1.0000  ,
  -0.1061 , 0.0000  , 0.1061  , 1.0000  , -0.7070 , 0.0000  , 0.7070  , 6.6641  , 0.1250  , 1.0000  , 0.0000  , 1.0000  ,
  0.0000  , 0.0000  , 0.1500  , 1.0000  , 0.0000  , 0.0000  , 1.0000  , 6.6641  , 0.5000  , 1.0000  , 0.0000  , 1.0000  ,
  -0.0406 , -0.1061 , 0.0980  , 1.0000  , -0.2705 , -0.7070 , 0.6533  , 6.6641  , 0.5000  , 1.0000  , 0.0000  , 1.0000  ,
  0.0406  , -0.1061 , 0.0980  , 1.0000  , 0.2705  , -0.7070 , 0.6533  , 6.6641  , 0.5000  , 1.0000  , 0.0000  , 1.0000  ,
  -0.0980 , -0.1061 , 0.0406  , 1.0000  , -0.6533 , -0.7070 , 0.2705  , 6.6641  , 0.0000  , 1.0000  , 0.6250  , 1.0000  ,
  -0.1061 , 0.0000  , 0.1061  , 1.0000  , -0.7070 , 0.0000  , 0.7070  , 6.6641  , 0.0000  , 1.0000  , 0.6250  , 1.0000  ,
  -0.1500 , 0.0000  , 0.0000  , 1.0000  , -1.0000 , 0.0000  , 0.0000  , 6.6641  , 0.0000  , 1.0000  , 0.6250  , 1.0000  ,
  -0.1061 , 0.0000  , 0.1061  , 1.0000  , -0.7070 , 0.0000  , 0.7070  , 6.6641  , 0.0000  , 1.0000  , 0.2500  , 1.0000  ,
  -0.0980 , -0.1061 , 0.0406  , 1.0000  , -0.6533 , -0.7070 , 0.2705  , 6.6641  , 0.0000  , 1.0000  , 0.2500  , 1.0000  ,
  -0.0406 , -0.1061 , 0.0980  , 1.0000  , -0.2705 , -0.7070 , 0.6533  , 6.6641  , 0.0000  , 1.0000  , 0.2500  , 1.0000  ,
  -0.0980 , -0.1061 , -0.0406 , 1.0000  , -0.6533 , -0.7070 , -0.2705 , 6.6641  , 0.0000  , 0.6250  , 1.0000  , 1.0000  ,
  -0.1500 , 0.0000  , 0.0000  , 1.0000  , -1.0000 , 0.0000  , 0.0000  , 6.6641  , 0.0000  , 0.6250  , 1.0000  , 1.0000  ,
  -0.1061 , 0.0000  , -0.1061 , 1.0000  , -0.7070 , 0.0000  , -0.7070 , 6.6641  , 0.0000  , 0.6250  , 1.0000  , 1.0000  ,
  -0.1500 , 0.0000  , 0.0000  , 1.0000  , -1.0000 , 0.0000  , 0.0000  , 6.6641  , 0.0000  , 1.0000  , 1.0000  , 1.0000  ,
  -0.0980 , -0.1061 , -0.0406 , 1.0000  , -0.6533 , -0.7070 , -0.2705 , 6.6641  , 0.0000  , 1.0000  , 1.0000  , 1.0000  ,
  -0.0980 , -0.1061 , 0.0406  , 1.0000  , -0.6533 , -0.7070 , 0.2705  , 6.6641  , 0.0000  , 1.0000  , 1.0000  , 1.0000  ,
  -0.0406 , -0.1061 , -0.0980 , 1.0000  , -0.2705 , -0.7070 , -0.6533 , 6.6641  , 0.1250  , 0.0000  , 1.0000  , 1.0000  ,
  -0.1061 , 0.0000  , -0.1061 , 1.0000  , -0.7070 , 0.0000  , -0.7070 , 6.6641  , 0.1250  , 0.0000  , 1.0000  , 1.0000  ,
  -0.0000 , 0.0000  , -0.1500 , 1.0000  , -0.0000 , 0.0000  , -1.0000 , 6.6641  , 0.1250  , 0.0000  , 1.0000  , 1.0000  ,
  -0.1061 , 0.0000  , -0.1061 , 1.0000  , -0.7070 , 0.0000  , -0.7070 , 6.6641  , 0.0000  , 0.2500  , 1.0000  , 1.0000  ,
  -0.0406 , -0.1061 , -0.0980 , 1.0000  , -0.2705 , -0.7070 , -0.6533 , 6.6641  , 0.0000  , 0.2500  , 1.0000  , 1.0000  ,
  -0.0980 , -0.1061 , -0.0406 , 1.0000  , -0.6533 , -0.7070 , -0.2705 , 6.6641  , 0.0000  , 0.2500  , 1.0000  , 1.0000  ,
  0.0406  , -0.1061 , -0.0980 , 1.0000  , 0.2705  , -0.7070 , -0.6533 , 6.6641  , 0.8750  , 0.0000  , 1.0000  , 1.0000  ,
  -0.0000 , 0.0000  , -0.1500 , 1.0000  , -0.0000 , 0.0000  , -1.0000 , 6.6641  , 0.8750  , 0.0000  , 1.0000  , 1.0000  ,
  0.1061  , 0.0000  , -0.1061 , 1.0000  , 0.7070  , 0.0000  , -0.7070 , 6.6641  , 0.8750  , 0.0000  , 1.0000  , 1.0000  ,
  -0.0000 , 0.0000  , -0.1500 , 1.0000  , -0.0000 , 0.0000  , -1.0000 , 6.6641  , 0.5000  , 0.0000  , 1.0000  , 1.0000  ,
  0.0406  , -0.1061 , -0.0980 , 1.0000  , 0.2705  , -0.7070 , -0.6533 , 6.6641  , 0.5000  , 0.0000  , 1.0000  , 1.0000  ,
  -0.0406 , -0.1061 , -0.0980 , 1.0000  , -0.2705 , -0.7070 , -0.6533 , 6.6641  , 0.5000  , 0.0000  , 1.0000  , 1.0000  ,
  0.0980  , -0.1061 , -0.0406 , 1.0000  , 0.6533  , -0.7070 , -0.2705 , 6.6641  , 1.0000  , 0.0000  , 0.3750  , 1.0000  ,
  0.1061  , 0.0000  , -0.1061 , 1.0000  , 0.7070  , 0.0000  , -0.7070 , 6.6641  , 1.0000  , 0.0000  , 0.3750  , 1.0000  ,
  0.1500  , 0.0000  , 0.0000  , 1.0000  , 1.0000  , 0.0000  , 0.0000  , 6.6641  , 1.0000  , 0.0000  , 0.3750  , 1.0000  ,
  0.1061  , 0.0000  , -0.1061 , 1.0000  , 0.7070  , 0.0000  , -0.7070 , 6.6641  , 1.0000  , 0.0000  , 0.7500  , 1.0000  ,
  0.0980  , -0.1061 , -0.0406 , 1.0000  , 0.6533  , -0.7070 , -0.2705 , 6.6641  , 1.0000  , 0.0000  , 0.7500  , 1.0000  ,
  0.0406  , -0.1061 , -0.0980 , 1.0000  , 0.2705  , -0.7070 , -0.6533 , 6.6641  , 1.0000  , 0.0000  , 0.7500  , 1.0000  ,
};

// fragment shader constants for sphere demo which give the light vector in the first column
// set to [1/sqrt(3), 1/sqrt(3), 1/sqrt(3)]
const float SPHERE_DEMO_LIGHT_VECTOR[4][4] = {
  {0.577, 0.0, 0.0, 0.0},
  {0.577, 0.0, 0.0, 0.0},
  {0.577, 0.0, 0.0, 0.0},
  {0.0,   0.0, 0.0, 0.0}
};

// converts a 32-bit float to a 16-bit float
// note: there is no overflow handling so it's assumed the exponent is within (-14,15)
// except 0 will correctly output 0
float16 to_half(float f) {
  // extract 32-bit representation from f so we can do bitwise operations
  union float_bits {
    float f;
    uint32_t b;
  } f_bits;
  f_bits.f = f;
  uint32_t bits_32 = f_bits.b;

  // float we're going to return
  float16 out_f = 0;

  // copy sign bit
  out_f |= (bits_32 & (1 << 31)) >> 16;
  // renormalize exponent to excess-16
  uint32_t old_exp = (bits_32 & (0b11111111 << 23)) >> 23;
  // clamp to 0 if old was 0
  uint32_t new_exp = old_exp < 113 ? 0 : (old_exp-127)+15;
  // copy only 5 bits of new exponent
  out_f |= (new_exp & 0b11111) << 10;
  // copy 10 bits of mantissa
  if(new_exp != 0) {
    out_f |= (bits_32 & 0b11111111110000000000000) >> 13;
  }

  // return result
  return out_f;
}

// takes product of 4x4 float matrices, out=a*b
void matrix_mult(float a[4][4], float b[4][4], float out[4][4]) {
  int i, j;
  for(i=0; i<4; i++) {
    for(j=0; j<4; j++) {
      // elements of result are dot product of a row i and b column j
      out[i][j] = a[i][0]*b[0][j]+a[i][1]*b[1][j]+a[i][2]*b[2][j]+a[i][3]*b[3][j];
    }
  }
}

// generate a matrix for the demo which transforms vertices as follows:
// rotate by an angle theta
// translate into middle of frustum
// apply perspective transformation for frustum 0.5 away from eye, 0.5 deep, and 1.0x1.0 near plane
void demo_matrix(float theta, float m[4][4]) {
  // matrix to rotate around y axis
  float y_rot_matrix[4][4] = {
    {cos(theta), 0.0, -sin(theta), 0.0},
    {0.0,        1.0, 0.0,         0.0},
    {sin(theta), 0.0, cos(theta),  0.0},
    {0.0,        0.0, 0.0,         1.0},
  };
  // matrix to translate by -0.75 in Z and 0.1sin(theta/4) in Y
  float trans_matrix[4][4] = {
    {1.0, 0.0, 0.0, 0.0},
    {0.0, 1.0, 0.0, 0.1*sin(theta/4.0)},
    {0.0, 0.0, 1.0, -0.75},
    {0.0, 0.0, 0.0, 1.0},
  };
  // transform from frustum into clip space
  // -n is the z-coordinate of the near plane
  // -f is the z-coordinate of the far plane
  // r is half the x-width of the near plane
  // t is half the y-height of the near plane
  float n = 0.5;
  float f = 1.0;
  float r = 0.5;
  float t = 0.5;
  float persp_matrix[4][4] = {
    {n/r, 0.0, 0.0,          0.0},
    {0.0, n/t, 0.0,          0.0},
    {0.0, 0.0, -(f+n)/(f-n), -2.0*f*n/(f-n)},
    {0.0, 0.0, -1.0,         0.0}
  };

  // product of translate and rotate matrices
  float trans_rot[4][4];
  matrix_mult(trans_matrix, y_rot_matrix, trans_rot);
  // multiply by perspective matrix into output
  matrix_mult(persp_matrix, trans_rot, m);
  /* print for debugging
  int i, j;
  for(i=0; i<4; i++) {
    for(j=0; j<4; j++) {
      Serial.print(m[i][j]);
      Serial.print(" ");
      char buff[8];
      sprintf(buff, "%04x", to_half(m[i][j]));
      Serial.print(buff);
      Serial.print(" ");
    }
    Serial.println("");
  }
  */
}

// write data to an address over the GPU data bus
void write_data(uint16_t addr, uint16_t data) {
  // set address
  GPIOB->ODR = addr;
  // set data
  GPIOC->ODR = data;
  // clock the data bus
  digitalWrite(WR_CLK_PIN, LOW);
  delayMicroseconds(CLK_PERIOD_US/2);
  digitalWrite(WR_CLK_PIN, HIGH);
  delayMicroseconds(CLK_PERIOD_US/2);
  // print for debugging purposes
  //char buff[64];
  //sprintf(buff, "addr %04x data %04x", addr, data);
  //Serial.println(buff);
}

// update all vertex constants from a 4x4 array, assuming each column is a constant quad
void update_v_consts(float consts[4][4]) {
  int i, j;
  // write every element to vertex constant element
  for(i=0; i<4; i++) {
    for(j=0; j<4; j++) {
      write_data(V_CONST_ADDR(i,j), to_half(consts[j][i]));
    }
  }
}

// update all fragment constants from a 4x4 array, assuming each column is a constant quad
void update_f_consts(const float consts[4][4]) {
  int i, j;
  // write every element to vertex constant element
  for(i=0; i<4; i++) {
    for(j=0; j<4; j++) {
      write_data(F_CONST_ADDR(i,j), to_half(consts[j][i]));
    }
  }
}

void init_tri_demo() {
  // 6 vertices in triangle demo
  write_data(V_COUNT_ADDR, TRI_DEMO_V_COUNT-1);
  // start vertex data at addr 0
  write_data(V_DATA_BASE_ADDR, 0);
  // vertex data made up of 2 quads
  write_data(V_DATA_SIZE_ADDR, 1);
  // write vertex data
  int v, i;
  // iterate through vertices
  for(v=0; v<TRI_DEMO_V_COUNT; v++) {
    // iterate through elements
    for(i=0; i<8; i++) {
      // send data from vertex data array
      write_data(V_DATA_ADDR(v*8+i), to_half(TRI_DEMO_VERTS[v*8+i]));
    }
  }
  
  // write vertex shader program
  for(i=0; i<TRI_DEMO_VPROG_SIZE; i++) {
    // write all 3 16-bit words, high first and low last
    write_data(V_PROG_ADDR(i, 2), TRI_DEMO_VERT_SHADER[i*3+0]);
    write_data(V_PROG_ADDR(i, 1), TRI_DEMO_VERT_SHADER[i*3+1]);
    write_data(V_PROG_ADDR(i, 0), TRI_DEMO_VERT_SHADER[i*3+2]);
  }
  
  // write fragment shader program
  for(i=0; i<TRI_DEMO_FPROG_SIZE; i++) {
    // write all 3 16-bit words, high first and low last
    write_data(F_PROG_ADDR(i, 2), VERT_COLOR_FRAG_SHADER[i*3+0]);
    write_data(F_PROG_ADDR(i, 1), VERT_COLOR_FRAG_SHADER[i*3+1]);
    write_data(F_PROG_ADDR(i, 0), VERT_COLOR_FRAG_SHADER[i*3+2]);
  }
}

void init_sphere_demo() {
  // vertices in sphere demo
  write_data(V_COUNT_ADDR, SPHERE_DEMO_V_COUNT-1);
  // start vertex data at addr 0
  write_data(V_DATA_BASE_ADDR, 0);
  // vertex data made up of 3 quads
  write_data(V_DATA_SIZE_ADDR, 2);
  // write vertex data
  int v, i;
  // iterate through vertices
  for(v=0; v<SPHERE_DEMO_V_COUNT; v++) {
    // iterate through elements
    for(i=0; i<12; i++) {
      // send data from vertex data array
      write_data(V_DATA_ADDR(v*12+i), to_half(SPHERE_DEMO_VERTS[v*12+i]));
    }
  }
  
  // write vertex shader program
  for(i=0; i<SPHERE_DEMO_VPROG_SIZE; i++) {
    // write all 3 16-bit words, high first and low last
    write_data(V_PROG_ADDR(i, 2), SPHERE_DEMO_VERT_SHADER[i*3+0]);
    write_data(V_PROG_ADDR(i, 1), SPHERE_DEMO_VERT_SHADER[i*3+1]);
    write_data(V_PROG_ADDR(i, 0), SPHERE_DEMO_VERT_SHADER[i*3+2]);
  }
  
  // write fragment shader program
  for(i=0; i<SPHERE_DEMO_FPROG_SIZE; i++) {
    // write all 3 16-bit words, high first and low last
    write_data(F_PROG_ADDR(i, 2), SPHERE_DEMO_FRAG_SHADER[i*3+0]);
    write_data(F_PROG_ADDR(i, 1), SPHERE_DEMO_FRAG_SHADER[i*3+1]);
    write_data(F_PROG_ADDR(i, 0), SPHERE_DEMO_FRAG_SHADER[i*3+2]);
  }

  // write fragment shader constants
  update_f_consts(SPHERE_DEMO_LIGHT_VECTOR);
}

void setup() {
  // init all used pins as output
  // yeah this is ugly, sorry, blame Arduino
  pinMode(WR_CLK_PIN, OUTPUT);
  pinMode(PB0, OUTPUT);
  pinMode(PB1, OUTPUT);
  pinMode(PB2, OUTPUT);
  pinMode(PB3, OUTPUT);
  pinMode(PB4, OUTPUT);
  pinMode(PB5, OUTPUT);
  pinMode(PB6, OUTPUT);
  pinMode(PB7, OUTPUT);
  pinMode(PB8, OUTPUT);
  pinMode(PB9, OUTPUT);
  pinMode(PB10, OUTPUT);
  pinMode(PB11, OUTPUT);
  pinMode(PB12, OUTPUT);
  pinMode(PB13, OUTPUT);
  pinMode(PB14, OUTPUT);
  pinMode(PB15, OUTPUT);
  pinMode(PC0, OUTPUT);
  pinMode(PC1, OUTPUT);
  pinMode(PC2, OUTPUT);
  pinMode(PC3, OUTPUT);
  pinMode(PC4, OUTPUT);
  pinMode(PC5, OUTPUT);
  pinMode(PC6, OUTPUT);
  pinMode(PC7, OUTPUT);
  pinMode(PC8, OUTPUT);
  pinMode(PC9, OUTPUT);
  pinMode(PC10, OUTPUT);
  pinMode(PC11, OUTPUT);
  pinMode(PC12, OUTPUT);
  pinMode(PC13, OUTPUT);
  pinMode(PC14, OUTPUT);
  pinMode(PC15, OUTPUT);

  Serial.begin(9600);

  // wait for a second to allow SPI display init to finish
  delay(1000);
  
  init_tri_demo();
}

void loop() {
  // transformation matrix to be put into vertex constant regs
  static float const_m[4][4];
  // theta for transformation matrix rotation
  static float theta = 0.2;//-PI/2;

  // generate new vertex constants
  demo_matrix(theta, const_m);
  // write the new vertex constants
  update_v_consts(const_m);
  // render new frame
  write_data(CTRL_ADDR, CTRL_RENDER);
  // wait for render
  delay(FRAMERATE_MS);
  // swap framebuffers
  write_data(CTRL_ADDR, CTRL_SWAP);
  // increment theta
  theta += 0.1;
}
