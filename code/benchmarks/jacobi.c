#include "stdlib.h"

void jacobi(double* dst, double* src, unsigned int rows, unsigned int columns) {
    double *C =  dst +   columns + 1;
    double *N =  src             + 1;
    double *W =  src +   columns    ;
    double *E =  src +   columns + 2;
    double *S =  src + 2*columns + 1;

    for(size_t iy = 0; iy < rows - 2; ++iy) {
        for(size_t ix = 0; ix < columns - 2; ++ix) {
            size_t idx = iy * columns + ix;
            C[idx] = 0.25 * (N[idx] + S[idx] + W[idx] + E[idx]);
        }
    }
}
