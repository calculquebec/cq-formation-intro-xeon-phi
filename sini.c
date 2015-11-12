#include <math.h>

#pragma omp declare target
void sini(float *in, float *out, int i)
{
  double sinx = sin( in[i] );
  out[i] = sinx*sinx;
}
