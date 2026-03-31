#include <stdio.h>

#ifdef _WIN32
# define EXPORT __declspec (dllexport)
#else
# define EXPORT /* */
#endif

typedef struct {
  int i;
  float f;
} intfloat;

typedef double (*double_build_t)(int i, float f);
typedef intfloat (*intfloat_build_t)(int i, float f);

EXPORT int int_sum(int a, int b) {
  return a + b;
}

EXPORT double double_sum(double a, double b) {
  return a + b;
}

EXPORT double intfloat_sum(intfloat n) {
  return n.f + n.i;
}

EXPORT double intfloat_sum_content(intfloat *n) {
  return n->f + n->i;
}

EXPORT intfloat intfloat_build(int i, float f) {
  intfloat n = { i, f };
  return n;
}

EXPORT double double_built(double_build_t proc) {
  return 2 * proc(10, 100.0);
}

EXPORT double multiply_built(double_build_t proc, int n) {
  double v = 0.0;
  int i;
  for (i = 0; i < n; i++) {
    v += proc(10, 100.0);
  }
  return v;
}

EXPORT double multiply_built2(double_build_t proc, double_build_t proc2, int n) {
  double v = 0.0;
  int i;
  for (i = 0; i < n; i++) {
    v += proc(10, 100.0);
    v += proc2(10, 100.0);
  }
  return v;
}

EXPORT double intfloat_sum_built(intfloat_build_t proc) {
  return intfloat_sum(proc(10, 100.0));
}
