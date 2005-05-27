
#include <Devices.h>

void FillInNetPointers(void **a, void **b, void **c, void **d, void **e, void **f, void **g)
{
  *a = PBOpenSync;
  *b = PBControlSync;
  *c = PBControlAsync;
  *d = SysEnvirons;
  *e = GetWDInfo;
  *f = NewRoutineDescriptor;
  *g = CallUniversalProc;
}
