
# ifdef MZ_PRECISE_GC
#  include "../gc2/gc2.h"
# else
#  ifdef USE_SENORA_GC
#   include "../sgc/sgc.h"
#  else
#   include "../gc/gc.h"
#  endif
# endif
