#ifdef _WIN64
# include "../../bc/foreign/libffi/src/x86/win64_intel.S"
#else
# include "../../bc/foreign/libffi/src/x86/sysv_intel.S"
#endif
