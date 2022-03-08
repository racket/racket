#if defined(_M_ARM64)
# include "../../bc/foreign/libffi/src/aarch64/win64_armasm.S"
#elif defined(_WIN64)
# include "../../bc/foreign/libffi/src/x86/win64_intel.S"
#else
# include "../../bc/foreign/libffi/src/x86/sysv_intel.S"
#endif
