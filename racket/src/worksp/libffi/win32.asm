#ifdef _WIN64
# include "../../bc/foreign/libffi/src/x86/win64.S"
#else
# include "../../bc/foreign/libffi/src/x86/win32.S"
#endif
