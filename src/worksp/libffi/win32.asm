#ifdef _WIN64
# include "../../foreign/libffi/src/x86/win64.S"
#else
# include "../../foreign/libffi/src/x86/win32.S"
#endif
