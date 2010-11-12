
#if (defined(__WIN32__) || defined(WIN32) || defined(_WIN32))
# define MZ_DLLEXPORT __declspec(dllexport)
#else
# define MZ_DLLEXPORT
#endif
