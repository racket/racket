AC_ARG_ENABLE(noopt,   [  --enable-strip          strip debug on install (usually enabled by default)])

STRIP_DEBUG=":"
STRIP_LIB_DEBUG=":"
strip_debug_flags=""
enable_strip_by_default=yes
strip_needs_dash_s=no
