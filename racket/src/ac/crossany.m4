show_explicitly_enabled "${enable_crossany}" "Own cross-compile target as machine-independent"

CROSS_COMPILE_TARGET_KIND=machine
if test "${enable_crossany}" = "yes" ; then
  CROSS_COMPILE_TARGET_KIND=any
fi
