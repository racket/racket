show_explicitly_enabled "${enable_libs}" "Installation of static libraries (if any)"
show_explicitly_disabled "${enable_libs}" "Installation of static libraries (if any)"

INSTALL_LIBS_ENABLE=no-install

if test "${enable_libs}" != "no" ; then
  # Intended to be canceled for some platforms:
  INSTALL_LIBS_ENABLE=install
fi
