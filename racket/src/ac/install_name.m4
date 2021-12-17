
if test "${enable_racket}" != "" ; then
  INSTALL_NAME_TOOL='$(INSTALL_NAME_TOOL_VIA_RACKET)'
else
  INSTALL_NAME_TOOL=install_name_tool
fi
