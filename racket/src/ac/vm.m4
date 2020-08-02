if test "${enable_bconly}" = "yes" ; then
   if test "${enable_csonly}" = "yes" ; then
     echo "ERROR: cannot specify both --enable-bconly and --enable-csonly"
   fi
fi

if test "${enable_cgcdefault}" = "yes" ; then
   if test "${enable_bcdefault}" = "no" ; then
     echo "ERROR: cannot specify both --enable-cgcdefault and --disable-bcdefault"
   fi
   if test "${enable_csdefault}" = "yes" ; then
     echo "ERROR: cannot specify both --enable-cgcdefault and --enable-csdefault"
   fi
   enable_bcdefault=yes
fi

if test "${enable_bcdefault}" = "yes" ; then
   if test "${enable_csdefault}" = "yes" ; then
     echo "ERROR: cannot specify both --enable-bcdefault and --enable-csdefault"
   fi
   if test "${enable_bconly}" = "no" ; then
     echo "ERROR: cannot specify both --enable-bcdefault and --disable-bconly"
   fi
   enable_csdefault=no
fi

if test "${enable_csdefault}" = "yes" ; then
   if test "${enable_csonly}" = "no" ; then
     echo "ERROR: cannot specify both --enable-csdefault and --disable-csonly"
   fi
   enable_bcdefault=no
fi

if test "${enable_bconly}" = "yes" ; then
   enable_bcdefault=yes
   enable_csdefault=no
fi

if test "${enable_csonly}" = "yes" ; then
   enable_csdefault=yes
   enable_bcdefault=no
fi

if test "${enable_csdefault}" = "" ; then
   if test "${enable_bcdefault}" = "" ; then
      # Pick a default default here, but make it consistent with "src/configure"
      enable_csdefault=yes
      enable_bcdefault=no
   fi
fi

show_explicitly_enabled "${enable_csdefault}" "CS executables without suffix"
show_explicitly_enabled "${enable_bcdefault}" "BC executables without suffix"
show_explicitly_enabled "${enable_cgcdefault}" "CGC as default" "Note that this mode is NOT RECOMMENDED for normal Racket use"
