show_explicitly_disabled "${enable_mac64}" "64-bit Mac OS"

show_explicitly_enabled "${enable_libfw}" "Frameworks-to-system"
show_explicitly_enabled "${enable_userfw}" "Frameworks-to-user"

if test "${enable_sdk}" != "" ; then
   if test "${enable_sdk5}" != "" ; then
     echo "ERROR: cannot specify both --enable-sdk and --enable-sdk5"
   fi
   if test "${enable_sdk6}" != "" ; then
     echo "ERROR: cannot specify both --enable-sdk and --enable-sdk6"
   fi
   if test "${enable_ios}" != "" ; then
     echo "ERROR: cannot specify both --enable-sdk and --enable-ios"
   fi
   echo "=== Using Mac OS 10.4 SDK directory ${enable_sdk}"
fi
if test "${enable_sdk5}" != "" ; then
   if test "${enable_sdk6}" != "" ; then
     echo "ERROR: cannot specify both --enable-sdk5 and --enable-sdk6"
   fi
   if test "${enable_ios}" != "" ; then
     echo "ERROR: cannot specify both --enable-sdk5 and --enable-ios"
   fi
   echo "=== Using Mac OS 10.5 SDK directory ${enable_sdk}"
fi
if test "${enable_sdk6}" != "" ; then
   if test "${enable_ios}" != "" ; then
     echo "ERROR: cannot specify both --enable-sdk6 and --enable-ios"
   fi
   echo "=== Using Mac OS 10.6 SDK directory ${enable_sdk6}"
fi
if test "${enable_ios}" != "" ; then
   echo "=== Using ios SDK directory ${enable_ios}"
fi

if test "${enable_sysroot}" != "" ; then
   echo "=== Using sysroot directory ${enable_sysroot}"
fi
