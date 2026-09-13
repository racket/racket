if test "${enable_natipkg}" = "no" ; then
  if "${enable_nonatipkg}" = "" ; then
    if "${enable_sofind}" = "" ; then
       enable_nonatipkg=yes
    fi
  fi
fi

if test "${enable_nonatipkg}" = "yes" ; then
  if test "${enable_natipkg}" = "yes" ; then
     echo 'cannot specify both `--enable-natipkg` and `--enable-nonatipkg`'
     exit 1
  fi
elif test "${enable_nonatipkg}" = "no" ; then
  if "${enable_nonatipkg}" = "no" ; then
    if "${enable_sofind}" = "" ; then
       echo 'cannot specify both `--disable-natipkg` and `--disable-nonatipkg`'
       echo 'without `--enable-sofind=<sofind>`'
       exit 1
    fi
  fi
fi

if test "${enable_sofind}" != "" ; then
  if test "${enable_natipkg}" = "yes" ; then
     echo 'cannot specify both `--enable-natipkg` and `--enable-sofind`'
     exit 1
  fi
  if test "${enable_nonatipkg}" = "yes" ; then
     echo 'cannot specify both `--enable-nonatipkg` and `--enable-sofind`'
     exit 1
  fi
fi
