if test "${ac_cv_sizeof_void_p}" != "4" ; then
  if test "${ac_cv_sizeof_void_p}" != "8" ; then
    echo "Something has gone wrong getting the pointer size; see config.log"
    exit 1
  fi
fi
