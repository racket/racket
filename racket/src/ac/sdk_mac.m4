    if test "${enable_sdk}" != "" ; then 
      PREFLAGS="$PREFLAGS -isysroot ${enable_sdk} -mmacosx-version-min=10.4 -DEXTRA_EXCEPTION_STUBS"
      LDFLAGS="$LDFLAGS -isysroot ${enable_sdk} -mmacosx-version-min=10.4"
      if test "${CC}" = "gcc" ; then
        CC=gcc-4.0
      fi
      if test "${CPP}" = "gcc -E" ; then
        CPP="gcc-4.0 -E"
        need_cpp_in_extras=yes
      fi
      if test "${LD}" = "gcc" ; then
        LD=gcc-4.0
        need_ld_in_extras=yes
      fi
      if test "$PROPAGATE_SUB_CONFIGURE" != "no" ; then
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CPPFLAGS="'"'"${PREFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
      fi
      need_cc_in_extras=yes
      if test "$ORIG_CC_FOR_BUILD" = "" ; then
        CC_FOR_BUILD="$CC"
      fi
    fi
    
    if test "${enable_sdk5}" != "" ; then 
      PREFLAGS="$PREFLAGS -isysroot ${enable_sdk5} -mmacosx-version-min=10.5"
      LDFLAGS="$LDFLAGS -isysroot ${enable_sdk5} -mmacosx-version-min=10.5"
      if test "$PROPAGATE_SUB_CONFIGURE" != "no" ; then
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CPPFLAGS="'"'"${PREFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
      fi
    fi
    
    if test "${enable_sdk6}" != "" ; then 
      PREFLAGS="$PREFLAGS -isysroot ${enable_sdk6} -mmacosx-version-min=10.6"
      LDFLAGS="$LDFLAGS -isysroot ${enable_sdk6} -mmacosx-version-min=10.6"
      if test "$PROPAGATE_SUB_CONFIGURE" != "no" ; then
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CPPFLAGS="'"'"${PREFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
      fi
    fi

    if test "${enable_sdk9}" != "" ; then 
      PREFLAGS="$PREFLAGS -isysroot ${enable_sdk9} -mmacosx-version-min=10.9"
      LDFLAGS="$LDFLAGS -isysroot ${enable_sdk9} -mmacosx-version-min=10.9"
      if test "$PROPAGATE_SUB_CONFIGURE" != "no" ; then
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CPPFLAGS="'"'"${PREFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
      fi
    fi

    if test "${enable_sdk11}" != "" ; then 
      PREFLAGS="$PREFLAGS -isysroot ${enable_sdk11} -mmacosx-version-min=11.0"
      LDFLAGS="$LDFLAGS -isysroot ${enable_sdk11} -mmacosx-version-min=11.0"
      if test "$PROPAGATE_SUB_CONFIGURE" != "no" ; then
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CFLAGS="'"'"${CFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CPPFLAGS="'"'"${PREFLAGS}"'"'
        SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} LDFLAGS="'"'"${LDFLAGS}"'"'
      fi
    fi

    # Force 32-bit build unless mac64 is enabled:
    if test "${enable_mac64}" != "yes" ; then
      if test "$host_cpu" != "powerpc" ; then
        if test "${ORIG_CC}" = "" ; then
           PREFLAGS="${PREFLAGS} -m32"
           CPPFLAGS="${CPPFLAGS} -m32"
           LDFLAGS="${LDFLAGS} -m32"
           # To make the libffi build work, we have to fold -m32 into CC
           # instead of CFLAGS:
           if test "$PROPAGATE_SUB_CONFIGURE" != "no" ; then
             SUB_CONFIGURE_EXTRAS="${SUB_CONFIGURE_EXTRAS} CC="'"'"${CC}"' -m32"'
           fi
           need_cc_in_extras=no
           if test "$build_cpu" = "x86_64" ; then
             # AG_PROC_CC has already decided that we weren't
             # cross compiling, so change that decision
             cross_compiling=yes
             echo may be cross compiling after all
           fi
        fi
      fi
    fi
