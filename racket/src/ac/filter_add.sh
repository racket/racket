# Added to the front of an autoconf script to handle
# `CFLAGS+=...` and similar additions, athering the
# content into vabiables like `CFLAGS_ADD` and removing
# them from autoconf's view

for arg do
    shift
    case $arg in
        CFLAGS+=*)
            CFLAGS_ADD="${CFLAGS_ADD} "`echo $arg | sed -e 's/^CFLAGS+=//'`
            ;;
        CPPFLAGS+=*)
            CPPFLAGS_ADD="${CPPFLAGS_ADD} "`echo $arg | sed -e 's/^CPPFLAGS+=//'`
            ;;
        LDFLAGS+=*)
            LDFLAGS_ADD="${LDFLAGS_ADD} "`echo $arg | sed -e 's/^LDFLAGS+=//'`
            ;;
        LIBS+=*)
            LIBS_ADD="${LIBS_ADD} "`echo $arg | sed -e 's/^LIBS+=//'`
            ;;
        *)
            set -- "$@" "$arg"
            ;;
    esac
done
