# Pass certain configure args on to rktio
keep_configure_args=
fixup_prev=
eval "set x $ac_configure_args"
shift
for fixup_arg
do
  case $fixup_arg in
  # Strip away all feature choices
  -enable* | --enable* | -disable* | --disable*)
    ;;
  -collects* | --collects* | -apps* | --apps* | -pkgs* | --pkgs*)
    ;;
  *)
    case $fixup_arg in
    *\'*) fixup_arg=`echo "$fixup_arg" | sed "s/'/'\\\\\\\\''/g"` ;;
    esac
    keep_configure_args="$keep_configure_args '$fixup_arg'" ;;
  esac
done
