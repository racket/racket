if test "${enable_ios}" != "" ; then
  case "$host_cpu" in
    aarch64)
     IOS_ARCH=arm64
     ;;
    *)
     IOS_ARCH=$host_cpu
     ;;
  esac
  case "${enable_ios}" in
    iPhoneOS|iPhoneSimulator)
     ios_sdk=/Applications/Xcode.app/Contents/Developer/Platforms/${enable_ios}.platform/Developer/SDKs/${enable_ios}.sdk
     echo "=== Using inferred iOS SDK path ${ios_sdk}"
     ;;
    *)
     ios_sdk="${enable_ios}"
     ;;
  esac
  IOS_PHONE_VERS="6.0"
  PREFLAGS="$PREFLAGS -DTARGET_OS_IPHONE=1 -arch ${IOS_ARCH} -isysroot ${ios_sdk} -miphoneos-version-min=${IOS_PHONE_VERS}"
  LDFLAGS="$LDFLAGS -arch ${IOS_ARCH} -isysroot ${ios_sdk} -miphoneos-version-min=${IOS_PHONE_VERS} -liconv"
fi
