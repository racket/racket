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
     IOS_SDK=/Applications/Xcode.app/Contents/Developer/Platforms/${enable_ios}.platform/Developer/SDKs/${enable_ios}.sdk
     echo "=== Using inferred iOS SDK path ${IOS_SDK}"
     ;;
    *)
     IOS_SDK="${enable_ios}"
     ;;
  esac
  IOS_PHONE_VERS="6.0"
  IOS_SDK_FLAGS="-miphoneos-version-min=${IOS_PHONE_VERS}"
  case "${IOS_SDK}" in
      # When -arch is arm64-apple-darwin and -sdk is iPhoneSimulator,
      # passing -miphoneos-version-min makes clang think it's targeting
      # iPhoneOS instead of iPhoneSimulator, which causes the configure
      # script to fail. There is no -miphonesimulator-version-min flag.
      # The -mtargetos flag exhibits the same behavior.
      *iPhoneSimulator.sdk)
          IOS_SDK_FLAGS=""
          ;;
  esac
  PREFLAGS="$PREFLAGS -DTARGET_OS_IPHONE=1 -arch ${IOS_ARCH} -isysroot ${IOS_SDK} ${IOS_SDK_FLAGS}"
  LDFLAGS="$LDFLAGS -arch ${IOS_ARCH} -isysroot ${IOS_SDK} ${IOS_SDK_FLAGS} -liconv"
fi
