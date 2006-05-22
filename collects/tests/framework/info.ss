(module info (lib "infotab.ss" "setup")
  (define name "Framework Test Suite")
  (define compile-omit-files '("key-specs.ss" "utils.ss" "receive-sexps-port.ss"))
  #| Do not create these launchers -- they won't look good in /usr/bin
  (define mred-launcher-libraries (list "framework-test-engine.ss"))
  (define mred-launcher-names (list "Framework Test Engine"))
  (define mzscheme-launcher-libraries (list "main.ss"))
  (define mzscheme-launcher-names (list "Framework Test"))
  |#
  )
