#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../private/libs.rkt"
         "../private/utils.rkt")

(define-runtime-lib cairo-lib
  [(unix) (ffi-lib "libcairo" '("2" ""))]
  [(macosx)
   (ffi-lib "libpixman-1.0.dylib")
   (ffi-lib "libpng16.16.dylib")
   (ffi-lib "libexpat.1.dylib")
   (ffi-lib "libfreetype.6.dylib")
   (ffi-lib "libfontconfig.1.dylib")
   (ffi-lib "libcairo.2.dylib")]
  [(windows)
   (ffi-lib "zlib1.dll")
   (ffi-lib "libintl-8.dll")
   (ffi-lib "libpng16-16.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "libfreetype-6.dll")
   (ffi-lib "libfontconfig-1.dll")
   (ffi-lib "libpixman-1-0.dll")
   (ffi-lib "libcairo-2.dll")])

(provide (protect-out cairo-lib))
