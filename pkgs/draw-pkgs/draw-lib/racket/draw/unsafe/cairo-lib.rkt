#lang racket/base
(require ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         setup/dirs
         "../private/libs.rkt"
         "../private/utils.rkt")

(define-runtime-lib fontconfig-lib
  [(unix) (ffi-lib "libfontconfig" '("1" ""))]
  [(macosx)
   (ffi-lib "libpng16.16.dylib")
   (ffi-lib "libexpat.1.dylib")
   (ffi-lib "libfreetype.6.dylib")
   (ffi-lib "libfontconfig.1.dylib")]
  [(windows)
   (ffi-lib "zlib1.dll")
   (ffi-lib "libintl-8.dll")
   (ffi-lib "libpng16-16.dll")
   (ffi-lib "libexpat-1.dll")
   (ffi-lib "libfreetype-6.dll")
   (ffi-lib "libfontconfig-1.dll")])

(define-runtime-lib cairo-lib
  [(unix) (ffi-lib "libcairo" '("2" ""))]
  [(macosx)
   (ffi-lib "libpixman-1.0.dylib")
   (ffi-lib "libcairo.2.dylib")]
  [(windows)
   (ffi-lib "libpixman-1-0.dll")
   (ffi-lib "libcairo-2.dll")])

;; A Racket-specific patch to Fontconfig defines FcSetFallbackDirs(),
;; which lets us set default paths to point to a Racket-specific
;; directory. If FcSetFallbackDirs() isn't defined, then we want
;; the system-defined directories, anyway.
(let ([FcSetFallbackDirs (get-ffi-obj 'FcSetFallbackDirs
                                      fontconfig-lib
                                      (_fun _path _path -> _void)
                                      (lambda () #f))]
      [FcSetConfigDir (get-ffi-obj 'FcSetConfigDir
                                   fontconfig-lib
                                      (_fun _path -> _void)
                                      (lambda () #f))])
  (when (and FcSetFallbackDirs
             FcSetConfigDir)
    (define share-dir (find-share-dir))
    (when share-dir
      (FcSetFallbackDirs (build-path share-dir "fonts")
                         (build-path (find-system-path 'addon-dir) "font-cache"))
      (FcSetConfigDir (build-path share-dir "fonts")))))

(provide (protect-out cairo-lib))
