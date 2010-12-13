#lang scheme/base
(require ffi/unsafe
         ffi/unsafe/define
         "../private/libs.rkt")

(provide (protect-out
          define-glib
          define-gmodule
          define-gobj))

(define-runtime-lib glib-lib 
  [(unix) (ffi-lib "libglib-2.0" '("0"))]
  [(macosx) (ffi-lib "libglib-2.0.0")]
  [(windows) (ffi-lib "libglib-2.0-0.dll")])

(define-runtime-lib gmodule-lib 
  [(unix) (ffi-lib "libgmodule-2.0" '("0"))]
  [(macosx) 
   (ffi-lib "libgmodule-2.0.0.dylib")]
  [(windows) 
   (ffi-lib ,(if win64?
		 "libgthread-2.0-0.dll"
		 "libgmodule-2.0-0.dll"))
   (ffi-lib "libgmodule-2.0-0.dll")])

(define-runtime-lib gobj-lib 
  [(unix) (ffi-lib "libgobject-2.0" '("0"))]
  [(macosx) (ffi-lib "libgobject-2.0.0")]
  [(windows) (ffi-lib "libgobject-2.0-0.dll")])

(define-ffi-definer define-glib glib-lib)
(define-ffi-definer define-gmodule gmodule-lib)
(define-ffi-definer define-gobj gobj-lib)
