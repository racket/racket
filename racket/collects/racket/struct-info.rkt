
(module struct-info '#%kernel
  (#%declare #:unsafe-cross-phase-persistent)
  (#%require "private/struct-info.rkt"
             (for-template "private/define-struct.rkt"))
  (#%provide (all-from "private/struct-info.rkt")
             checked-struct-info?))
