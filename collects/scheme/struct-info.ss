
(module struct-info '#%kernel
  (#%require "private/struct-info.ss"
             (for-template "private/define-struct.ss"))
  (#%provide (all-from "private/struct-info.ss")
             checked-struct-info?))
