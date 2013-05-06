#lang scheme/base

(require scheme/contract
         scheme/class
         scheme/unit
         scheme/dict
         scheme/include
         scheme/pretty
         scheme/math
         scheme/match
         scheme/shared
         scheme/tcp
         scheme/udp
         scheme/list
         scheme/vector
         scheme/string
         scheme/function
         scheme/path
         scheme/file
         scheme/port
         scheme/cmdline
         scheme/promise
         scheme/bool
         scheme/local
         scheme/nest
         (for-syntax scheme/base))

(provide (all-from-out scheme/contract
                       scheme/class
                       scheme/unit
                       scheme/dict
                       scheme/include
                       scheme/pretty
                       scheme/math
                       scheme/match
                       scheme/shared
                       scheme/base
                       scheme/tcp
                       scheme/udp
                       scheme/list
                       scheme/vector
                       scheme/string
                       scheme/function
                       scheme/path
                       scheme/file
                       scheme/port
                       scheme/cmdline
                       scheme/promise
                       scheme/bool
                       scheme/local
                       scheme/nest)
         (for-syntax (all-from-out scheme/base)))

(module reader syntax/module-reader
  scheme)
