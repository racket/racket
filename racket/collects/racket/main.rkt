#lang racket/base

(require racket/contract
         racket/class
         racket/unit
         racket/dict
         racket/include
         racket/pretty
         racket/math
         racket/match
         racket/shared
         racket/set
         racket/tcp
         racket/udp
         racket/list
         racket/vector
         racket/string
         racket/bytes
         racket/function
         racket/path
         racket/file
         racket/place
         racket/future
         racket/port
         racket/cmdline
         racket/promise
         racket/bool
         racket/stream
         racket/sequence
         racket/local
         racket/system
         racket/format
         (for-syntax racket/base))

(provide (all-from-out racket/contract
                       racket/class
                       racket/unit
                       racket/dict
                       racket/include
                       racket/pretty
                       racket/math
                       racket/match
                       racket/shared
                       racket/base
                       racket/set
                       racket/tcp
                       racket/udp
                       racket/list
                       racket/vector
                       racket/string
                       racket/bytes
                       racket/function
                       racket/path
                       racket/file
                       racket/place
                       racket/future
                       racket/port
                       racket/cmdline
                       racket/promise
                       racket/bool
                       racket/stream
                       racket/sequence
                       racket/local
                       racket/system
                       racket/format)
         (for-syntax (all-from-out racket/base)))

(module reader syntax/module-reader
  racket)
