(module main scheme/base
  (require scheme/contract
           scheme/class
           scheme/unit
           scheme/include
           scheme/pretty
           scheme/math
           scheme/match
           scheme/shared
           scheme/tcp
           scheme/udp
           scheme/list
           scheme/path
           scheme/file
           scheme/cmdline
           scheme/promise
           scheme/bool
           scheme/local
           (for-syntax scheme/base))

  (provide (all-from-out scheme/contract
                         scheme/class
                         scheme/unit
                         scheme/include
                         scheme/pretty
                         scheme/math
                         scheme/match
                         scheme/shared
                         scheme/base
                         scheme/tcp
                         scheme/udp
                         scheme/list
                         scheme/path
                         scheme/file
                         scheme/cmdline
                         scheme/promise
                         scheme/bool
                         scheme/local)
           (for-syntax (all-from-out scheme/base))))
