(module main scheme/base
  (require scheme/contract
           scheme/class
           scheme/unit
           scheme/include
           scheme/pretty
           scheme/math
           scheme/match
           scheme/tcp
           scheme/udp
           scheme/list
           (for-syntax scheme/base))

  (provide (all-from-out scheme/contract
                         scheme/class
                         scheme/unit
                         scheme/include
                         scheme/pretty
                         scheme/math
                         scheme/match
                         scheme/base
                         scheme/tcp
                         scheme/udp
                         scheme/list)
           (for-syntax (all-from-out scheme/base))))
