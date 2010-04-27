
(module unit racket/base
  (require mzlib/unit
           racket/contract/base
           (for-syntax racket/base
                       syntax/struct))
  (provide (except-out (all-from-out mzlib/unit)
                        struct struct/ctc
                        struct~r struct~r/ctc
                        struct~s struct~s/ctc)
           (rename-out [struct~r/ctc struct/ctc])))
