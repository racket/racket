
(module unit scheme/base
  (require mzlib/unit
           scheme/contract/base
           (for-syntax scheme/base
                       syntax/struct))
  (provide (except-out (all-from-out mzlib/unit)
                       struct struct/ctc
                       struct~r struct~r/ctc)
           (rename-out [struct~s struct]
                       [struct~s/ctc struct/ctc])))

