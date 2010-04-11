
(module unit scheme/base
  (require mzlib/unit
           scheme/contract/base
           (for-syntax scheme/base
                       syntax/struct))
  (provide (except-out (all-from-out mzlib/unit)
                       struct struct/ctc)
           (rename-out [struct~ struct]
                       [struct~/ctc struct/ctc])))

