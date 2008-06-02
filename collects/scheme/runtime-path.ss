#lang scheme/base
(require (for-syntax scheme/base))

(require mzlib/runtime-path)
(provide (all-from-out mzlib/runtime-path)
         (for-syntax #%datum))


