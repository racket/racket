;;; Copyright (c) 2000-2015 Andrew W. Keep, R. Kent Dybvig
;;; See the accompanying file Copyright for details

(library (tests unit-test-helpers-implementation)
  (export with-output-to-string display-condition format-error-message)
  (import (chezscheme))
  (define-syntax format-error-message
    (syntax-rules ()
      [(_ args ...) (parameterize ([print-level 3] [print-length 6]) (format args ...))])))
