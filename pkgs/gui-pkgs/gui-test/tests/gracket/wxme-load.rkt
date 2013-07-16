#lang racket/base
(require racket/gui/base)

(parameterize ([current-namespace (make-gui-namespace)])
  (namespace-require 'racket/gui/init)
  (void (load (collection-file-path "prog8.mre" "tests" "gracket"))))
