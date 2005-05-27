#!/bin/sh
#|
exec mzscheme -mvt "$0" "$@"
|#

(module make mzscheme
  (require (lib "launcher.ss" "launcher"))

  (define tiny-program
    '(begin [use-compiled-file-paths '()]
            (current-directory (build-path (collection-path "mzlib") 'up 'up))
            (load "install")
            (main '("install" "-i"))))
  (make-mred-launcher
   `("-mvq" "-e" ,(format "~s" tiny-program))
   (mred-program-launcher-path "Finish Install")
   (cons
    '(forget-exe? . #t)
    (build-aux-from-path (build-path
                          (collection-path "finish-install")
                          "finish-install")))))
