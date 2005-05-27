;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> This module combines all modules to form the Swindle language module.
;;>
;;> Note that it does not re-define `#%module-begin', so the language used
;;> for transformers is still the one defined by `turbo'.

(module swindle (lib "turbo.ss" "swindle")
  (require (lib "clos.ss" "swindle")
           (lib "extra.ss" "swindle"))
  (provide (all-from (lib "turbo.ss" "swindle"))
           (all-from (lib "clos.ss" "swindle"))
           (all-from (lib "extra.ss" "swindle")))
  (current-prompt-read (lambda () (display "=> ") (read)))
  (install-swindle-printer)
  ;; This comes out ugly in DrScheme.
  ;; (printf
  ;;  "Welcome to Swindle -- Eli Barzilay: Maze is Life! (eli@barzilay.org)\n")
  )
