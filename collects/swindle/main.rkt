;;; Written by Eli Barzilay: Maze is Life!  (eli@barzilay.org)

;;> This module combines all modules to form the Swindle language module.
;;>
;;> Note that it does not re-define `#%module-begin', so the language used
;;> for transformers is still the one defined by `turbo'.

#lang s-exp swindle/turbo

(require swindle/clos swindle/extra)
(provide (all-from swindle/turbo)
         (all-from swindle/clos)
         (all-from swindle/extra))
(current-prompt-read
 (let ([old-prompt-read (current-prompt-read)])
   (lambda () (display "=") (flush-output) (old-prompt-read))))
(install-swindle-printer)
