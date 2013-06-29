#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/class
         racket/runtime-path
         racket/file
         racket/gui/base)
(define-runtime-path snip "snip")

(fire-up-drracket-and-run-tests
 (λ ()
   
   (define drs (wait-for-drracket-frame))
   (define tmpdir (make-temporary-file "drracketsniptest~a" 'directory))
   (define defs (queue-callback/res (λ () (send drs get-definitions-text))))
   (for ([rfile (in-list (directory-list snip))])
     (define file (build-path snip rfile))
     (when (file-exists? file)  ;; skip subdirectories
       (printf "  trying ~a\n" rfile)
       (queue-callback/res 
        (λ () (send defs load-file file)))
       (save-drracket-window-as (build-path tmpdir rfile))
       (define drs2 (wait-for-drracket-frame))
       (unless (eq? drs drs2)
         (error 'snips.rkt "lost drracket frame while saving ~s" rfile))))
   
   (delete-directory/files tmpdir)))



