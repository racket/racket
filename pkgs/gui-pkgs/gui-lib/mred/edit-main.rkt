
;; Uses editor.rkt to open a frame for each argument

(module edit-main mzscheme
  (require "edit.rkt"
           mzlib/cmdline)

  (module test racket/base)

  (command-line
   "Edit"
   (current-command-line-arguments)
   [args files
	 (if (null? files)
	     (new-text-frame #f)
	     (for-each new-text-frame files))]))
