
;; Uses editor.rkt to open a frame for each argument

(module edit-main mzscheme
  (require "edit.rkt"
           mzlib/cmdline)

  (command-line
   "Edit"
   (current-command-line-arguments)
   [args files
	 (if (null? files)
	     (new-text-frame #f)
	     (for-each new-text-frame files))]))
