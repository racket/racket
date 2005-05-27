
;; Uses editor.ss to open a frame for each argument

(module edit-main mzscheme
  (require "edit.ss"
	   (lib "cmdline.ss"))

  (command-line
   "Edit"
   (current-command-line-arguments)
   [args files
	 (if (null? files)
	     (new-text-frame #f)
	     (for-each new-text-frame files))]))
