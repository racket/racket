
(module rep mzscheme
  (require "pread.ss")

  (current-prompt-read 
   (let ([orig-read (current-prompt-read)]
	 [orig-input (current-input-port)])
     (lambda ()
       (if (eq? (current-input-port) orig-input)
	   (prompt-read-using-readline (lambda (n) (if (zero? n) "> " "  ")))
	   (orig-read))))))


