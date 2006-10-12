
(module rep mzscheme
  (require "pread.ss")

  ;; Change the input port and readline-prompt hook

  (if (eq? 'stdin (object-name (current-input-port)))
    (current-input-port readline-input)
    ;; don't replace some random port
    (error 'readline-input
           "invoke this library when the current-input-port is stdin"))

  (current-prompt-read 
   (let ([orig-read (current-prompt-read)]
         [orig-input (current-input-port)])
     (lambda ()
       (if (eq? (current-input-port) orig-input)
           (read-cmdline-syntax)
	   (orig-read))))))
