#lang typed/racket


(current-locale)


(define my-input (open-input-string "This (is a long) input \"string\""))
(define-values (my-output-reader my-output) (make-pipe))
(define my-error-port (open-output-bytes))

(port-count-lines! my-input)

(input-port? my-input)
(output-port? my-output-reader)

(port-closed? my-error-port)
(file-stream-port? my-output)
(terminal-port? my-output)

(parameterize ((current-input-port my-input)
               (current-output-port my-output)
               (current-error-port my-error-port))
  (write (read))
  (newline)
  (flush-output))

(file-stream-buffer-mode my-input)
(file-position my-input)
(port-next-location my-input)

(read my-output-reader)
(get-output-bytes my-error-port)

(close-input-port my-input)
(close-input-port my-output-reader)
(close-output-port my-output)
(close-output-port my-error-port)
