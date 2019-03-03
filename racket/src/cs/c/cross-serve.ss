;; The client half of this interaction is in "cs/linklet/cross-compile.ss".

;; Communication uses the Chez Scheme printer and reader so make the
;; server independent from Racket, although it is run by the Racket
;; executable.

;; Suppress printout on startup:
(define original-output-port (current-output-port))
(let-values ([(o get) (open-bytevector-output-port (current-transcoder))])
  (current-output-port o))

;; Server function to run after cross-compiler is loaded:
(define (serve-cross-compile)
  ;; Don't exit due to Ctl-C:
  (keyboard-interrupt-handler void)
  ;; Restore output
  (current-output-port original-output-port)
  ;; Set up the enviornment
  (expand `(import (rename (rumble)
                           [correlated? syntax?]
                           [correlated-source syntax-source]
                           [correlated-line syntax-line]
                           [correlated-column syntax-column]
                           [correlated-position syntax-position]
                           [correlated-span syntax-span]
                           [correlated-e syntax-e]
                           [correlated->datum syntax->datum]
                           [datum->correlated datum->syntax]
                           [correlated-property syntax-property]
                           [correlated-property-symbol-keys syntax-property-symbol-keys])
                   (thread)
                   (io)
                   (regexp)
                   (linklet)))
  (expand-omit-library-invocations #t)
  ;; Serve requests to compile or to fasl data:
  (let loop ()
    (let ([cmd (read)])
      (unless (eof-object? cmd)
        (let-values ([(o get) (open-bytevector-output-port)])
          (case cmd
            [(compile)
             (compile-to-port (list `(lambda () ,(read-fasled))) o)]
            [else
             (error 'serve-cross-compile (format "unrecognized command: ~s" cmd))])
          (let ([result (get)])
            (write result)
            (newline)
            (flush-output-port)))
        (loop)))))

;; ----------------------------------------

(define (read-fasled)
  (let ([bstr (read)])
    (fasl-read (open-bytevector-input-port bstr))))
