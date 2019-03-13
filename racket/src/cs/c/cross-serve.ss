;; The client half of this interaction is in "cs/linklet/cross-compile.ss".

;; Communication uses the Chez Scheme printer and reader so make the
;; server independent from Racket, although it is run by the Racket
;; executable.

;; Suppress printout on startup:
(define original-output-port (current-output-port))
(let-values ([(o get) (open-bytevector-output-port (current-transcoder))])
  (current-output-port o))

;; Server function to run after cross-compiler is loaded:
(define (serve-cross-compile target)
  ;; Don't exit due to Ctl-C:
  (keyboard-interrupt-handler void)
  ;; Restore output
  (current-output-port original-output-port)
  ;; Racket compilation mode
  (generate-inspector-information #f)
  (enable-arithmetic-left-associative #t)
  (generate-procedure-source-information #t)
  (expand-omit-library-invocations #t)
  ;; Set up the environment
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
  ;; Serve requests to compile or to fasl data:
  (let ([in (standard-input-port)]
        [out (standard-output-port)])
    (let loop ()
      (let ([cmd (get-u8 in)])
        (unless (eof-object? cmd)
          (get-u8 in) ; newline
          (let-values ([(o get) (open-bytevector-output-port)])
            (case (integer->char cmd)
              [(#\c)
               (compile-to-port (list `(lambda () ,(read-fasled in))) o)]
              [(#\f)
               ;; Reads host fasl format, then writes target fasl format
               (let ([v (read-fasled in)])
                 (parameterize ([#%$target-machine (string->symbol target)])
                   (fasl-write v o)))]
              [else
               (error 'serve-cross-compile (format "unrecognized command: ~s" cmd))])
            (let ([result (get)]
                  [len-bv (make-bytevector 8)])
              (bytevector-u64-set! len-bv 0 (bytevector-length result) (endianness little))
              (put-bytevector out len-bv)
              (put-bytevector out result)
              (flush-output-port out)))
          (loop))))))

;; ----------------------------------------

(define (read-fasled in)
  (let ([len-bv (get-bytevector-n in 8)])
    (fasl-read (open-bytevector-input-port
                (get-bytevector-n in (bytevector-u64-ref len-bv 0 (endianness little)))))))
