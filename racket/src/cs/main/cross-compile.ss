;; The client half of this interaction is in "../linklet/cross-compile.ss".
;; Communication uses the Chez Scheme printer and reader so make the
;; server as independent from Racket as possible. We don't even need
;; this code to run as part of Racket CS, but it's convenient to
;; organize things that way.

(define (serve-cross-compile cross-compile-server-patch-file)
  (break-enabled #f) ; exit on EOF, but not on a break signal
  (unsafe-start-atomic)
  (call-with-system-wind
   (lambda ()
     (let-values ([(o get) (open-bytevector-output-port (current-transcoder))])
       (parameterize ([#%current-output-port o])
         ;; Loading the patch file disables normal `compile` and makes
         ;; `compile-to-port` compile to some other machine type:
         (#%load cross-compile-server-patch-file)))
     ;; Serve requests to compile or to fasl data:
     (let loop ()
       (let ([cmd (#%read)])
         (unless (eof-object? cmd)
           (let-values ([(o get) (open-bytevector-output-port)])
             (case cmd
               [(fasl)
                (fasl-write (unmarshal-annotation (#%read)) o)]
               [(compile)
                (compile-to-port (list `(lambda () ,(unmarshal-annotation (#%read)))) o)]
               [else
                (#%error 'serve-cross-compile (#%format "unrecognized command: ~s" cmd))])
             (let ([result (get)])
               (#%write result)
               (#%newline)
               (#%flush-output-port)))
           (loop))))))
  (exit))
