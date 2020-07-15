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
  ;; Set up the environment; ../expander/env.ss must be loaded before compiling
  (expand (let-syntax ([env (lambda (stx)
                              (datum->syntax stx (list 'quote environment-imports)))])
            env))
  ;; Serve requests to compile or to fasl data:
  (let ([in (standard-input-port)]
        [out (standard-output-port)])
    (let loop ()
      (let ([cmd (get-u8 in)])
        (unless (eof-object? cmd)
          (get-u8 in) ; newline
          (let-values ([(o get) (open-bytevector-output-port)])
            (let ([sfd-paths
                   (case (integer->char cmd)
                     [(#\c)
                      (call-with-fasled
                       in
                       (lambda (v pred)
                         (compile-to-port (list `(lambda () ,v)) o #f #f #f (string->symbol target) #f pred)))]
                     [(#\f)
                      ;; Reads host fasl format, then writes target fasl format
                      (call-with-fasled
                       in
                       (lambda (v pred)
                         (parameterize ([#%$target-machine (string->symbol target)])
                           (fasl-write v o pred))))]
                     [else
                      (error 'serve-cross-compile (format "unrecognized command: ~s" cmd))])])
              (let ([result (get)])
                (put-num out (bytevector-length result))
                (put-bytevector out result)
                (let ([len (vector-length sfd-paths)])
                  (put-num out len)
                  (let loop ([i 0])
                    (unless (fx= i len)
                      (put-num out (vector-ref sfd-paths i))
                      (loop (fx+ i 1)))))
                (flush-output-port out)))
            (loop)))))))

;; ----------------------------------------

(define (put-num out n)
  (let ([bv (make-bytevector 8)])
    (bytevector-u64-set! bv 0 n (endianness little))
    (put-bytevector out bv)))

(define (get-num in)
  (let ([bv (get-bytevector-n in 8)])
    (bytevector-u64-ref bv 0 (endianness little))))

;; ----------------------------------------

(define-record-type path-placeholder
  (fields pos))

(define (call-with-fasled in proc)
  (let* ([fasled-bv (get-bytevector-n in (get-num in))]
         [num-sfd-paths (get-num in)]
         [sfd-paths (list->vector
                      (let loop ([i 0])
                        (if (fx= i num-sfd-paths)
                            '()
                            (cons (make-path-placeholder i)
                                  (loop (fx+ i 1))))))]
         [used-placeholders '()]
         ;; v is the Chez Scheme value communicated from the client,
         ;; but with each path replace by a `path-placeholder`:
         [v (fasl-read (open-bytevector-input-port fasled-bv)
                       'load
                       sfd-paths)])
      (proc v
            (lambda (a)
              (and (path-placeholder? a)
                   (begin
                     (set! used-placeholders (cons a used-placeholders))
                     #t))))
      ;; Return indices of paths used in new fasled output, in the
      ;; order that they're used
      (list->vector (map path-placeholder-pos used-placeholders))))
