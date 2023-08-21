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
  (let ([omit-debugging? #t]
        [measure-performance? #f])
    (include "../linklet/config.ss"))
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
          (let ([compress? (eqv? (get-u8 in) (char->integer #\y))])
            (get-u8 in) ; newline
            (let-values ([(o get) (open-bytevector-output-port)])
              (let ([literals
                     (case (integer->char cmd)
                       [(#\c #\u)
                        (call-with-fasled
                         in
                         (lambda (v+realm pred)
                           (let ([v (car v+realm)]
                                 [realm (cdr v+realm)])
                             (parameterize ([optimize-level (if (fx= cmd (char->integer #\u))
                                                                3
                                                                (optimize-level))]
                                            [fasl-compressed compress?]
                                            [compile-procedure-realm realm])
                               (compile-to-port (list v) o #f #f #f (string->symbol target) #f pred 'omit-rtds)))))]
                       [(#\f #\d)
                        ;; Reads host fasl format, then writes target fasl format
                        (call-with-fasled
                         in
                         (lambda (v pred)
                           (parameterize ([#%$target-machine (string->symbol target)]
                                          [fasl-compressed compress?])
                             (fasl-write v o pred))))]
                       [else
                        (error 'serve-cross-compile (format "unrecognized command: ~s" cmd))])])
                (let ([result (get)])
                  (put-num out (bytevector-length result))
                  (put-bytevector out result)
                  (let ([len (vector-length literals)])
                    (put-num out len)
                    (let loop ([i 0])
                      (unless (fx= i len)
                        (put-num out (vector-ref literals i))
                        (loop (fx+ i 1)))))
                  (flush-output-port out)))
              (loop))))))))

;; ----------------------------------------

(define (put-num out n)
  (let ([bv (make-bytevector 8)])
    (bytevector-u64-set! bv 0 n (endianness little))
    (put-bytevector out bv)))

(define (get-num in)
  (let ([bv (get-bytevector-n in 8)])
    (bytevector-u64-ref bv 0 (endianness little))))

;; ----------------------------------------

(define-record-type literal-placeholder
  (fields pos))

(define (call-with-fasled in proc)
  (let* ([fasled-bv (get-bytevector-n in (get-num in))]
         [literals-bv (get-bytevector-n in (get-num in))]
         [transparent-placeholders (make-eq-hashtable)]
         [literals (let ([vec (fasl-read (open-bytevector-input-port literals-bv))])
                     ;; Use a placeholder for opaque literals that could not be
                     ;; communicated from the Racket world. "Transparent" literals
                     ;; are things like strings and bytevectors that can affect
                     ;; compilation, since code might be specialized to a string
                     ;; or bytevector literal.
                     (let loop ([i 0])
                       (if (fx= i (vector-length vec))
                           vec
                           (let ([e (vector-ref vec i)]
                                 [ph (make-literal-placeholder i)])
                             (cond
                               [(not e) (vector-set! vec i ph)]
                               [else (hashtable-set! transparent-placeholders e ph)])
                             (loop (fx+ i 1))))))]
         [used-placeholders '()]
         ;; v is the Chez Scheme value communicated from the client,
         ;; but with each opaque literal replaced by a `literal-placeholder`:
         [v (fasl-read (open-bytevector-input-port fasled-bv)
                       'load
                       literals)])
      (proc v
            (lambda (a)
              (let ([a (eq-hashtable-ref transparent-placeholders a a)])
                (and (literal-placeholder? a)
                     (begin
                       (set! used-placeholders (cons a used-placeholders))
                       #t)))))
      ;; Return indices of literals used in new fasled output in the order
      ;; that they're used.
      (list->vector (reverse (map literal-placeholder-pos used-placeholders)))))
