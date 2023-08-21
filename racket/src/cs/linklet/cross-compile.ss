;; The server half of this interaction is in "../c/cross-serve.ss".

;; Currently, cross-compilation support in Chez Scheme replaces the
;; compiler for the build machine. Until that changes, we can't load
;; cross-compilation support into the Chez Scheme instance that runs
;; Racket. Instead, launch a separate process and commuincate with
;; it via stdin/stdout.

;; To manage the possibility of multipe Racket threads and places that
;; cross-compile at the same time, we create a separate cross-compiler
;; process for each request --- but cache processes so that requests
;; can complete quickly in common cases. These separate processes can
;; just be forgotten if the compilation request is abandoned, so we
;; put each compiler in a thread (managed by the root custodian) that
;; can be cleaned up.

;; List of (list <machine-sym> <path-path>)
(define cross-machine-types '())

(define (add-cross-compiler! x-machine-type x-path exe-path)
  (set! cross-machine-types
        (cons (list x-machine-type (cons exe-path x-path))
              cross-machine-types)))

;; List of (list <machine-sym> <msg-channel>)
;; representing started compiler processes.
(define cross-machine-compiler-cache (unsafe-make-place-local '()))

;; To clean up abandonded compilers:
(define compiler-will-executor (unsafe-make-place-local #f))

;; Find compiler, starting one if necessary
(define (find-cross who machine)
  (disable-interrupts)
  (let* ([cache (unsafe-place-local-ref cross-machine-compiler-cache)]
         [a (#%assq machine cache)])
    (cond
     [a
      (unsafe-place-local-set! cross-machine-compiler-cache (#%remq a cache))
      (enable-interrupts)
      a]
     [else
      (enable-interrupts)
      (let ([a (#%assq machine cross-machine-types)])
        (cond
         [a (start-cross-compiler machine (cadr a))]
         [else
          (#%error who "no compiler loaded for ~a" machine)]))])))

(define (cache-cross-compiler a)
  (with-interrupts-disabled
   (unsafe-place-local-set! cross-machine-compiler-cache
                            (cons a (unsafe-place-local-ref cross-machine-compiler-cache)))))

(define (do-cross cmd machine v quoteds)
  (let* ([a (find-cross 'cross-compile machine)]
         [ch (cadr a)]
         [reply-ch (make-channel)])
    (channel-put ch (list cmd
                          v
                          quoteds
                          reply-ch))
    (let ([bv+literals (channel-get reply-ch)])
      (cache-cross-compiler a)
      (values (car bv+literals) (cdr bv+literals)))))

(define (cross-compile machine v quoteds realm unsafe?)
  (do-cross (if unsafe? 'u 'c) machine (cons v realm) quoteds))

(define (cross-fasl-to-string machine v quoteds mode)
  (do-cross (if (eq? mode 'code) 'f 'd) machine v quoteds))

;; Start a compiler as a Racket thread under the root custodian.
;; Using Racket's scheduler lets us use the event and I/O system,
;; including support for running a process and managing resources
;; through a custodian. Putting each cross-compiler instance in
;; its own thread more gracefully handles the case that a compilation
;; request is abandoned by the caller.
(define (start-cross-compiler machine exe+x)
  (let ([we (with-interrupts-disabled
             (or (unsafe-place-local-ref compiler-will-executor)
                 (let ([we (make-will-executor)])
                   (unsafe-place-local-set! compiler-will-executor we)
                   we)))])
    (let clean-up ()
      (when (will-try-execute we)
        (clean-up)))
    (let ([exe (find-exe (car exe+x))]
          [xpatch-dir (cdr exe+x)]
          [msg-ch (make-channel)]
          [c (unsafe-make-custodian-at-root)])
      (with-continuation-mark
          parameterization-key
          (extend-parameterization (continuation-mark-set-first
                                    #f
                                    parameterization-key)
                                   current-custodian
                                   c)
        ;; At this point, we're under the root custodian
        (thread
         (lambda ()
           (define (patchfile base)
             (build-path xpatch-dir (string-append base "-xpatch." (symbol->string machine))))
           (let-values ([(subproc from to err)
                         (subprocess #f #f (get-original-error-port)
                                     exe
                                     "--cross-server"
                                     (symbol->string machine)
                                     (patchfile "compile")
                                     (patchfile "library"))])
             ;; If this compiler instance becomes unreachable because the
             ;; called is interrupted, then shut this compiler down:
             (will-register we msg-ch (lambda (msg-ch) (custodian-shutdown-all c)))
             (let loop ()
               (let* ([msg (channel-get msg-ch)]
                      [compress? (case (car msg)
                                   [(d) compress-data?]
                                   [else compress-code?])])
                 ;; msg is (list <command> <value> <quoted> <reply-channel>)
                 (write-string (#%format "~a~a\n" (car msg) (if compress? #\y #\n)) to)
                 (let-values ([(bv literals) (fasl-to-bytevector (cadr msg) (caddr msg))])
                   ;; We can't send all literals to the cross compiler, but we can send
                   ;; strings and byte stringa, which might affect compilation. Otherwise,
                   ;; we report the existence of other literals, and the cross compiler can
                   ;; report which of those remain used in the compiled form.
                   (let-values ([(literals-bv ignored) (fasl-to-bytevector (strip-opaque literals) #f)])
                     (write-bytes (integer->integer-bytes (bytevector-length bv) 8 #f #f) to)
                     (write-bytes bv to)
                     (write-bytes (integer->integer-bytes (bytevector-length literals-bv) 8 #f #f) to)
                     (write-bytes literals-bv to)
                     (flush-output to))
                   (let* ([read-num (lambda ()
                                      (integer-bytes->integer (read-bytes 8 from) #f #f))]
                          [len (read-num)]
                          [bv (read-bytes len from)]
                          [kept-literals-count (read-num)] ; number of used-literal indices
                          [kept-literals (list->vector
                                          (let loop ([i 0])
                                            (if (fx= i kept-literals-count)
                                                '()
                                                (cons (vector-ref literals (read-num))
                                                      (loop (fx+ i 1))))))])
                     (channel-put (cadddr msg) (cons bv kept-literals))))
                 (loop)))))))
      (list machine msg-ch))))

(define (fasl-to-bytevector v quoteds)
  (let-values ([(o get) (open-bytevector-output-port)])
    (let ([literals (fasl-write/literals* v quoteds o)])
      (values (get) literals))))

(define (strip-opaque vec)
  (let ([vec2 (make-vector (vector-length vec) #f)])
    (let loop ([i 0])
      (unless (fx= i (vector-length vec))
        (let ([e (vector-ref vec i)])
          (when (or (string? e)
                    (bytevector? e))
            (vector-set! vec2 i e)))
        (loop (fx+ i 1))))
    vec2))

(define (find-exe exe)
  (let-values ([(base name dir?) (split-path exe)])
    (cond
     [(eq? base 'relative)
      (let loop ([paths (get-exe-search-path)])
        (cond
         [(null? paths) exe]
         [else
          (let ([f (build-path (car paths) exe)])
            (if (file-exists? f)
                f
                (loop (cdr paths))))]))]
     [else
      (path->complete-path exe (find-system-path 'orig-dir))])))

(define (get-exe-search-path)
  (define (accum->path one-accum)
    (bytes->path (u8-list->bytevector (reverse one-accum))))
  (let ([path (environment-variables-ref
               (|#%app| current-environment-variables)
               (string->utf8 "PATH"))])
    (cond
     [(not path) '()]
     [else
      (let loop ([bytes (bytevector->u8-list path)] [one-accum '()] [accum '()])
        (cond
         [(null? bytes) (let ([accum (if (null? one-accum)
                                         accum
                                         (cons (accum->path one-accum)
                                               accum))])
                          (reverse accum))]
         [(eqv? (car bytes) (if (eq? 'windows (system-type))
                                (char->integer #\;)
                                (char->integer #\:)))
          (if (null? one-accum)
              (loop (cdr bytes) '() accum)
              (loop (cdr bytes) '() (cons (accum->path one-accum) accum)))]
         [else
          (loop (cdr bytes) (cons (car bytes) one-accum) accum)]))])))
