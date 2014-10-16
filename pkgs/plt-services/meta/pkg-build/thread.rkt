#lang racket/base

(provide thread/chunk-output
         wait-chunk-output
         flush-chunk-output)

;; Run `thunk` in a thread, capturing output to deliver
;; in chunks.
(define (thread/chunk-output thunk)
  (define (make-port e?)
    (make-output-port (if e?
                          'stderr/chunked
                          'stdout/chunked)
                      always-evt
                      (lambda (bstr s e buffer? break?)
                        (thread-send manager (vector t (subbytes bstr s e) e?))
                        (- e s))
                      void))
  (define go (make-semaphore))
  (define t (parameterize ([current-error-port (make-port #t)]
                           [current-output-port (make-port #f)])
              (thread (lambda ()
                        (semaphore-wait go)
                        (thunk)))))
  (thread-send manager t)
  (semaphore-post go)
  t)

;; ----------------------------------------

(define no-threads-ch (make-channel))

(define manager
  (thread 
   (lambda ()
     (define (show-output t output)
       (define e (current-error-port))
       (define o (current-output-port))
       (define es (hash-ref output t '()))
       (for ([i (in-list (reverse es))])
         (write-bytes (cdr i) (if (car i) e o))))
     (let loop ([output (hash)])
       (define (do-message msg-evt)
         (define msg (thread-receive))
         (cond
          [(thread? msg) (loop (hash-set output msg null))]
          [(pair? msg)
           (define t (car msg))
           (define s (cdr msg))
           (cond
            [(hash-ref output t #f)
             (show-output t output)
             (semaphore-post s)
             (loop (hash-set output t null))]
            [else
             (semaphore-post s)
             (loop output)])]
          [else
           (define-values (t o e?) (vector->values msg))
           (loop (hash-set output t (cons (cons e? o)
                                          (hash-ref output t null))))]))
       (sync/timeout
        (lambda ()
          (apply
           sync
           (handle-evt (thread-receive-evt) do-message)
           (if (zero? (hash-count output))
               (handle-evt (channel-put-evt no-threads-ch (void))
                           (lambda (_)
                             (loop output)))
               never-evt)
           (map
            (lambda (t)
              (handle-evt 
               t
               (lambda (_)
                 (show-output t output)
                 (loop (hash-remove output t)))))
            (hash-keys output))))
        (handle-evt (thread-receive-evt) do-message))))))

(define (flush-chunk-output)
  (define s (make-semaphore))
  (thread-send manager (cons (current-thread) s))
  (semaphore-wait s))

(define (wait-chunk-output)
  (channel-get no-threads-ch))

;; --------------------------------------------------

(module test racket/base
  (define o (open-output-bytes))
  (parameterize ([current-output-port o]
                 [current-error-port o])
    (define-syntax-rule (def id)
      (define id
        (dynamic-require (module-path-index-join
                          `(submod "..")
                          (variable-reference->module-path-index
                           (#%variable-reference)))
                         'id)))
    (def thread/chunk-output)
    (def flush-chunk-output)
    (def wait-chunk-output)
    (define t1 (thread/chunk-output
                (lambda ()
                  (printf "hi\n")
                  (eprintf "bye\n")
                  (flush-chunk-output)
                  (sync (system-idle-evt))
                  (printf "HI\n")
                  (eprintf "BYE\n"))))
    (define t2 (thread/chunk-output
                (lambda ()
                  (printf "hola\n")
                  (eprintf "adios\n")
                  (flush-chunk-output)
                  (sync (system-idle-evt))
                  (printf "HOLA\n")
                  (eprintf "ADIOS\n"))))
    (wait-chunk-output))
  (let ([l '("hi\nbye" "hola\nadios")]
        [s (get-output-string o)]
        [sa (lambda (a b) (string-append (car a)
                                         "\n"
                                         (cadr a)
                                         "\n"
                                         (car b)
                                         "\n"
                                         (cadr b)
                                         "\n"))]
        [r reverse]
        [u (lambda (l) (map string-upcase l))])
    (unless (or (equal? s (sa l (u l)))
                (equal? s (sa (r l) (u l)))
                (equal? s (sa (r l) (u (r l))))
                (equal? s (sa l (u (r l)))))
      (error "mismatch: " s))))
