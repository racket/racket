#lang racket/base

(require racket/private/port)

(provide if-stream-out
         if-stream-in
         streamify-in
         streamify-out
         pump-ports)

(define (if-stream-out who p [sym-ok? #f])
  (cond [(and sym-ok? (eq? p 'stdout)) p]
        [(or (not p) (and (output-port? p) (file-stream-port? p))) p]
        [(output-port? p) #f]
        [else (raise-type-error who
                                (if sym-ok?
                                    "output port, #f, or 'stdout" 
                                    "output port or #f")
                                p)]))

(define (if-stream-in who p)
  (cond [(or (not p) (and (input-port? p) (file-stream-port? p))) p]
        [(input-port? p) #f]
        [else (raise-type-error who "input port or #f" p)]))

(define (streamify-in cin in ready-for-break)
  (if (and cin (not (file-stream-port? cin)))
    (thread (lambda ()
              (dynamic-wind
                void
                (lambda ()
                  (with-handlers ([exn:break? void])
                    (ready-for-break #t)
                    (copy-port cin in)
                    (ready-for-break #f)))
                (lambda () (close-output-port in)))
              (ready-for-break #t)))
    in))

(define (streamify-out cout out)
  (if (and cout 
           (not (eq? cout 'stdout))
           (not (file-stream-port? cout)))
      (thread (lambda ()
                (dynamic-wind
                    void
                    (lambda () (copy-port out cout))
                    (lambda () (close-input-port out)))))
      out))

(define (pump-ports evt pin pout perr in out err)
  (define who 'pump-ports)
  (define it-ready (make-semaphore))
  (define inpump  (streamify-in in 
                                (if-stream-out who pin) 
                                (lambda (ok?)
                                  (if ok?
                                      (semaphore-post it-ready)
                                      (semaphore-wait it-ready)))))
  (define outpump (streamify-out out (if-stream-in who pout)))
  (define errpump (streamify-out err (if-stream-in who perr)))
  (when (thread? inpump)
    ;; Wait for place to end, then stop copying input:
    (thread (lambda ()
              (sync evt inpump)
              (semaphore-wait it-ready)
              (break-thread inpump))))

  (values inpump outpump errpump))
