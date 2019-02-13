#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "custom-port.rkt"
         "pipe.rkt"
         "peek-via-read-port.rkt"
         "count.rkt")

(provide make-input-port)

(define/who (make-input-port name
                             user-read-in
                             user-peek-in
                             user-close
                             [user-get-progress-evt #f]
                             [user-commit #f]
                             [user-get-location #f]
                             [user-count-lines! void]
                             [user-init-position 1]
                             [user-buffer-mode #f])
  (check who
         (lambda (p) (or (input-port? p) (and (procedure? p) (procedure-arity-includes? p 1))))
         #:contract "(or/c (procedure-arity-includes/c 1) input-port?)"
         user-read-in)
  (check who
         (lambda (p) (or (not p) (input-port? p) (and (procedure? p) (procedure-arity-includes? p 3))))
         #:contract "(or/c (procedure-arity-includes/c 3) input-port? #f)"
         user-peek-in)
  (check who (procedure-arity-includes/c 0) user-close)
  (check who (procedure-arity-includes/c 0) #:or-false user-get-progress-evt)
  (check who (procedure-arity-includes/c 3) #:or-false user-commit)
  (check who (procedure-arity-includes/c 0) #:or-false user-get-location)
  (check who (procedure-arity-includes/c 0) #:or-false user-count-lines!)
  (check-init-position who user-init-position)
  (check-buffer-mode who user-buffer-mode)

  (when (not (eqv? (input-port? user-read-in) (input-port? user-peek-in)))
    (raise-arguments-error who (if (input-port? user-read-in)
                                   "read argument is an input port, but peek argument is not a port"
                                   "read argument is not an input port, but peek argument is a port")
                          "read argument" user-read-in
                          "peek argument" user-peek-in))

  (when (and (not user-peek-in) user-get-progress-evt)
    (raise-arguments-error who "peek argument is #f, but progress-evt argument is not"
                           "progress-evt argument" user-get-progress-evt))

  (when (and (not user-get-progress-evt) user-commit)
    (raise-arguments-error who "progress-evt argument is #f, but commit argument is not"
                           "commit argument" user-commit))
  (when (and (not user-commit) user-get-progress-evt)
    (raise-arguments-error who "commit argument is #f, but progress-evt argument is not"
                           "progress-evt argument" user-get-progress-evt))
  
  (define input-pipe #f) ; `user-read-in` can redirect input
  
  (define (protect-in dest-bstr dest-start dest-end copy? user-read-in)
    ;; We don't trust `user-read-in` to refrain from modifying its
    ;; byte-string argument after it returns, and the `user-read-in`
    ;; interface doesn't deal with start and end positions, so copy`
    ;; dest-bstr` if needed
    (define len (- dest-end dest-start))
    (define user-bstr
      (if (or copy?
              (not (zero? dest-start))
              (not (= len dest-end)))
          (make-bytes len)
          dest-bstr))
    (define n (user-read-in user-bstr))
    (cond
      [(eq? user-bstr dest-bstr)
       n]
      [(evt? n)
       (wrap-evt n
                 (lambda (n)
                   (when (exact-positive-integer? n)
                     (bytes-copy! dest-bstr dest-start user-bstr 0 n))
                   n))]
      [else
       (when (exact-positive-integer? n)
         (bytes-copy! dest-bstr dest-start user-bstr 0 n))
       n]))

  ;; in atomic mode
  (define (check-read-result who r dest-start dest-end #:peek? [peek? #f] #:ok-false? [ok-false? #f])
    (cond
      [(exact-nonnegative-integer? r)
       (unless (r . <= . (- dest-end dest-start))
         (end-atomic)
         (raise-arguments-error who "result integer is larger than the supplied byte string"
                                "result" r
                                "byte-string length" (- dest-end dest-start)))]
      [(eof-object? r) (void)]
      [(and (procedure? r) (procedure-arity-includes? r 4))
       (unless user-peek-in
         (end-atomic)
         (raise-arguments-error who
                                (string-append "the port has no specific peek procedure, so"
                                               " a special read result is not allowed")
                                "special result" r))]
      [(pipe-input-port? r)
       (set! input-pipe r)]
      [(evt? r) r]
      [(and peek? (not r))
       (unless ok-false?
         (end-atomic)
         (raise-arguments-error who "returned #f when no progress evt was supplied"))]
      [else
       (end-atomic)
       (raise-result-error who
                           (string-append
                            "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port?"
                            (if (and peek? ok-false?)
                                " #f"
                                "")
                            (if user-peek-in
                                " (procedure-arity-includes/c 4)"
                                "")
                            ")")
                           r)]))

  ;; possibly in atomic mode
  (define (wrap-check-read-evt-result who evt dest-start dest-end peek? ok-false?)
    (wrap-evt evt (lambda (r)
                    (start-atomic)
                    (check-read-result who r dest-start dest-end #:peek? peek? #:ok-false? ok-false?)
                    (end-atomic)
                    (cond
                      [(pipe-input-port? r) 0]
                      [(evt? r)
                       (wrap-check-read-evt-result who r dest-start dest-end peek? ok-false?)]
                      [else r]))))

  ;; possibly in atomic mode
  (define (wrap-procedure-result r)
    (define called? #f)
    (define (called!)
      (when called?
        (raise-arguments-error 'read-special "cannot be called a second time"))
      (set! called? #t))
    (define (four-args a b c d)
      (called!)
      (check 'read-special exact-positive-integer? #:or-false b)
      (check 'read-special exact-nonnegative-integer? #:or-false c)
      (check 'read-special exact-positive-integer? #:or-false d)
      (r a b c d))
    (cond
      [(procedure-arity-includes? r 0)
       (case-lambda
         [() (called!) (r)]
         [(a b c d) (four-args a b c d)])]
      [else
       four-args]))

  ;; in atomic mode
  (define (read-in self dest-bstr dest-start dest-end copy?)
    (cond
      [input-pipe
       (cond
         [(zero? (pipe-content-length input-pipe))
          (set! input-pipe #f)
          (read-in self dest-bstr dest-start dest-end copy?)]
         [else
          (send core-input-port input-pipe read-in dest-bstr dest-start dest-end copy?)])]
      [else
       (define r
         (parameterize-break #f
           (non-atomically
            (protect-in dest-bstr dest-start dest-end copy? user-read-in))))
       (check-read-result '|user port read| r dest-start dest-end)
       (cond
         [(pipe-input-port? r)
          (read-in self dest-bstr dest-start dest-end copy?)]
         [(evt? r)
          (wrap-check-read-evt-result '|user port read| r dest-start dest-end #f #f)]
         [(procedure? r)
          (wrap-procedure-result r)]
         [else r])]))

  ;; in atomic mode
  ;; Used only if `user-peek-in` is a function:
  (define (peek-in self dest-bstr dest-start dest-end skip-k progress-evt copy?)
    (cond
      [input-pipe
       (cond
         [((pipe-content-length input-pipe) . <= . skip-k)
          (set! input-pipe #f)
          (peek-in self dest-bstr dest-start dest-end skip-k progress-evt copy?)]
         [else
          (send core-input-port input-pipe peek-in dest-bstr dest-start dest-end skip-k progress-evt copy?)])]
      [else
       (define r
         (parameterize-break #f
           (non-atomically
            (protect-in dest-bstr dest-start dest-end copy?
                        (lambda (user-bstr) (user-peek-in user-bstr skip-k progress-evt))))))
       (check-read-result '|user port peek| r dest-start dest-end #:peek? #t #:ok-false? progress-evt)
       (cond
         [(pipe-input-port? r)
          (peek-in self dest-bstr dest-start dest-end skip-k progress-evt copy?)]
         [(evt? r)
          (wrap-check-read-evt-result '|user port peek| r dest-start dest-end #t progress-evt)]
         [(procedure? r)
          (wrap-procedure-result r)]
         [else r])]))

  ;; in atomic mode
  ;; Used only if `user-peek-in` is a function:
  (define (byte-ready self work-done!)
    (cond
      [(and input-pipe
            (positive? (pipe-content-length input-pipe)))
       #t]
      [else
       (define bstr (make-bytes 1))
       (define v (peek-in self bstr 0 1 0 #f #f))
       (work-done!)
       (cond
         [(evt? v) v]
         [else (not (eqv? v 0))])]))

  ;; in atomic mode
  (define (close self)
    (end-atomic)
    (user-close)
    (start-atomic))

  (define (get-progress-evt self)
    (define r (user-get-progress-evt))
    (unless (evt? r)
      (raise-result-error '|user port progress-evt| "evt?" r))
    r)

  ;; in atomic mode
  (define (commit self amt evt ext-evt finish)
    (define r
      (parameterize-break #f
        (non-atomically
         (user-commit amt evt ext-evt))))
    (cond
      [(not r) #f]
      [(bytes? r) (finish r) #t]
      [else (finish (make-bytes amt (char->integer #\x))) #t]))

  (define get-location
    (and user-get-location
         (make-get-location user-get-location)))

  (define count-lines!
    (and user-count-lines!
         (lambda (self) (end-atomic) (user-count-lines!) (start-atomic))))

  (define-values (init-offset file-position)
    (make-init-offset+file-position user-init-position))

  (define buffer-mode
    (and user-buffer-mode
         (make-buffer-mode user-buffer-mode)))

  (finish-port/count
   (cond
     [user-peek-in
      (new core-input-port
           #:field
           [name name]
           [offset init-offset]
           #:override
           [read-in (if (input-port? user-read-in)
                        user-read-in
                        read-in)]
           [peek-in (if (input-port? user-peek-in)
                        user-peek-in
                        peek-in)]
           [byte-ready (if (input-port? user-peek-in)
                           user-peek-in
                           byte-ready)]
           [close close]
           [get-progress-evt (and user-get-progress-evt get-progress-evt)]
           [commit (and user-commit commit)]
           [get-location get-location]
           [count-lines! count-lines!]
           [file-position file-position]
           [buffer-mode buffer-mode])]
     [else
      (new peek-via-read-input-port
           #:field
           [name name]
           [offset init-offset]
           #:override
           [read-in/inner read-in]
           [close (values
                   (lambda (self)
                     (close self)
                     (send peek-via-read-input-port self close-peek-buffer)))]
           [get-location  get-location]
           [count-lines! count-lines!]
           [file-position file-position]
           [buffer-mode (or buffer-mode
                            (case-lambda
                              [(self) (send peek-via-read-input-port self default-buffer-mode)]
                              [(self mode) (send peek-via-read-input-port self default-buffer-mode mode)]))])])))
