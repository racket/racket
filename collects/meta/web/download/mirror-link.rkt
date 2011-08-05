#lang racket/base

(require net/url net/ftp)

;; the mirrors file has (list url secs result) entries containing the url as a
;; string, the time it was checked (the result of `current-seconds'), and the
;; result of the `verify-*' function.
(define known-mirrors-file
  (let ([f (getenv "KNOWN_MIRRORS_FILE")]) (and (not (equal? "" f)) f)))
(define known-mirrors
  (if (and known-mirrors-file (file-exists? known-mirrors-file))
    (call-with-input-file* known-mirrors-file
      (lambda (inp) (for/list ([x (in-producer read eof inp)]) x)))
    '()))

;; main entry to getting a known entry result: given the url, return the
;; remembered result unless it doesn't exist or it expired; in those cases use
;; the thunk to get a current result and remember it; note that expiration
;; times are different for different results, and the decision whether to check
;; one is randomized (cheaply, since it'll be sensitive to how frequently a
;; build is done -- usually by the nightly build).
(define (known-mirror-get url size thunk)
  (define entry     (assoc url known-mirrors))
  (define last-time (and entry (cadr entry)))
  (define result    (and entry (caddr entry)))
  (define new
    (and (cond
           ;; failed, check again after 15 minutes (to accomodate re-runs after
           ;; a release was done)
           [(eq? #f result)
            (or (not entry) ; actually missing => try now
                (current-time . > . (+ last-time (* 15 60))))]
           ;; known but without a size to verify, check again after two days
           [(eq? #t result)
            (and (current-time . > . (+ last-time (* 2 24 60 60)))
                 (zero? (random 3)))]
           ;; has a bad size, check again after a day
           [(not (= result size))
            (and (current-time . > . (+ last-time (* 24 60 60)))
                 (zero? (random 3)))]
           ;; otherwise check again after a month
           [else (and (current-time . > . (+ last-time (* 30 24 60 60)))
                      (zero? (random 20)))])
         (list url current-time (thunk))))
  (when new
    ;; keep them sorted by time
    (set! known-mirrors
          `(,@(if entry (remq entry known-mirrors) known-mirrors) ,new))
    (call-with-output-file* known-mirrors-file #:exists 'truncate
      (lambda (outp)
        (for ([entry (in-list known-mirrors)])
          (fprintf outp "~s\n" entry)))))
  (if new (caddr new) result))

;; use the time when the file is loaded (no point for a finer granularity)
(define current-time (current-seconds))

(provide mirror-link)
(define (mirror-link url size)
  (and (or (not known-mirrors-file) ; no file => don't check, just use all
           (let ([r (known-mirror-get
                     url size (lambda () (validate url size)))])
             (or (eq? r #t) (equal? r size))))
       url))

(define (validate url size)
  (eprintf "  checking ~a\n" url)
  (define scheme
    (string->symbol (cadr (or (regexp-match #rx"^([^:]*):" url)
                              (error 'mirror-link "bad url: ~a" url)))))
  ((case scheme
     [(http https) verify-http]
     [(ftp) verify-ftp]
     [else (error 'mirror-link "unrecognizable url scheme: ~a\n" url)])
   url))

;; verifiers return #f for failures, #t for dumb success (= didn't get size),
;; and a number for success with the remote file's size

(define (verify-http url)
  (call/input-url (string->url url) head-impure-port
    (lambda (inp)
      (define status (read-line inp))
      (define status* (regexp-match #rx"^HTTP/[0-9.]+ ([0-9]+)" status))
      (cond
        [(not status*)
         (eprintf "WARNING: bad status line for ~a:\n  ~s\n" url status)
         #f]
        [(not (regexp-match #rx"^2..$" (cadr status*)))
         (eprintf "WARNING: bad status code for ~a: ~s\n" url (cadr status*))
         #f]
        [else
         (or (for/or ([line (in-lines inp)])
               (cond [(regexp-match #rx"^(?i:content-length: *([0-9]+) *)$"
                                    line)
                      => (compose string->number cadr)]
                     [else #f]))
             (begin (eprintf "WARNING: no `content-length' for ~a" url)
                    #t))]))))

(define (verify-ftp url)
  (define-values [host port? path]
    ;; FIXME
    (apply values
           (cdr (or (regexp-match #rx"^ftp://([^/:]+)(?::([0-9]+))?(/.*)$" url)
                    (error 'verify-ftp "bad ftp url: ~a" url)))))
  (define port (or port? 21))
  (define r
    (let ([c (ftp-establish-connection host port "anonymous" "anonymous@")])
      (begin0 (ftp-directory-list c path) (ftp-close-connection c))))
  (cond [(not (and (list? r) (= 1 (length r)) (list? (car r))))
         (eprintf "WARNING: failure getting ftp info for ~a\n" url)
         #f]
        [(not (= 4 (length (car r))))
         (eprintf "WARNING: no size for: ~a\n" url)
         #t]
        [else (string->number (list-ref (car r) 3))]))
