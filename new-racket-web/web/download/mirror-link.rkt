#lang racket/base

#|

This file polls mirror links: (mirror-link <url> <size>) will return the
url only if it is live, and its size fits the expected size.  Otherwise
it returns #f, or #t in some rare cases where the size is not known from
the poll.

This is done only if the "KNOWN_MIRRORS_FILE" environment variable is
set, otherwise all mirror links are included.  If the variable is set,
it should point at a cache file that holds information about polls, used
to avoid re-polling all links all the time.

Polling a URL can result in one of four options:
1. The URL doesn't seem to exist.  In this case, the link is not shown,
   and the URL will be re-polled if a web-page build is done 15
   minutes (or more) later.
2. The URL exists, but no size information is available (via a HEAD
   query, or via an FTP directory listing).  The link will be shown in
   this case, but it will be re-polled two days later.  (With a 1..2
   random factor, and a nightly build that happens at the same time,
   this will be around 3-5 days.)  So far, all mirrors provide size
   information, so this works fine.
3. The URL exists and we get its size, but the size does not match.  The
   URL is not shown, and will be re-polled in an hour.  The assumption
   here is either bad synchronization, or we caught it in progress.
4. The size is correct, so the URL is shown.  This case still leads to
   re-polling, but only after a month.  The reason for this is in case a
   mirror is not maintained -- we'll want the links to eventually
   disappear.

|#

(require net/url net/ftp net/sendmail)

;; the mirrors file has (list url secs result) entries containing the url as a
;; string, the time it was checked (the result of `current-seconds'), and the
;; result of the `verify-*' function.
(define known-mirrors-file
  (let ([f (getenv "KNOWN_MIRRORS_FILE")]) (and (not (equal? "" f)) f)))
(define known-mirrors
  (if (and known-mirrors-file (file-exists? known-mirrors-file))
    (call-with-input-file* known-mirrors-file
      (λ (inp) (for/list ([x (in-producer read eof inp)]) x)))
    '()))

;; main entry to getting a known entry result: given the url, return the
;; remembered result unless it doesn't exist or it expired; in those cases use
;; the thunk to get a current result and remember it; note that expiration
;; times are different for different results, and the decision whether to check
;; one is randomized (cheaply, since it'll be sensitive to how frequently a
;; build is done -- usually by the nightly build).
(define (known-mirror-get url size thunk get-responsible-email)
  (define entry     (assoc url known-mirrors))
  (define last-time (and entry (cadr entry)))
  (define result    (and entry (caddr entry)))
  (define R (+ 1 (random))) ; random 1..2 number, to avoid congestion
  (define new
    (and (cond
           ;; failed, check again after 15 minutes (to accomodate re-runs after
           ;; a release was done)
           [(eq? #f result)
            (or (not entry) ; actually missing => try now
                (current-time . > . (+ last-time (* 15 60 R))))]
           ;; known but without a size to verify, check again after two days
           [(eq? #t result)
            (current-time . > . (+ last-time (* 2 24 60 60 R)))]
           ;; has a bad size, check again after an hour
           [(not (= result size))
            (current-time . > . (+ last-time (* 60 60 R)))]
           ;; otherwise check again after a month
           [else (current-time . > . (+ last-time (* 30 24 60 60 R)))])
         (list url current-time (thunk))))
  (when new
    ;; keep them sorted by time
    (set! known-mirrors
          `(,@(if entry (remq entry known-mirrors) known-mirrors) ,new))
    (call-with-output-file* known-mirrors-file #:exists 'truncate
      (λ (outp) (for ([entry (in-list known-mirrors)])
                  (fprintf outp "~s\n" entry)))))
  (when (and new                              ; we computed a new value
             (equal? result size)             ; we had a good result
             (not (equal? (caddr new) size))) ; but now it's bad
    ;; this means that a good mirror just went bad => nag someone
    (send-mail-message
     "eli@barzilay.org" "*** Mirror Link Down ***"
     ;; FIXME: if these messages are useful, change it to nag the mirror owner
     ;; instead of a fixed email -- use (list (get-responsible-email))
     '("eli@barzilay.org") '() '()
     `("A mirror link that used to be fine is now broken:"
       ,(format "  ~a" url)
       ,(format "The expected size is ~a, we now have ~a" size (caddr new))
       ""
       "This mirror will not be listed for now, until a re-poll finds it"
       "working.")))
  (if new (caddr new) result))

;; use the time when the file is loaded (no point for a finer granularity)
(define current-time (current-seconds))

(provide mirror-link)
(define (mirror-link url size get-responsible-email)
  (and (or (not known-mirrors-file) ; no file => don't check, just use all
           (let ([r (known-mirror-get url size (λ () (validate url size))
                                      get-responsible-email)])
             (or (eq? r #t) (equal? r size))))
       url))

(define (validate url size)
  (eprintf "  checking ~a\n" url)
  ((case (string->symbol (cadr (or (regexp-match #rx"^([^:]*):" url)
                                   (error 'mirror-link "bad url: ~a" url))))
     [(http https) verify-http]
     [(ftp) verify-ftp]
     [else (error 'mirror-link "unrecognizable url scheme: ~a\n" url)])
   url))

;; Verifiers return #f for failures, #t for dumb success (= didn't get size),
;; and a number for success with the remote file's size.
;;
;; Note: if `net/url' gets to deal with `ftp://' URLs, then only a
;; single verification function will be needed.  But for that it will
;; need to mimic HEAD requests too.

(define-syntax-rule (with-handlers+timeout name body ...)
  (let ([cust (make-custodian)] [ch (make-channel)])
    (parameterize ([current-custodian cust])
      (thread (λ () (channel-put ch (with-handlers ([exn:fail? exn-message])
                                      body ...)))))
    (begin0 (or (sync/timeout 20 ch) (format "~a connection timeout" name))
      (custodian-shutdown-all cust))))

(define (verify-http url)
  (define (check-contents inp)
    (define status (read-line inp))
    (define status* (regexp-match #rx"^HTTP/[0-9.]+ ([0-9]+)" status))
    (cond [(not status*)
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
               (begin (eprintf "WARNING: no `content-length' for ~a\n" url)
                      #t))]))
  (define r
    (with-handlers+timeout 'http
      (call/input-url (string->url url) head-impure-port check-contents)))
  (if (string? r)
    (begin (eprintf "WARNING: failure getting http info for ~a (~a)\n" url r)
           #f)
    r))

(define (verify-ftp url)
  (define-values [host port? path]
    (apply values
           (cdr (or (regexp-match #rx"^ftp://([^/:]+)(?::([0-9]+))?(/.*)$" url)
                    (error 'verify-ftp "bad ftp url: ~a" url)))))
  (define port (or port? 21))
  (define r
    (with-handlers+timeout 'ftp
      (define c (ftp-establish-connection host port "anonymous" "anonymous@"))
      (begin0 (ftp-directory-list c path) (ftp-close-connection c))))
  (cond [(not (and (list? r) (= 1 (length r)) (list? (car r))))
         (eprintf "WARNING: failure getting ftp info for ~a~a\n"
                  url (if (string? r) (format " (~a)" r) ""))
         #f]
        [(not (= 4 (length (car r))))
         (eprintf "WARNING: no size for: ~a\n" url)
         #t]
        [else (string->number (list-ref (car r) 3))]))
