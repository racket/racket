#lang racket/base

(require net/url
         net/url-connect
         "utils.rkt")

(module+ test) ;; see below

(define version-url "https://download.racket-lang.org/version.txt")
(define timeout 30)

(define-logger version/check)

(define (url->port url)
  (parameterize ([current-https-protocol 'secure])
    (get-pure-port (string->url url)
                   #:redirections 5)))

(define error-value
  (case-lambda
    [(what) `(error ,what)]
    [(what more)
     `(error ,what ,(cond [(list? more) (format "~a" more)]
                          [(exn? more)  (format "(~a)" (exn-message more))]
                          [else         (format "(~a)" more)]))]))

(define (with-timeout timeout thunk)
  (define cust (make-custodian))
  (define ch (make-channel))
  (parameterize ([current-custodian cust])
    (thread
     (λ ()
       (define result
         (with-handlers ([void (λ (e)
                                 (error-value "internal error" e))])
           (thunk)))
       (channel-put ch result))))
  (cond
    [(sync/timeout timeout ch)]
    [else
     (custodian-shutdown-all cust)
     (log-version/check-info "killed thread due to timeout")
     (error-value "timeout")]))

(define (check-version-raw)
  (let/ec escape
    (define (err . args) (escape (apply error-value args)))
    (define-syntax-rule (try expr error-message)
      (with-handlers ([void (λ (e) (err error-message e))]) expr))
    ;; Get server information, carefully
    (define version-info
      (parameterize ([current-input-port
                      (try (url->port (format "~a?~a" version-url (version)))
                           "could not connect to website")])
        (when (simulate-timeout-for-testing?)
          (log-version/check-warning
           "starting to sleep due to PLT_CHECK_VERSION_SIMULATE_TIMEOUT")
          (sleep (+ 5 timeout))
          (log-version/check-error
           "internal error: thread was not killed after simulated timeout"))
        (try (call-with-default-reading-parameterization read)
             "unexpected response from server")))
    (define (get key)
      (cond [(assq key version-info) => cadr]
            [else (err (format "no `~s' in response" key) version-info)]))
    (define (getver key)
      (define ver (get key))
      (if (valid-version? ver) ver (err "bad version string from server" key)))
    (unless (and (list? version-info)
                 (andmap (λ (x) (and (list? x)
                                     (= 2 (length x))
                                     (symbol? (car x))
                                     (string? (cadr x))))
                         version-info))
      (err "bad response from server" version-info))
    ;; Make a decision
    (define current (version))
    (define stable  (getver 'stable))
    (define recent  (getver 'recent))
    (cond
      ;; we have the newest version (can be > if we have a build from git)
      [(version<=? recent current) 'ok]
      ;; we're stable, but there's a newer version
      [(version<=? stable current) `(ok-but ,recent)]
      ;; new version out -- no alphas or we have an alpha => show recent
      [(or (equal? recent stable)
           (and (alpha-version? current)
                ;; but if we have an alpha that is older then the current
                ;; stable then go to the next case
                (version<=? stable current)))
       `(newer ,recent)]
      ;; new version out, we have an outdated stable, there is also an alpha
      ;; (alternatively, we have an alpha that is older than the current
      ;; stable)
      [else `(newer ,stable ,recent)])))

;; Check the version on the server and compare to our version.  Possible return
;; values (message is always a string):
;; * `ok
;;   You're fine.
;; * `(ok-but ,version)
;;   You have a fine stable version, but note that there is a newer alpha
;; * `(newer ,version)
;;   You have an old version, please upgrade to `version'
;; * `(newer ,version ,alpha)
;;   You have an old version, please upgrade to `version' you may consider also
;;   the alpha version
;; * `(error ,message [,additional-info])
;;   An error occurred, the third (optional) value can be shown as the system
;;   error that happened or the value that caused an error.
(provide check-version)
(define (check-version)
  (with-timeout timeout check-version-raw))

(define (simulate-timeout-for-testing?)
  (getenv "PLT_CHECK_VERSION_SIMULATE_TIMEOUT"))

(module+ test
  (let ([result (check-version)])
    (unless (eq? result 'ok)
      (error 'check-version "failed due to non-ok result: ~v" result)))
  (parameterize ([current-environment-variables
                  (make-environment-variables
                   #"PLT_CHECK_VERSION_SIMULATE_TIMEOUT" #"1")])
    (define receiver
      (make-log-receiver version/check-logger 'warning))
    (define result
      (check-version))
    (define warning-message
      (sync/timeout 0 receiver))
    (unless (equal? result '(error "timeout"))
      (raise-arguments-error 'check-version "failed to simulate timeout"
                             "result" result
                             "logged message" warning-message))
    (unless warning-message
      (error 'check-version "failed to log a message before simulating timeout"))
    (unless (and (vector? warning-message)
                 (eq? 'warning (vector-ref warning-message 0))
                 (eq? 'version/check (vector-ref warning-message 3)))
      (raise-arguments-error
       'check-version
       "unexpected log message while simulating timeout"
       "expected" (unquoted-printing-string
                   "topic 'version/check at level 'warning")
       "received" warning-message))
    (cond
      [(sync/timeout 7 receiver)
       => (λ (msg)
            (error 'check-version "~a;\n ~a\n  ~a: ~e"
                   "unexpected log message"
                   "may have failed to kill thread after simulated timeout"
                   "message" msg))])))
