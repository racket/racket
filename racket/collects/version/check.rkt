#lang racket/base

(require net/url
         racket/tcp
         "utils.rkt")

(define version-url "http://download.racket-lang.org/version.txt")
(define timeout 30)

(define (url->port url)
  (get-pure-port (string->url url)))

(define error-value
  (case-lambda
    [(what) `(error ,what)]
    [(what more)
     `(error ,what ,(cond [(list? more) (format "~a" more)]
                          [(exn? more)  (format "(~a)" (exn-message more))]
                          [else         (format "(~a)" more)]))]))

(define (with-timeout timeout thunk)
  (define result #f)
  (define r (sync/timeout timeout
              (thread (λ ()
                        (set! result
                              (with-handlers
                                  ([void (λ (e)
                                           (error-value "internal error" e))])
                                (thunk)))))))
  (if r result (error-value "timeout")))

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
        (try (read) "unexpected response from server")))
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
;;   An error occured, the third (optional) value can be shown as the system
;;   error that happened or the value that caused an error.
(provide check-version)
(define (check-version)
  (with-timeout timeout check-version-raw))

(module+ test
  (let ([result (check-version)])
    (unless (eq? result 'ok)
      (error 'check-version "failed due to non-ok result: ~v" result))))
