(module check mzscheme

  (define version-url "http://download.plt-scheme.org/version")
  (define timeout 30)

  (require (lib "url.ss" "net"))

  (define error-value
    (case-lambda
     [(what) `(error ,what)]
     [(what more)
      `(error ,what
              ,(cond [(list? more) (format "~a" more)]
                     [(exn? more)  (format "(~a)" (exn-message more))]
                     [else         (format "(~a)" more)]))]))

  (define (with-timeout timeout thunk)
    (define result #f)
    (let ([r (sync/timeout timeout
               (thread (lambda ()
                         (set! result
                               (with-handlers
                                   ([void (lambda (e)
                                            (error-value "internal error" e))])
                                 (thunk))))))])
      (if r result (error-value "timeout"))))

  (define (check-version/timeout)
    (let/ec escape
      (define (err . args) (escape (apply error-value args)))
      (define-syntax try
        (syntax-rules ()
          [(_ expr error-message)
           (with-handlers ([void (lambda (e) (err error-message e))]) expr)]))
      ;; Get server information, carefully
      (define version-info
        (parameterize ([current-input-port
                        (try (get-pure-port (string->url version-url))
                             "could not connect to website")])
          (try (read) "unexpected response from server")))
      (define (get key)
        (cond [(assq key version-info) => cadr]
              [else (err (format "no `~s' in response" key) version-info)]))
      (unless (and (list? version-info)
                   (andmap (lambda (x)
                             (and (list? x)
                                  (= 2 (length x))
                                  (symbol? (car x))
                                  (string? (cadr x))))
                           version-info))
        (err "bad response from server" version-info))
      ;; Make a decision
      (let ([current (version)]
            [stable (get 'stable)]
            [recent (get 'recent)])
        (cond
         ;; we have the newest version (can be > if we have an svn build)
         [(string>=? current recent) 'ok]
         ;; we're stable, but there's a newer version
         [(equal? current stable)
          `(ok-but ,recent)]
         ;; new version out -- no alphas or we have an alpha => show recent
         ;; (also for svn builds of a stable version -- anything with ".")
         [(or (equal? recent stable) (regexp-match #rx"[.]" current))
          `(newer ,recent)]
         ;; new version out, we have an outdated stable, there is also an alpha
         [else `(newer ,stable ,recent)]))))

  ;; Check the version on the server and compare to our version.
  ;; Possible return values (message is always a string):
  ;; * `ok
  ;;   You're fine.
  ;; * `(ok-but ,version)
  ;;   You have a fine stable version, but note that there is a newer alpha
  ;; * `(newer ,version)
  ;;   You have an old version, please upgrade to `version'
  ;; * `(newer ,version ,alpha)
  ;;   You have an old version, please upgrade to `version' you may consider
  ;;   also the alpha version
  ;; * `(error ,message [,additional-info])
  ;;   An error occured, the third (optional) value can be shown as the system
  ;;   error that happened or the value that caused an error.
  (provide check-version)
  (define (check-version)
    (with-timeout timeout check-version/timeout))

  )
