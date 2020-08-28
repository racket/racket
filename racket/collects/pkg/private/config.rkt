#lang racket/base
(require setup/dirs
         racket/file
         racket/path
         racket/match
         racket/format
         racket/string
         net/url
         "../path.rkt"
         "dirs.rkt"
         "params.rkt"
         "lock.rkt"
         "print.rkt")

;; Reading and writing the package-relevant configuration of
;; an installation or for the current user.

(provide (all-defined-out))

(define (get-download-cache-dir)
  (or (current-pkg-download-cache-dir)
      (read-pkg-cfg/def 'download-cache-dir)))
(define (get-download-cache-max-files)
  (or (current-pkg-download-cache-max-files)
      (read-pkg-cfg/def 'download-cache-max-files)))
(define (get-download-cache-max-bytes)
  (or (current-pkg-download-cache-max-bytes)
      (read-pkg-cfg/def 'download-cache-max-bytes)))

(define (get-trash-max-packages)
  (or (current-pkg-trash-max-packages)
      (read-pkg-cfg/def 'trash-max-packages)))
(define (get-trash-max-seconds)
  (or (current-pkg-trash-max-seconds)
      (read-pkg-cfg/def 'trash-max-seconds)))

(define (get-network-retries)
  (or (current-pkg-network-retries)
      (read-pkg-cfg/def 'network-retries)))

(define (get-git-checkout-credentials)
  (define format+creds
    (or (current-pkg-git-checkout-credentials)
        (read-pkg-cfg/def 'git-checkout-credentials)))
  (define format-spec (car format+creds))
  (unless (eq? format-spec 'basic)
    (pkg-error "only 'basic credentials are supported"))
  (cdr format+creds))

;; Must hold lock for current scope
(define (read-pkg-cfg/def key)
  (read-pkg-cfg/def/scope key (lambda (v scope) v)))

;; Must hold lock for current scope
(define (read-pkg-cfg-effective-scope key)
  (read-pkg-cfg/def/scope key (lambda (v scope) scope)))

;; `k` receives value and scope where it was found
(define (read-pkg-cfg/def/scope key k)
  ;; Lock is held for the current scope, but if
  ;; the key is not found in the current scope,
  ;; get the next scope's lock and try there,
  ;; etc., based on the rule that we can look
  ;; wider with a given lock
  (define (get-default)
    (match key
      ['catalogs
       (list "https://pkgs.racket-lang.org"
             "https://planet-compats.racket-lang.org")]
      ['default-scope "user"]
      ['installation-name (version)]
      ['download-cache-dir (build-path (find-system-path 'cache-dir)
                                       "download-cache")]
      ['download-cache-max-files 1024]
      ['download-cache-max-bytes (* 64 1024 1024)]
      ['trash-max-packages 512]
      ['trash-max-seconds (* 60 60 24 2)] ; 2 days
      ['network-retries 5]
      ['git-checkout-credentials '(basic)]
      [_ #f]))
  (define c (read-pkg-file-hash (pkg-config-file)))
  (define v (hash-ref c key 'none))
  (cond
   [(eq? v 'none)
    ;; Default from enclosing scope or hard-wired default:
    (define s (current-pkg-scope))
    (define all-scopes (get-all-pkg-scopes))
    ;; We want to search all scopes before the current one in `all-scopes`.
    ;; Instead of `reverse` plus `member`, this loop handles a path
    ;; appearing multiple times in the list (which is not a good configuration,
    ;; but this approach is better than potentially looping forever:
    (define check-scopes
      (let loop ([scopes all-scopes] [accum '()])
        (cond
          [(null? scopes) accum]
          [(equal? s (car scopes)) accum]
          [else (loop (cdr scopes) (cons (car scopes) accum))])))
    (cond
      [(null? check-scopes)
       ;; Use hard-wired default:
       (k (get-default) #f)]
      [else
       ;; Enclosing:
       (parameterize ([current-pkg-scope (car check-scopes)])
         (read-pkg-cfg/def/scope key k))])]
   [else
    (k (match key
         ['catalogs
          ;; Replace #f with default URLs, relative path
          ;; with absolute path:
          (apply append (for/list ([i (in-list v)])
                          (cond
                            [(not i) (get-default)]
                            [(regexp-match? #rx"^[a-z]+://" i)
                             (list i)]
                            [else
                             ;; If it doesn't look like a URL, then treat it as
                             ;; a path (potentially relative to the configuration file):
                             (list
                              (url->string
                               (path->url
                                (simple-form-path
                                 (path->complete-path i (path->complete-path (pkg-dir #t)))))))])))]
         [_ v])
       (current-pkg-scope))]))

(define (update-pkg-cfg! key val)
  (define f (pkg-config-file))
  (write-file-hash! 
   f
   (hash-set (read-pkg-file-hash f) key val)))

(define (default-pkg-scope)
  (match (default-pkg-scope-as-string)
    ["installation" 'installation]
    [_ 'user]))
(define (default-pkg-scope-as-string)
  (read-pkg-cfg/def 'default-scope))

(define (pkg-config-catalogs)
  (with-pkg-lock/read-only
   (read-pkg-cfg/def 'catalogs)))

(define (pkg-catalogs)
  (or (current-pkg-catalogs)
      (map string->url (read-pkg-cfg/def 'catalogs))))

(define (narrower-scope? scope than-scope)
  (define all-scopes (get-all-pkg-scopes))
  (and (member scope (cdr (or (member than-scope all-scopes)
                              '(#f))))
       #t))

;; Makes the most sense when `(current-pkg-scope)` is 'user,
;; and a read lock must be held for the current scope.
(define (pkg-config-default-scope-scope)
  (read-pkg-cfg-effective-scope 'default-scope))

;; ----------------------------------------
              
(define (pkg-config config:set key+vals
                    #:from-command-line? [from-command-line? #f]
                    #:default-scope-scope [default-scope-scope #f])
  (cond
    [config:set
     (match key+vals
       [(list)
        (pkg-error "no config key given")]
       [(list (and key
                   (or "default-scope"
                       "name"
                       "download-cache-max-files"
                       "download-cache-max-bytes"
                       "download-cache-dir"
                       "doc-open-url"
                       "trash-max-packages"
                       "trash-max-seconds"
                       "network-retries")))
        (pkg-error (~a "missing value for config key\n"
                       "  config key: ~a")
                   key)]
       [(list* (and key
                    (or "default-scope"
                        "name"
                        "download-cache-max-files"
                        "download-cache-max-bytes"
                        "download-cache-dir"
                        "doc-open-url"
                        "trash-max-packages"
                        "trash-max-seconds"
                        "network-retries"))
               val
               another-val
               more-vals)
        (pkg-error (~a "too many values provided for config key\n"
                       "  config key: ~a\n"
                       "  given values:~a")
                   key
                   (format-list (cons val more-vals)))]
       [(list* (and key "catalogs") val)
        (update-pkg-cfg! 'catalogs
                         (for/list ([s (in-list val)])
                           (cond
                            [(equal? s "") #f]
                            [else s])))]
       [(list (and key "default-scope") val)
        (unless (member val '("installation" "user"))
          (pkg-error (~a "invalid value for config key\n"
                         "  config key: ~a\n"
                         "  given value: ~a\n"
                         "  valid values: installation, user")
                     key
                     val))
        (update-pkg-cfg! 'default-scope val)
        (when (and default-scope-scope
                   (narrower-scope? default-scope-scope (current-pkg-scope)))
          (printf " Note: setting `default-scope` in ~a scope is not effective,\n" (current-pkg-scope))
          (printf " because `default-scope` is configured in a narrower scope\n"))]
       [(list (and key "name") val)
        (unless (eq? 'installation (current-pkg-scope))
          (pkg-error (~a "setting `name` makes sense only ininstallation scope\n"
                         "  current package scope: ~a")
                     (current-pkg-scope)))
        (update-pkg-cfg! 'installation-name val)]
       [(list (and key "download-cache-dir")
              val)
        (update-pkg-cfg! (string->symbol key) (if (complete-path? val)
                                                  val
                                                  (path->string
                                                   (path->complete-path val))))]
       [(list (and key (or "download-cache-max-files"
                           "download-cache-max-bytes"
                           "trash-max-packages"
                           "trash-max-seconds"
                           "network-retries"))
              val)
        (unless (real? (string->number val))
          (pkg-error (~a "invalid value for config key\n"
                         "  config key: ~a\n"
                         "  given value: ~a\n"
                         "  valid values: real numbers")
                     key
                     val))
        (update-pkg-cfg! (string->symbol key) (string->number val))]
       [(list (and key "doc-open-url") val)
        (unless (eq? 'installation (current-pkg-scope))
          (pkg-error (~a "setting `doc-open-url` works only in `installation` scope\n"
                         "  current package scope: ~a")
                     (current-pkg-scope)))
        (update-pkg-cfg! 'doc-open-url (if (equal? val "") #f val))]
       [(list* "git-checkout-credentials" vals)
        (define (credentials-format-error msg val)
          (pkg-error (~a msg "\n"
                         "  given: ~a\n"
                         "  expected: value in the form <username>:<password>")
                     val))
        (update-pkg-cfg! 'git-checkout-credentials
                         (cons 'basic
                               (for/list ([val (in-list vals)])
                                 (match (string-split val ":" #:trim? #f)
                                   [(list "" _)
                                    (credentials-format-error
                                     "invalid empty username in git checkout credentials"
                                     val)]
                                   [(list _ "")
                                    (credentials-format-error
                                     "invalid empty password in git checkout credentials"
                                     val)]
                                   [(list username password)
                                    `#hasheq((username . ,username)
                                             (password . ,password))]
                                   [(list* _ _ _)
                                    (credentials-format-error
                                     "too many elements for git checkout credentials"
                                     val)]
                                   [(list _)
                                    (credentials-format-error
                                     "not enough elements for git checkout credentials"
                                     val)]))))
        (displayln "WARNING: checkout credentials are stored UNENCRYPTED" (current-error-port))]
       [(list* key args)
        (pkg-error "unsupported config key\n  key: ~a" key)])]
    [else
     (define (show key+vals indent)
       (match key+vals
         [(list key)
          (match key
            ["catalogs"
             (for ([s (in-list (read-pkg-cfg/def 'catalogs))])
               (printf "~a~a\n" indent s))]
            ["default-scope"
             (define val (read-pkg-cfg/def 'default-scope))
             (printf "~a~a\n" indent val)
             (when (and default-scope-scope
                        (narrower-scope? default-scope-scope (current-pkg-scope)))
               (printf "~a Note: `default-scope` in ~a scope is not effective,\n" indent (current-pkg-scope))
               (printf "~a because `default-scope` is configured in a narrower scope\n" indent))]
            ["name"
             (printf "~a~a\n" indent (read-pkg-cfg/def 'installation-name))]
            [(or "download-cache-dir"
                 "download-cache-max-files"
                 "download-cache-max-bytes"
                 "trash-max-packages"
                 "trash-max-seconds"
                 "network-retries")
             (printf "~a~a\n" indent (read-pkg-cfg/def (string->symbol key)))]
            ["doc-open-url"
             (printf "~a~a\n" indent (or (read-pkg-cfg/def 'doc-open-url) ""))]
            ["git-checkout-credentials"
             (for ([creds (in-list (cdr (read-pkg-cfg/def 'git-checkout-credentials)))])
               (printf "~a~a:~a\n" indent (hash-ref creds 'username) (hash-ref creds 'password)))]
            [_
             (pkg-error "unsupported config key\n  key: ~e" key)])]
         [(list)
          (pkg-error "config key not provided")]
         [_
          (pkg-error (~a "multiple config keys provided"
                         (if from-command-line?
                             ";\n supply `--set` to set a config key's value"
                             "")))]))
     (match key+vals
       [(list)
        (for ([key (in-list '("name"
                              "catalogs"
                              "default-scope"
                              "download-cache-dir"
                              "download-cache-max-files"
                              "download-cache-max-bytes"
                              "git-checkout-credentials"
                              "trash-max-packages"
                              "trash-max-seconds"
                              "network-retries"))])
          (printf "~a:\n" key)
          (show (list key) "  "))]
       [_ (show key+vals "")])]))
