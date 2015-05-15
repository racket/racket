#lang racket/base
(require setup/dirs
         racket/file
         racket/path
         racket/match
         racket/format
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

(define (read-pkg-cfg/def k)
  ;; Lock is held for the current scope, but if
  ;; the key is not found in the current scope,
  ;; get the next scope's lock and try there,
  ;; etc.
  (define (get-default)
    (match k
      ['catalogs
       (list "http://pkgs.racket-lang.org"
             "http://planet-compats.racket-lang.org")]
      ['default-scope "user"]
      ['installation-name (version)]
      ['download-cache-dir (build-path (find-system-path 'addon-dir)
                                       "download-cache")]
      ['download-cache-max-files 1024]
      ['download-cache-max-bytes (* 64 1024 1024)]
      ['trash-max-packages 512]
      ['trash-max-seconds (* 60 60 24 2)] ; 2 days
      [_ #f]))
  (define c (read-pkg-file-hash (pkg-config-file)))
  (define v (hash-ref c k 'none))
  (cond
   [(eq? v 'none)
    ;; Default from enclosing scope or hard-wired default:
    (define s (current-pkg-scope))
    (if (eq? s 'installation)
        ;; Hard-wided:
        (get-default)
        ;; Enclosing:
        (parameterize ([current-pkg-scope 'installation])
          (read-pkg-cfg/def k)))]
   [else
    (match k
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
      [_ v])]))

(define (update-pkg-cfg! key val)
  (define f (pkg-config-file))
  (write-file-hash! 
   f
   (hash-set (read-pkg-file-hash f) key val)))

(define (default-pkg-scope)
  (match (default-pkg-scope-as-string)
    ["installation" 'installation]
    [else 'user]))
(define (default-pkg-scope-as-string)
  (read-pkg-cfg/def 'default-scope))

(define (pkg-config-catalogs)
  (with-pkg-lock/read-only
   (read-pkg-cfg/def 'catalogs)))

(define (pkg-catalogs)
  (or (current-pkg-catalogs)
      (map string->url (read-pkg-cfg/def 'catalogs))))

;; ----------------------------------------

(define (pkg-config config:set key+vals
                    #:from-command-line? [from-command-line? #f])
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
                       "trash-max-seconds")))
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
                        "trash-max-seconds"))
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
        (update-pkg-cfg! 'default-scope val)]
       [(list (and key "name") val)
        (unless (eq? 'installation (current-pkg-scope))
          (pkg-error (~a "setting `name' makes sense only in `installation' scope\n"
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
                           "trash-max-seconds"))
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
          (pkg-error (~a "setting `doc-open-url' works only in `installation' scope\n"
                         "  current package scope: ~a")
                     (current-pkg-scope)))
        (update-pkg-cfg! 'doc-open-url (if (equal? val "") #f val))]
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
             (printf "~a~a\n" indent (read-pkg-cfg/def 'default-scope))]
            ["name"
             (printf "~a~a\n" indent (read-pkg-cfg/def 'installation-name))]
            [(or "download-cache-dir"
                 "download-cache-max-files"
                 "download-cache-max-bytes"
                 "trash-max-packages"
                 "trash-max-seconds")
             (printf "~a~a\n" indent (read-pkg-cfg/def (string->symbol key)))]
            ["doc-open-url"
             (printf "~a~a\n" indent (or (read-pkg-cfg/def 'doc-open-url) ""))]
            [_
             (pkg-error "unsupported config key\n  key: ~e" key)])]
         [(list)
          (pkg-error "config key not provided")]
         [_
          (pkg-error (~a "multiple config keys provided"
                         (if from-command-line?
                             ";\n supply `--set' to set a config key's value"
                             "")))]))
     (match key+vals
       [(list)
        (for ([key (in-list '("name"
                              "catalogs"
                              "default-scope"
                              "download-cache-dir"
                              "download-cache-max-files"
                              "download-cache-max-bytes"
                              "trash-max-packages"
                              "trash-max-seconds"))])
          (printf "~a:\n" key)
          (show (list key) "  "))]
       [_ (show key+vals "")])]))
