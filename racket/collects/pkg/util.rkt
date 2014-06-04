#lang racket/base
(require racket/path
         racket/list
         racket/function
         racket/file
         racket/port
         racket/match
         racket/format
         racket/string
         racket/set
         net/url
         json)

(define-logger pkg)

(define (make-parent-directory* p)
  (define parent (path-only p))
  (make-directory* parent))

(define (table-display l)
  (define how-many-cols (length (first l)))
  (define max-widths
    (for/list ([col (in-range how-many-cols)])
      (apply max (map (compose string-length (curryr list-ref col)) l))))
  (for ([row (in-list l)])
    (for ([col (in-list row)]
          [i (in-naturals 1)]
          [width (in-list max-widths)])
      (printf "~a~a"
              col
              (if (= i how-many-cols)
                ""
                (make-string (+ (- width (string-length col)) 4) #\space))))
    (printf "\n")))

(define (call/input-url+200 u fun
                            #:headers [headers '()]
                            #:failure [fail-k (lambda (s) #f)])
  #;(printf "\t\tReading ~a\n" (url->string u))
  (define-values (ip hs) (get-pure-port/headers u headers
                                                #:redirections 25
                                                #:status? #t))
  (if (string=? "200" (substring hs 9 12))
      (begin0
       (fun ip)
       (close-input-port ip))
      (fail-k hs)))

(define (url-path/no-slash url)
  (define p (url-path url))
  (define rp (reverse p))
  (reverse
   (match rp
     [(list* (path/param "" _) rest)
      rest]
     [_ rp])))

(define github-client_id (make-parameter #f))
(define github-client_secret (make-parameter #f))

(define (split-github-url pkg-url)
  (if (equal? (url-scheme pkg-url) "github")
      ;; github://
      (map path/param-path (url-path/no-slash pkg-url))
      ;; git://
      (let* ([paths (map path/param-path (url-path/no-slash pkg-url))])
        (list* (car paths)
               (regexp-replace* #rx"[.]git$" (cadr paths) "")
               (or (url-fragment pkg-url) "master")
               (let ([a (assoc 'path (url-query pkg-url))])
                 (or (and a (cdr a) (string-split (cdr a) "/"))
                     null))))))

(define (package-url->checksum pkg-url-str [query empty]
                               #:download-printf [download-printf void]
                               #:pkg-name [pkg-name "package"])
  (define pkg-url
    (string->url pkg-url-str))
  (match (url-scheme pkg-url)
    [(or "github" "git")
     (match-define (list* user repo branch path)
                   (split-github-url pkg-url))
     (or
      (for/or ([kind '("branches" "tags")])
        (define api-u
          (url "https" #f "api.github.com" #f #t
               (map (λ (x) (path/param x empty))
                    (list "repos" user repo kind))
               (append query
                       (if (and (github-client_id)
                                (github-client_secret))
                           (list (cons 'client_id (github-client_id))
                                 (cons 'client_secret (github-client_secret)))
                           empty))
               #f))
        (download-printf "Querying GitHub ~a\n" kind)
        (log-pkg-debug "Querying GitHub at ~a" (url->string api-u))
        (define api-bs
          (call/input-url+200
           api-u port->bytes
           #:headers (list (format "User-Agent: raco-pkg/~a" (version)))))
        (unless api-bs
          (error 'package-url->checksum
                 "could not connect to GitHub\n URL: ~a"
                 (url->string 
                  (struct-copy url api-u
                               [query query]))))
        (define branches
          (read-json (open-input-bytes api-bs)))
        (unless (and (list? branches)
                     (andmap hash? branches)
                     (andmap (λ (b) (hash-has-key? b 'name)) branches)
                     (andmap (λ (b) (hash-has-key? b 'commit)) branches))
          (error 'package-url->checksum
                 "Invalid response from Github: ~e"
                 api-bs))
        (for/or ([b (in-list branches)])
          (and (equal? (hash-ref b 'name) branch)
               (hash-ref (hash-ref b 'commit) 'sha))))
      ;; no matching branch/tag found, so if `branch' matches the
      ;; syntax of a commit id, then assume that it refers to a commit
      (and (regexp-match? #rx"[a-f0-9]+" branch)
           branch))]
    [_
     (define u (string-append pkg-url-str ".CHECKSUM"))
     (download-printf "Downloading checksum for ~a\n" pkg-name)
     (log-pkg-debug "Downloading checksum as ~a" u)
     (call/input-url+200 (string->url u)
                         port->string)]))

;; uses a custodian to avoid leaks:
(define (call-with-url url handler)
  (define c (make-custodian))
  (dynamic-wind
      void
      (lambda ()
        (define-values (p hs)
          (parameterize ([current-custodian c])
            (get-pure-port/headers url #:redirections 25 #:status? #t)))
        (begin0
          (and (string=? "200" (substring hs 9 12))
               (handler p))
          (close-input-port p)))
      (lambda ()
        (custodian-shutdown-all c))))

(define (read-from-server who url pred
                          [failure
                           (lambda (s)
                             (error who
                                    (~a "bad response from server\n"
                                        "  url: ~a\n"
                                        "  response: ~v")
                                    (url->string url)
                                    s))])
  (define bytes (call-with-url url port->bytes))
  ((if bytes
     (with-handlers ([exn:fail:read? (lambda (exn)
                                       (lambda () (failure bytes)))])
       (define v (read (open-input-bytes bytes)))
       (lambda ()
         (if (pred v)
           v
           (failure bytes))))
     (lambda () (failure #f)))))

(define (lift-directory-content pkg-dir path)
  (define orig-sub (let ([s (car path)])
                     (if (string? s)
                         (string->path s)
                         s)))
  ;; Delete everything except `orig-sub`:
  (for ([f (in-list (directory-list pkg-dir))])
    (unless (equal? f orig-sub)
      (delete-directory/files (build-path pkg-dir f))))
  ;; Get list of files and directories to move:
  (define sub-l (directory-list (apply build-path pkg-dir path)))
  ;; Make sure `sub` doesn't match a name we want to move here:
  (define sub
    (let loop ([sub orig-sub] [i 0])
      (cond
       [(member sub sub-l)
        ;; pick a new name:
        (loop (string->path (format "sub~a" i)) (add1 i))]
       [(not (equal? sub orig-sub))
        (rename-file-or-directory (build-path pkg-dir orig-sub)
                                  (build-path pkg-dir sub))
        sub]
       [else sub])))
  ;; Move content of `sub` out:
  (define sub-path (apply build-path (cons sub (cdr path))))
  (for ([f (in-list sub-l)])
    (rename-file-or-directory (build-path pkg-dir sub-path f)
                              (build-path pkg-dir f)))
  ;; Remove directory that we moved files out of:
  (delete-directory/files (build-path pkg-dir sub)))

(define (remove-extra-directory-layer pkg-dir)
  ;; Treat a single directory produced in `pkg-dir`
  ;; as having the content of the package, instead of
  ;; being included itself in the package content.
  (define l (directory-list pkg-dir))
  (when (= 1 (length l))
    (define orig-sub (car l))
    (when (directory-exists? (build-path pkg-dir orig-sub))
      (lift-directory-content pkg-dir (list orig-sub)))))

(provide (all-defined-out))
