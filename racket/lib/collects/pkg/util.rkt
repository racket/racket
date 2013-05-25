#lang racket/base
(require racket/path
         racket/list
         racket/function
         racket/file
         racket/port
         racket/match
         racket/format
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

(define (call/input-url+200 u fun #:headers [headers '()])
  #;(printf "\t\tReading ~a\n" (url->string u))
  (define-values (ip hs) (get-pure-port/headers u headers
                                                #:redirections 25
                                                #:status? #t))
  (and (string=? "200" (substring hs 9 12))
       (fun ip)))

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

(define (package-url->checksum pkg-url-str [query empty]
                               #:download-printf [download-printf void])
  (define pkg-url
    (string->url pkg-url-str))
  (match (url-scheme pkg-url)
    ["github"
     (match-define (list* user repo branch path)
                   (map path/param-path (url-path/no-slash pkg-url)))
     (define api-u
       (url "https" #f "api.github.com" #f #t
            (map (λ (x) (path/param x empty))
                 (list "repos" user repo "branches"))
            (append query
                    (if (and (github-client_id)
                             (github-client_secret))
                      (list (cons 'client_id (github-client_id))
                            (cons 'client_secret (github-client_secret)))
                      empty))
            #f))
     (download-printf "Querying GitHub\n")
     (log-pkg-debug "Querying GitHub at ~a" (url->string api-u))
     (define api-bs
       (call/input-url+200
        api-u port->bytes
        #:headers (list (format "User-Agent: raco-pkg/~a" (version)))))
     (unless api-bs
       (error 'package-url->checksum
              "Could not connect to GitHub"
              (url->string api-u)))
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
            (hash-ref (hash-ref b 'commit) 'sha)))]
    [_
     (define u (string-append pkg-url-str ".CHECKSUM"))
     (download-printf "Downloading checksum\n")
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

(provide (all-defined-out))
