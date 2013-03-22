#lang racket/base
(require racket/path
         racket/list
         racket/function
         racket/file
         racket/port
         racket/match
         net/url
         json)

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

(define (call/input-url+200 u fun)
  #;(printf "\t\tReading ~a\n" (url->string u))
  (define-values (ip hs) (get-pure-port/headers u #:redirections 25 #:status? #t))
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

(define (package-url->checksum pkg-url-str [query empty])
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
            query
            #f))
     (define api-bs
       (call/input-url+200 api-u port->bytes))
     (unless api-bs
       (error 'package-url->checksum 
              "Could not connect to GitHub"))
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
     (call/input-url+200 (string->url (string-append pkg-url-str ".CHECKSUM"))
                         port->string)]))

(provide (all-defined-out))
