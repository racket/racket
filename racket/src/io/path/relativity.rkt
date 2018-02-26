#lang racket/base
(require "../common/check.rkt"
         "path.rkt"
         "sep.rkt"
         "windows.rkt")

(provide relative-path?
         absolute-path?
         complete-path?)

(define-syntax-rule (define-...-path? id 
                      unix-bstr-check unix-str-check
                      windows-bstr-check)
  (define (id p)
    (check-path-test-argument 'id p)
    (cond
     [(path? p)
      (case (path-convention p)
        [(unix)
         (define bstr (path-bytes p))
         (unix-bstr-check bstr)]
        [(windows)
         (windows-bstr-check (path-bytes p))])]
     [(string? p)
      (and (string-no-nuls? p)
           (positive? (string-length p))
           (case (system-path-convention-type)
             [(unix)
              (unix-str-check p)]
             [(windows)
              (windows-bstr-check (string->path-bytes p))]))])))

(define (check-path-test-argument who p)
  (check who (lambda (p) (or (path? p) (string? p) (path-for-some-system? p)))
         #:contract "(or/c path? string? path-for-some-system?)"
         p))

(define-...-path? relative-path?
  (lambda (p) 
    (not (is-sep? (bytes-ref p 0) 'unix)))
  (lambda (p)
    (not (is-sep? (char->integer (string-ref p 0)) 'unix)))
  (lambda (p)
    (windows-relative-path-bytes? p)))

(define (windows-relative-path-bytes? p)
  (let ([bbq (backslash-backslash-questionmark-kind p)])
    (cond
      [(eq? bbq 'rel) #t]
      [bbq #f]
      [(is-sep? (bytes-ref p 0) 'windows) #f]
      [(letter-drive-start? p (bytes-length p)) #f]
      [else #t])))

(define-...-path? absolute-path?
  (lambda (p) 
    (is-sep? (bytes-ref p 0) 'unix))
  (lambda (p)
    (is-sep? (char->integer (string-ref p 0)) 'unix))
  (lambda (p)
    (not (windows-relative-path-bytes? p))))

(define-...-path? complete-path?
  (lambda (p) 
    (is-sep? (bytes-ref p 0) 'unix))
  (lambda (p)
    (is-sep? (char->integer (string-ref p 0)) 'unix))
  (lambda (p)
    (let ([bbq (backslash-backslash-questionmark-kind p)])
      (cond
        [bbq
         (and (not (eq? bbq 'red))
              (not (eq? bbq 'rel)))]
        [else
         (or (letter-drive-start? p (bytes-length p))
             (and (parse-unc p 0) #t))]))))
