#lang racket/base
(require net/url
         racket/list
         racket/contract)
(require web-server/private/util)
(define url->path/c
  ((url?) () . ->* . (values path? (listof path-piece?))))

(provide/contract
 [url->path/c contract?]
 [make-url->path (path-string? . -> . url->path/c)]
 [make-url->valid-path (url->path/c . -> . url->path/c)]
 [filter-url->path (regexp? url->path/c . -> . url->path/c)])

(define (build-path* . l)
  (if (empty? l)
      (build-path 'same)
      (apply build-path l)))

(define ((make-url->path base) u)
  (define nbase (path->complete-path base))
  (define the-path
    ; Complete it against the base
    (path->complete-path
     ; Build a path
     (apply build-path*
            ; Remove all ".."s
            (strip-prefix-ups
             (map (lambda (p)
                    (if (and (string? p) (string=? "" p))
                        'same
                        p))
                  ; Extract the paths from the url-path
                  (map path/param-path 
                       (url-path u)))))
     nbase))
  (define w/o-base (path-without-base nbase the-path))
  #;(printf "~S\n" `(url->path ,base ,nbase ,(url->string u) ,the-path ,w/o-base))
  (values the-path w/o-base))

(define ((make-url->valid-path url->path) u)
  (let loop ([up (url-path u)])
    #;(printf "~S\n" `(url->valid-path ,(url->string u) ,up))
    (with-handlers ([exn:fail? (lambda (exn)
                            #;((error-display-handler) (exn-message exn) exn)
                            (if (empty? up)
                                (raise exn)
                                (loop (reverse (rest (reverse up))))))])
      (define-values (p w/o-base)
        (url->path (url-replace-path (lambda _ up) u)))
      (unless (or (file-exists? p) (link-exists? p))
        (raise (make-exn:fail:filesystem:exists (string->immutable-string (format "No valid path: ~a" p)) (current-continuation-marks))))
      (values p w/o-base))))

(define ((filter-url->path regex url->path) u)
  (define-values (p w/o-base) (url->path u))
  (if (regexp-match regex (path->string p))
      (values p w/o-base)
      (raise (make-exn:fail:filesystem:exists (string->immutable-string (format "Does not pass filter: ~a" p))
                                              (current-continuation-marks)))))
