(module filesystem-map mzscheme
  (require (lib "url.ss" "net")
           (lib "list.ss")
           (lib "contract.ss"))
  (require "../private/util.ss")
  (define url-path?
    ((url?) . ->* . (path? list?)))
  
  (provide/contract
   [make-url->path (path? . -> . url-path?)]
   [make-url->path/optimism (url-path? . -> . url-path?)])  
  
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
              ; XXX Should error?
              (strip-prefix-ups
               (map (lambda (p)
                      (if (string=? "" p) 'same p))
                    ; Extract the paths from the url-path
                    (map path/param-path 
                         (url-path u)))))
       nbase))
    (define w/o-base (path-without-base nbase the-path))
    #;(printf "~S~n" `(url->path ,base ,nbase ,(url->string u) ,the-path ,w/o-base))
    (values the-path w/o-base))
  
  (define ((make-url->path/optimism url->path) u)
    (let loop ([up (url-path u)])
      #;(printf "~S~n" `(url->path/optimism ,(url->string u) ,up))
      (with-handlers ([exn? (lambda (exn)
                              #;((error-display-handler) (exn-message exn) exn)
                              (if (empty? up)
                                  (raise exn)
                                  (loop (reverse (rest (reverse up))))))])
        (define-values (p w/o-base)
          (url->path (url-replace-path (lambda _ up) u)))
        (unless (or (file-exists? p) (link-exists? p))
          (raise (make-exn:fail:filesystem:exists (string->immutable-string "No valid path") (current-continuation-marks))))
        (values p w/o-base)))))