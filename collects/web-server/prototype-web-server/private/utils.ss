(module utils mzscheme
  (require (lib "url.ss" "net")
           (lib "list.ss"))
  (provide url->servlet-path
           make-session-url
           split-url-path)

  ;; make-session-url: url (listof string) -> url
  ;; produce a new url for this session:
  ;;   Minimal path to the servlet.
  ;;   No query.
  ;;   No fragment.
  (define (make-session-url uri new-path)
    (make-url
     (url-scheme uri)
     (url-user uri)
     (url-host uri)
     (url-port uri)
     #t
     (map (lambda (p) (make-path/param p empty))
          new-path)
     '()
     #f
     ))

  ;; build-root-path: -> path
  ;; build the root path for whatever this OS is
  (define (build-root-path)
    (let loop ([prev (simplify-path (build-path 'same))]
               [next (simplify-path (build-path 'up))])
      (if (equal? prev next)
          prev
          (loop next
                (simplify-path (build-path next 'up))))))

  (define the-root-path (build-root-path))

  ;; simplify-url-path: url -> (listof string)
  ;; take the dots out of the url-path
  ;; Note: we simplify the url path relative to a hypothetical root,
  ;;       so that a malicious url can't cause the server to chase ".."
  ;;       up beyond the legitimate servlet root.
  (define (simplify-url-path uri)
    (path->list
     (simplify-path
      (apply build-path
             (cons the-root-path
                   (map
                    (lambda (str)
                      (if (string=? str "")
                          'same
                          str))
                    (map
                     (lambda (path-elt)
                       (if (path/param? path-elt)
                           (path/param-path path-elt)
                           path-elt))
                     (url-path uri))))))))

  ;; path->list pth
  ;; convert an absolute path to a list of strings
  (define (path->list pth)
    (reverse
     (let path->list ([pth pth])
       (let-values ([(base name must-be-dir?) (split-path pth)])
         (if base
             (cons (path->string name) (path->list base))
             '())))))


  ;; url->servlet-path: path url -> (values (union path #f)
  ;;                                        (union (listof url->string) #f)
  ;;                                        (union (listof string) #f))
  ;; Given a servlet directory and url, find a servlet.
  ;;   The first value is the servlet path.
  ;;   The second value is the prefix of the url-path used to find the servlet.
  ;;   The third value is the remaining suffix of the url-path.
  (define (url->servlet-path servlet-dir uri)
    #;(printf "   current-directory = ~s~n" (current-directory))
    (let loop ([base-path servlet-dir]
               [servlet-path '()]
               [path-list (simplify-url-path uri)])
      (if
       (null? path-list)
       (values #f #f #f)
       (let* ([next-path-segment (car path-list)]
              [new-base (build-path base-path next-path-segment)])
         #;(printf "   new-base = ~s~n" new-base)
         (cond
           [(file-exists? new-base)
            (values new-base
                    (reverse (cons next-path-segment servlet-path))
                    (cdr path-list))]
           [else (loop new-base
                       (cons next-path-segment servlet-path)
                       (cdr path-list))])))))
  
  ;; split-url-path: url url -> (union (listof string) #f)
  ;; the first url's path is a prefix of the path of the second
  ;; find the suffix and return it as a list of strings
  (define (split-url-path pref-url suff-url)
    (let loop ([pref-path (simplify-url-path pref-url)]
               [suff-path (simplify-url-path suff-url)])
      (cond
        [(null? pref-path) suff-path]
        [(string=? (car pref-path) (car suff-path))
         (loop (cdr pref-path) (cdr suff-path))]
        [else
         (error "split-url-path: first path is not a preffix of the second")])))    
  )
