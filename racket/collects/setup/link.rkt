#lang racket/base
(require racket/file
         racket/path
         setup/dirs
         setup/collection-name)

(provide links)

(define (links #:error [error error]
               #:user? [user? #t]
               #:user-version [user-version (and user?
                                                 (get-installation-name))]
               #:file [in-file #f]
               #:name [name #f]
               #:version-regexp [version-regexp #f]
               #:root? [root? #f]
               #:static-root? [static-root? #f]
               #:remove? [remove? #f]
               #:show? [show? #f]
               #:repair? [repair? #f]
               #:with-path? [with-path? #f]
               . dirs)
  (define (check-name name)
    (unless (collection-name-element? name)
      (error 'links "name is not valid as a top-level collection name: ~e"
             name)))

  (when name
    (check-name name))

  (define file (or in-file
                   (if user?
                       (build-path (find-system-path 'addon-dir) user-version "links.rktd")
                       (find-links-file))))

  (define need-repair? #f)

  (define (content-error str v)
    (if repair?
        (begin
          (log-warning (format "~a~e" str v))
          (set! need-repair? #t)
          #f)
        (error 'links "~a~e" str v)))

  (define table
    (with-handlers ([exn:fail?
                     (lambda (exn)
                       (let ([msg (format
                                   "error reading from link file: ~s: ~a"
                                   file
                                   (exn-message exn))])
                         (if repair?
                             (begin
                               (log-warning msg)
                               (set! need-repair? #t)
                               null)
                             (error 'links "~a" msg))))])
      (if (and file (file-exists? file))
          (let ([l (with-input-from-file file read)])
            (if (list? l)
                (for/list ([e (in-list l)]
                           #:when
                           (or (and (list? e)
                                    (or (= 2 (length e))
                                        (= 3 (length e))))
                               (content-error "entry is a not a 2- or 3-element list: " e))
                           #:when 
                           (or (or (string? (car e))
                                   (eq? 'root (car e))
                                   (eq? 'static-root (car e)))
                               (content-error "entry's first element is not a string, 'root, or 'static-root: " e))
                           #:when 
                           (or (path-string? (cadr e))
                               (content-error "entry's second element is not a path string: " e))
                           #:when
                           (or (null? (cddr e))
                               (regexp? (caddr e))
                               (content-error "entry's third element is not a version regexp: " e)))
                  e)
                (begin
                  (content-error "content is not a list: " l)
                  null)))
          null)))

  (define mapped (make-hash))

  (define (add-entry! e)
    (hash-set! mapped 
               (car e) 
               (cons (cdr e) (hash-ref mapped (car e) null))))


  (for ([e (in-list table)]) (add-entry! e))

  (define file-dir 
    (and file
         (let-values ([(base name dir?) 
                       (split-path (simplify-path 
                                    (path->complete-path file)))])
           base)))

  (define (simplify p)
    (simplify-path (path->complete-path p file-dir)))

  (define new-table
    (reverse
     (for/fold ([table (reverse table)]) ([d (in-list 
                                              (if (and (null? dirs)
                                                       name)
                                                  '(#f)
                                                  dirs))])
       (let* ([dp (and d 
                       (find-relative-path file-dir
                                           (simplify-path 
                                            (path->complete-path d))
                                           #:more-than-root? #t))]
              [a-name (if root?
                          (if static-root?
                              'static-root
                              'root)
                          (and d
                               (or name
                                   (let-values ([(base name dir?) (split-path dp)])
                                     (path-element->string name)))))]
              [rx version-regexp]
              [d (and dp (path->string dp))]
              [sd (and d (simplify d))])
         (unless remove?
           (unless (directory-exists? sd)
             (error 'links
                    "no such directory for link: ~a"
                    sd)))
         (if remove?
             (filter (lambda (e) 
                       (or (and d
                                (not (equal? (simplify (cadr e)) 
                                             sd)))
                           (and name
                                (not (equal? (car e) name)))
                           (and root?
                                (not (or (eq? (car e) 'root)
                                         (eq? (car e) 'static-root))))
                           (and version-regexp
                                (pair? (cddr e))
                                (not (equal? (caddr e) version-regexp)))))
                     table)
             (let ([l (hash-ref mapped a-name null)]
                   [e (list* a-name 
                             d
                             (if rx (list rx) null))])
               (if (member (cdr e) l)
                   table
                   (let ()
                     (when (string? a-name)
                       (check-name a-name))
                     (add-entry! e)
                     (cons e table)))))))))

  (unless (and (not need-repair?)
               (equal? new-table table))
    (let ([dir (let-values ([(base name dir?) (split-path file)])
                 base)])
      (make-directory* dir)
      (call-with-atomic-output-file
       file
       (lambda (o tmp-path)
         (parameterize ([current-output-port o])
           (printf "(")
           (let loop ([l new-table] [prefix ""])
             (cond
              [(null? l) (printf ")\n")]
              [else
               (printf "~a~s" prefix (car l))
               (unless (null? (cdr l)) (newline))
               (loop (cdr l) " ")])))))))

  (when show?
    (for ([e (in-list new-table)])
      (printf " ~a~s  path: ~s~a\n"
              (if (or (eq? (car e) 'root)
                      (eq? (car e) 'static-root))
                  ""
                  "collection: ")
              (car e)
              (path->string (simplify (cadr e)))
              (if (null? (cddr e))
                  ""
                  (format "  version: ~s"
                          (caddr e))))))

  (if remove?
      ;; return list of removed entries:
      (filter (lambda (e) (not (member e new-table))) table)
      (if root?
          ;; Return root paths:
          (for/list ([e (in-list new-table)]
                     #:when (or (eq? 'root (car e))
                                (eq? 'static-root (car e)))
		     #:when (or (null? (cddr e))
				(regexp-match? (caddr e) (version))))
            (simplify (cadr e)))
          ;; Return list of collections mapped for this version:
          (let ([ht (make-hash)])
            (for ([e (in-list new-table)])
              (when (and (string? (car e))
                         (or (null? (cddr e))
                             (regexp-match? (caddr e) (version))))
                (hash-set! ht (if with-path?
                                  (cons (car e) (simplify (cadr e)))
                                  (car e))
                           #t)))
            (hash-keys ht)))))
