;; Utilities for creating a .plt package
(module pack mzscheme
  (require (lib "deflate.ss")
           (lib "base64.ss" "net")
           (lib "process.ss")
	   (lib "list.ss")
	   (lib "etc.ss")
	   (lib "port.ss")
	   (lib "file.ss")
	   (lib "getinfo.ss" "setup"))

  (provide pack mztar std-filter pack-collections)

  (define pack
    (opt-lambda (dest name paths collections
                      [filter std-filter]
                      [encode? #t]
                      [file-mode 'file]
                      [unpack-unit #f]
                      [plt-relative? #t]
                      [requires null]
                      [conflicts null]
                      [plt-home-relative? #f])
      (define file (open-output-file dest 'truncate/replace))
      (define-values (fileout thd)
        (if encode?
          (let-values ([(b64-out b64-in) (make-pipe 4096)]
                       [(gz-out gz-in) (make-pipe 4096)])
            (thread
             (lambda ()
               (let ([normal (lambda ()
                               (gzip-through-ports gz-out b64-in #f 0)
                               (close-output-port b64-in))]
                     [gzip (find-executable-path "gzip" #f)])
                 (if gzip
                   (let ([p (process* gzip "-c")])
                     (if (eq? 'running ((list-ref p 4) 'status))
                       (begin
                         ;; Use gzip process.
                         ;; Errors to error port:
                         (thread
                          (lambda ()
                            (dynamic-wind
                              void
                              (lambda () (copy-port (cadddr p) (current-error-port)))
                              (lambda () (close-input-port (cadddr p))))))
                         ;; Copy input to gzip:
                         (thread
                          (lambda ()
                            (dynamic-wind
                              void
                              (lambda () (copy-port gz-out (cadr p)))
                              (lambda ()
                                (close-input-port gz-out)
                                (close-output-port (cadr p))))))
                         ;; Copy input to b64:
                         (dynamic-wind
                           void
                           (lambda () (copy-port (car p) b64-in))
                           (lambda ()
                             (close-input-port (car p))
                             (close-output-port b64-in))))
                       (normal)))
                   (normal)))))
            (values gz-in (thread (lambda ()
                                    (base64-encode-stream b64-out file)
                                    (close-output-port file)))))
          (values file (thread void))))
      (fprintf fileout "PLT\n")
      (write `(lambda (request failure)
                (case request
                  [(name) ,name]
                  [(unpacker) 'mzscheme]
                  [(requires) ',requires]
                  [(conflicts) ',conflicts]
                  [(plt-relative?) ,plt-relative?]
                  [(plt-home-relative?) ,(and plt-relative? plt-home-relative?)]
                  [else (failure)]))
             fileout)
      (newline fileout)
      (write (or unpack-unit
                 `(unit (import main-collects-parent-dir mzuntar)
                        (export)
                        (mzuntar void)
                        (quote ,collections)))
             fileout)
      (newline fileout)
      (for-each (lambda (path) (mztar path fileout filter file-mode)) paths)
      (close-output-port fileout)
      (thread-wait thd)))

  (define (element->string x)
    (if (path? x) (path->string x) x))

  (define (mztar path output filter file-mode)
    (define (path->list p)
      (if (eq? p 'same)
        null
        (let-values ([(base name dir?) (split-path p)])
          (if (path? base)
            (append (path->list base) (list name))
            (list name)))))
    (define-values (init-dir init-files)
      (if (file-exists? path)
        (let-values ([(base name dir?) (split-path path)])
          (values (if (eq? base 'relative) 'same base) (list name)))
        (values (if (string? path) (string->path path) path) #f)))

    (let loop ([dir init-dir] [dpath (path->list init-dir)] [files init-files])
      (printf "MzTarring ~a...\n" (if files (build-path dir (car files)) dir))
      (fprintf output "~s\n~s\n" 'dir (map element->string dpath))
      (for-each (lambda (f)
                  (let* ([p (build-path dir f)]
                         [filter-val (filter p)])
                    (when filter-val
                      (if (directory-exists? p)
                        (loop p (append dpath (list f)) #f)
                        (let ([len (file-size p)])
                          ;; (printf "MzTarring ~a~n" p)
                          (fprintf output "~s~n~s~n~s~n*"
                                   (case filter-val
                                     [(file) 'file]
                                     [(file-replace) 'file-replace]
                                     [else file-mode])
                                   (map element->string (append dpath (list f)))
                                   len)
                          (call-with-input-file* p
                            (lambda (p) (copy-port p output))))))))
                (or files
                    (sort (map path->string (directory-list dir)) string<?)))))

  (define (std-filter path)
    (let ([name (path->bytes (let-values ([(_1 name _2) (split-path path)])
                               name))])
      (not (or (regexp-match #rx#"^(?:compiled|CVS|[.]svn|[.]cvsignore)$" name)
               (regexp-match #rx#"(?:~$|^#.*#$|[.]plt$|^[.]#)" name)))))

  (define pack-collections
    (opt-lambda (output name collections replace? extra-setup-collections
                        [file-filter std-filter] [plt-home-relative? #f])
      (let-values
          ([(output) (path->complete-path output)]
           [(dir source-files requires conflicts name)
            (let ([dirs (map (lambda (cp) (apply collection-path cp)) collections)])
              ;; Figure out the base path:
              (let* ([base-path #f]
                     [base-path-setter #f]
                     [rel-paths
                      (map (lambda (dir coll)
                             (let*-values
                                 ([(base c-name dir?) (split-path dir)]
                                  [(base subdir)
                                   (let loop ([l (cdr coll)] [base base])
                                     (let-values ([(base x-name dir?) (split-path base)])
                                       (if (null? l)
                                         (values base x-name)
                                         (let-values ([(base subdir) (loop (cdr l) base)])
                                           (values base (build-path subdir x-name))))))])
                               (if base-path
                                 (unless (equal? base base-path)
                                   (error 'mzc
                                          "cannot combine collections that live in different directories: \"~a\" and: \"~a\""
                                          base-path-setter
                                          dir))
                                 (begin (set! base-path-setter dir)
                                        (set! base-path base)))
                               (build-path 'same subdir c-name)))
                           dirs collections)]
                     [infos (map (lambda (cp) (get-info cp))
                                 collections)]
                     [coll-list? (lambda (cl)
                                   (and (list? cl)
                                        (andmap (lambda (c)
                                                  (and (list? c)
                                                       (andmap string? c)
                                                       (andmap relative-path? c)))
                                                cl)))]
                     [get-dep-coll (lambda (which)
                                     (apply append (map (lambda (i src-cp)
                                                          (let ([rl (if i
                                                                      (i which (lambda () null))
                                                                      null)])
                                                            (unless (coll-list? rl)
                                                              (error
                                                               'mzc
                                                               "bad ~a specification in info.ss for collection ~s"
                                                               which
                                                               src-cp))
                                                            rl))
                                                        infos collections)))])
                (values base-path
                        rel-paths
                        (get-dep-coll 'requires)
                        (append (if replace? null collections)
                                (get-dep-coll 'conflicts))
                        (or name
                            ((or (car infos)
                                 (lambda (n f) (caar collections)))
                             'name
                             (lambda () (caar collections)))))))])
	(parameterize ([current-directory dir])
          (pack output name
                source-files
                (append extra-setup-collections (filter get-info collections))
                file-filter #t
                (if replace? 'file-replace 'file)
                #f
                #t ; plt-relative
                ;; For each require, get current version
                (map (lambda (r)
                       (let ([i (get-info r)])
                         (let ([v (and i (i 'version (lambda () #f)))])
                           (if v
                             (begin
                               (unless (and (list? v)
                                            (andmap (lambda (x)
                                                      (and (number? v)
                                                           (exact? v)
                                                           (integer? v)))
                                                    v))
                                 (error
                                  'mzc
                                  "bad version specification in info.ss for collection ~s"
                                  r))
                               (list r v))
                             (list r null)))))
                     (cons '("mzscheme") requires))
                conflicts
                plt-home-relative?))))))
