
(module runtime-path mzscheme
  (require (lib "etc.ss")
           (lib "modcollapse.ss" "syntax")
	   (lib "dirs.ss" "setup")
           (only "private/runtime-path-table.ss" table))

  (provide define-runtime-path
           define-runtime-paths
           define-runtime-path-list
           runtime-paths)
  
  (define-for-syntax ext-file-table (make-hash-table))

  (define (lookup-in-table tag-stx p)
    ;; This function is designed to cooperate with a table embedded
    ;; in an executable by create-embedding-executable.
    (let ([mpi (syntax-source-module tag-stx)])
      (let ([p (hash-table-get
                table
                (cons (cond
                        [(module-path-index? mpi)
                         (module-path-index-resolve mpi)]
                        [(symbol? mpi) mpi]
                        [else #f])
                      (if (path? p)
                          (path->bytes p)
                          p))
                #f)])
        (and p
             (car p)
             (let* ([p (car p)]
                    [p (if (bytes? p)
                           (bytes->path p)
                           p)])
               (if (absolute-path? p)
                   p
                   (parameterize ([current-directory (find-system-path 'orig-dir)])
                     (or (find-executable-path (find-system-path 'exec-file) p #t)
                         (build-path (current-directory) p)))))))))

  (define (resolve-paths tag-stx get-base paths)
    (let ([base #f])
      (map (lambda (p)
             (or 
              ;; Check table potentially substituted by
              ;;  mzc --exe:
              (and table
                   (lookup-in-table tag-stx p))
              ;; Normal resolution
              (cond
                [(and (or (string? p) (path? p))
                      (not (complete-path? p)))
                 (unless base
                   (set! base (get-base)))
                 (path->complete-path p base)]
                [(string? p) (string->path p)]
                [(path? p) p]
		[(and (list? p)
		      (= 2 (length p))
		      (eq? 'so (car p))
		      (string? (cadr p)))
		 (let ([f (path-replace-suffix (cadr p) (system-type 'so-suffix))])
		   (or (ormap (lambda (p)
				(let ([p (build-path p f)])
				  (and (file-exists? p)
				       p)))
			      (get-lib-search-dirs))
		       (cadr p)))]
		[(and (list? p)
		      ((length p) . > . 1)
		      (eq? 'lib (car p))
		      (andmap string? (cdr p)))
		 (let ([dir (if (null? (cddr p))
                                (collection-path "mzlib")
                                (apply collection-path (cddr p)))])
                   (build-path dir (cadr p)))]
                [else (error 'runtime-path "unknown form: ~e" p)])))
           paths)))
  
  (define-for-syntax (register-ext-files tag-stx paths)
    (let ([mpi (syntax-source-module tag-stx)])
      (let ([modname (cond
                       [(module-path-index? mpi) (module-path-index-resolve mpi)]
                       [(symbol? mpi) mpi]
                       [else (error 'register-ext-files
                                    "cannot determine source")])])
        (let ([files (hash-table-get ext-file-table modname null)])
          (hash-table-put! ext-file-table modname (append paths files))))))
    
  (define-syntax (-define-runtime-path stx)
    (syntax-case stx ()
      [(_ orig-stx (id ...) expr to-list to-values)
       (let ([ids (syntax->list #'(id ...))])
         (unless (memq (syntax-local-context) '(module module-begin top-level))
           (raise-syntax-error #f "allowed only at the top level" #'orig-stx))
         (for-each (lambda (id)
                     (unless (identifier? id)
                       (raise-syntax-error
                        #f
                        #'orig-stx
                        id)))
                   ids)
         (let ([tag (datum->syntax-object #'orig-stx 'tag #'orig-stx)])
           #`(begin
               (define-values (id ...)
                 (let-values ([(id ...) expr])
                   (let ([get-dir (lambda ()
                                    #,(datum->syntax-object
                                       tag
                                       `(,#'this-expression-source-directory)
                                       tag))])
                     (apply to-values (resolve-paths (quote-syntax #,tag) 
                                                     get-dir
                                                     (to-list id ...))))))
               (begin-for-syntax
                 (register-ext-files 
                  (quote-syntax #,tag) 
                  (let-values ([(id ...) expr])
                    (to-list id ...)))))))]))
  
  (define-syntax (define-runtime-path stx)
    (syntax-case stx ()
      [(_ id expr) #`(-define-runtime-path #,stx (id) expr list values)]))
  
  (define-syntax (define-runtime-paths stx)
    (syntax-case stx ()
      [(_ (id ...) expr) #`(-define-runtime-path #,stx (id ...) expr list values)]))

  (define-syntax (define-runtime-path-list stx)
    (syntax-case stx ()
      [(_ id expr) #`(-define-runtime-path #,stx (id) expr values list)]))
  
  (define-syntax (runtime-paths stx)
    (syntax-case stx ()
      [(_ mp)
       #`(quote
          #,(hash-table-get
             ext-file-table
             (module-path-index-resolve (module-path-index-join
                                         (syntax-object->datum #'mp)
                                         (syntax-source-module stx)))
             null))]))

  )
