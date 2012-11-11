#lang racket/base

;; Library for accessing paths relative to a source file at runtime

(require racket/list
         setup/dirs
         "private/this-expression-source-directory.rkt"
         (only-in "private/runtime-path-table.rkt" table)
         (for-syntax racket/base))
  
(provide ;; used to be in `mzlib/runtime-path`
         define-runtime-path
         define-runtime-paths
         define-runtime-path-list
         define-runtime-module-path-index
         runtime-paths
         ;; from `racket/runtime-path`
         (for-syntax #%datum)
         define-runtime-module-path)

(define-syntax (define-runtime-module-path stx)
  (syntax-case stx ()
    [(_ id mod-path)
     (begin
       (unless (memq (syntax-local-context) '(top-level module module-begin))
         (raise-syntax-error #f
                             "allowed only in a module top-level or top-level context"
                             stx))
       (unless (identifier? #'id)
         (raise-syntax-error #f
                             "expected an identifier to bind"
                             stx
                             #'id))
       (unless (module-path? (syntax->datum #'mod-path))
         (raise-syntax-error #f
                             "expected a literal module path"
                             stx
                             #'mod-path))
       #`(begin
           (require (only-in (for-label mod-path)))
           (define id (combine-module-path (#%variable-reference) 'mod-path))))]))

(define (combine-module-path vr mod-path)
  (module-path-index-resolve (module-path-index-join
                              mod-path
                              (variable-reference->resolved-module-path vr))))

;; ----------------------------------------
;; the code below used to be in `mzlib/runtime-path`
  
(define-for-syntax ext-file-table (make-hasheq))

(define (lookup-in-table var-ref p)
  ;; This function is designed to cooperate with a table embedded
  ;; in an executable by create-embedding-executable.
  (let ([modname (variable-reference->resolved-module-path var-ref)])
    (let ([p (hash-ref
              table
              (cons (resolved-module-path-name modname)
                    (if (path? p)
                        (path->bytes p)
                        (if (and (pair? p) (eq? 'module (car p)))
                            (list 'module (cadr p))
                            p)))
              #f)])
      (and p
           (car p)
           (let* ([p (car p)]
                  [p (if (bytes? p)
                         (bytes->path p)
                         p)])
             (if (symbol? p)
                 (module-path-index-join (list 'quote p) #f) ; make it a module path index
                 (if (absolute-path? p)
                     p
                     (parameterize ([current-directory (find-system-path 'orig-dir)])
                       (or (find-executable-path (find-system-path 'exec-file) p #t)
                           (build-path (current-directory) p))))))))))

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
               (let* ([strs (regexp-split #rx"/" 
                                          (let ([s (cadr p)])
                                            (if (regexp-match? #rx"[./]" s)
                                                s
                                                (string-append s "/main.rkt"))))])
                 (apply collection-file-path
                        (last strs)
                        (if (and (null? (cddr p))
                                 (null? (cdr strs)))
                            (list "mzlib")
                            (append (cddr p) (drop-right strs 1)))))]
              [(and (list? p)
	      ((length p) . = . 3)
	      (eq? 'module (car p))
                    (or (not (caddr p))
                        (variable-reference? (caddr p))))
               (let ([p (cadr p)]
                     [vr (caddr p)])
                 (unless (module-path? p)
                   (error 'runtime-path "not a module path: ~.s" p))
                 (let ([base (and vr
                                  (variable-reference->resolved-module-path vr))])
                   (if (and (pair? p)
                            (eq? (car p) 'submod)
                            (path? (cadr p)))
                       (module-path-index-join `(submod "." ,@(cddr p)) 
                                               (module-path-index-join (cadr p) base))
                       (module-path-index-join p base))))]
              [else (error 'runtime-path "unknown form: ~.s" p)])))
         paths)))
  
(define-for-syntax (register-ext-files var-ref paths)
  (let ([modname (variable-reference->resolved-module-path var-ref)])
    (let ([files (hash-ref ext-file-table modname null)])
      (hash-set! ext-file-table modname (append paths files)))))

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
       (let ([phase (sub1 (variable-reference->phase (#%variable-reference)))])
         (unless (zero? phase)
           (log-warning "warning: ~a: at phase level ~a: ~e"
                        (syntax-e (syntax-case #'orig-stx () [(id . _) #'id]))
                        phase
                        #'orig-stx)))
       #`(begin
           (define-values (id ...)
             (let-values ([(id ...) expr])
               (let ([get-dir (lambda ()
                                #,(datum->syntax
                                   #'orig-stx
                                   `(,#'this-expression-source-directory)
                                   #'orig-stx))])
                 (apply to-values (resolve-paths (#%variable-reference)
                                                 get-dir
                                                 (to-list id ...))))))
           (begin-for-syntax
            (register-ext-files 
             (#%variable-reference)
             (let-values ([(id ...) expr])
               (to-list id ...))))))]))

(define-syntax (define-runtime-path stx)
  (syntax-case stx ()
    [(_ id expr) #`(-define-runtime-path #,stx (id) expr list values)]))

(define-syntax (define-runtime-paths stx)
  (syntax-case stx ()
    [(_ (id ...) expr) #`(-define-runtime-path #,stx (id ...) expr list values)]))

(define-syntax (define-runtime-path-list stx)
  (syntax-case stx ()
    [(_ id expr) #`(-define-runtime-path #,stx (id) expr values list)]))

(define-syntax (define-runtime-module-path-index stx)
  (syntax-case stx ()
    [(_ id expr) #`(-define-runtime-path #,stx (id) `(module ,expr ,(#%variable-reference)) list values)]))

(define-syntax (runtime-paths stx)
  (syntax-case stx ()
    [(_ mp)
     #`(quote
        #,(hash-ref
           ext-file-table
           (module-path-index-resolve 
            (let ([p (syntax->datum #'mp)]
                  [base (syntax-source-module stx)])
              (if (and (pair? p) (eq? (car p) 'submod) (path? (cadr p)))
                  (module-path-index-join `(submod "." ,@(cddr p))
                                          (module-path-index-join (cadr p) base))
                  (module-path-index-join p base))))
           null))]))
