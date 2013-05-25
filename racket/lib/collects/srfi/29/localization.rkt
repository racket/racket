(module localization mzscheme
  
  (require mzlib/contract
           mzlib/file
           mzlib/runtime-path
           mzlib/string
           syntax/modread)
  
  (provide/contract (current-language (parameter/c symbol?))
                    (current-country (parameter/c symbol?))
                    (current-locale-details (parameter/c (listof symbol?)))
                    (declare-bundle! (-> (listof symbol?) (listof pair?) any))
                    (load-bundle! (->* ((listof symbol?)) any/c any))
                    (store-bundle! (-> (listof symbol?) any))
                    (localized-template (-> symbol? any/c any)))
  
  (provide re-read-locale)
  
  (define get-from-locale
    (lambda (what)
      (let ((locale (current-locale)))
	(if (string=? locale "")
	    (case what
	      ((language) 'en) ;; Default language: English
	      ((country) 'us) ;; Default country: US
	      (else null))
	    (let ((len (string-length locale)))
	      (case what
		((language)
		 (if (>= len 2)
		     (string->symbol (substring locale 0 2))
		     'en))
		((country)
		 (if (>= len 5)
		     (string->symbol (substring locale 3 5))
		     'us))
		(else ;; details
		 (if (> len 6)
		     (list (string->symbol (substring locale 6)))
		     null))))))))
  
  ;; The association list in which bundles will be stored
  (define *localization-bundles*
    (make-hash-table 'equal))
  
  (define current-language
    (make-parameter (get-from-locale 'language)))
  
  (define current-country
    (make-parameter (get-from-locale 'country)))
  
  (define current-locale-details
    (make-parameter (get-from-locale 'details)))
  
  (define (make-name bundle-specifier)
    (string->symbol
     (string-append "srfi-29:"
                    (expr->string bundle-specifier))))
  
  (define (declare-bundle! bundle-specifier bundle-assoc-list)
    (hash-table-put! *localization-bundles* bundle-specifier bundle-assoc-list))
  
  (define (store-bundle! bundle-specifier)
    (put-preferences (list (make-name bundle-specifier))
                     (list (hash-table-get *localization-bundles* bundle-specifier)))
    #t)
  
  (define (load-bundle-from-preference! bundle-specifier)
    (let/ec k
      (declare-bundle! bundle-specifier
                       (get-preference (make-name bundle-specifier)
                                       (lambda () (k #f))))
      #t))
  
  ;; If you change (current-locale), you don't have to set current-*
  ;; by hand, you can simply call this procedure, and it will update
  ;; those parameters to the values in the new locale.
  (define (re-read-locale)
    (current-language (get-from-locale 'language))
    (current-country (get-from-locale 'country))
    (current-locale-details (get-from-locale 'details)))
  
  ;; System bundles are here:
  (define-runtime-path system-bundles "bundles")
  
  (define (with-reader-params thunk)
    ;; Use `with-module-reading-parameterization' to get
    ;; most defaults...
    (with-module-reading-parameterization 
     (lambda ()
       ;; ... but disable `#reader':
       (parameterize ([read-accept-reader #f])
         (thunk)))))
  
  ;; load-bundle! accpect an alternate-path to search bundle
  (define (load-bundle! bundle-specifier . alternate-path)
    (or (load-bundle-from-preference! bundle-specifier)
        (let* ((filename (case (length bundle-specifier)
                           ((1) (symbol->string (car bundle-specifier)))
                           ((2) (build-path (symbol->string (cadr bundle-specifier))
                                            (symbol->string (car bundle-specifier))))
                           (else (build-path (symbol->string (cadr bundle-specifier))
                                             (symbol->string (caddr bundle-specifier))
                                             (symbol->string (car bundle-specifier))))))
               (path (build-path (if (null? alternate-path)
                                     system-bundles
                                     (car alternate-path))
                                 filename)))
          (and (file-exists? path)
               (declare-bundle! bundle-specifier
                                (with-reader-params
                                 (lambda ()
                                   (with-input-from-file path read))))
               #t))))
  (define (rdc ls)
    (if (null? (cdr ls))
        '()
        (cons (car ls) (rdc (cdr ls)))))
  
  ;;Retrieve a localized template given its package name and a template name
  (define (localized-template package-name template-name)
    (let loop ((specifier (list package-name
                                (current-language)
                                (current-country))))
      (and (not (null? specifier))
           (let ((bundle (hash-table-get *localization-bundles* specifier #f)))
             (cond ((and bundle (assq template-name bundle)) => cdr)
                   ((null? (cdr specifier)) #f)
                   (else (loop (rdc specifier))))))))
  )
