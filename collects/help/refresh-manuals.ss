(module refresh-manuals mzscheme
  (require "private/docpos.ss"
           "private/search.ss"
           "private/manuals.ss"
           "private/standard-urls.ss"
           "private/link.ss"
           (lib "plt-installer.ss" "setup")
           (lib "url.ss" "net")
           (lib "mred.ss" "mred")
           (lib "string-constant.ss" "string-constants")
           (lib "contract.ss")
           (lib "port.ss")
           (lib "thread.ss"))
  
  (provide refresh-manuals 
           bytes-to-path)
  
  
  (define sc-refreshing-manuals (string-constant plt:hd:refreshing-manuals))
  (define sc-refresh-downloading... (string-constant plt:hd:refresh-downloading...))
  (define sc-refresh-deleting... (string-constant plt:hd:refresh-deleting...))
  (define sc-refresh-installing... (string-constant plt:hd:refresh-installing...))
  (define sc-finished-installation (string-constant plt:hd:refreshing-manuals-finished))
  (define sc-clearing-cached-indicies (string-constant plt:hd:refresh-clearing-indicies))
  
  (define refresh-manuals
    (case-lambda
      [() (refresh-manuals known-docs)]
      [(docs-to-install)
       (unless (and (list? docs-to-install)
                    (andmap (lambda (x) (and (pair? x)
                                             (path? (car x))
                                             (string? (cdr x))))
                            docs-to-install))
         (error 'refresh-manuals "expected (listof (cons path string)) as argument, got ~e" docs-to-install))
       (let ([tmp-directory (find/create-temporary-docs-dir)]
             [success? #f]
             [thd #f])
         (with-installer-window
          (lambda (parent)
            (set! thd (current-thread))
            (unless tmp-directory
              (error 'plt-installer "please clean out ~a" (find-system-path 'temp-dir)))
            (let ([docs-error (download-docs docs-to-install tmp-directory)])
              (cond
                [docs-error
                 (printf "~a\n" docs-error)]
                [else
                 (delete-docs docs-to-install)
                 (install-docs docs-to-install tmp-directory parent)
                 (delete-local-plt-files tmp-directory)
                 (display sc-clearing-cached-indicies)
                 (newline)
                 
                 ;; tell the web-server to visit the url for flushing the cache
                 ;; this is necc. because the server creates a new namespace for
                 ;; each servlet, so we have to get the webserver to visit the servlet
                 ;; in order to flush the cache. We don't, however, want to actually
                 ;; visit the page, so we just do this for its effect.
                 (let-values ([(in1 out1) (make-pipe)]
                              [(in2 out2) (make-pipe)])
                   (thread (lambda () 
                             (fprintf out1 "GET ~a HTTP/1.0\r\n" flush-manuals-path)
                             (close-output-port out1)))
                   (serve-ports in1 out2) ;; spawns its own thread
                   (let loop ()
                     (let ([b (with-handlers ([exn? (lambda (x) eof)])
                                (read-byte in2))])
                       (unless (eof-object? b)
                         (loop))))
                   (close-input-port in2))])
              
              (display sc-finished-installation)
              (newline)
              (set! success? #t)))
          (lambda ()
            (unless success?
              (delete-local-plt-files tmp-directory))
            (kill-thread thd))))]))
  
  ; needed in "../private/manuals.ss" due to links with > getting mangled
  (define bytes-to-path bytes->path)
      
  (define (make-local-doc-filename tmp-dir stub)
    (build-path tmp-dir (format "~a-doc.plt" stub)))

  ;; if cannot find a suitable directory, #f is returned
  ;; if okay, returns the path to the directory.
  (define find/create-temporary-docs-dir
    ;(-> (union string? false?))
    (lambda ()
      (let ([temp-dir (find-system-path 'temp-dir)])
        (let loop ([n 0])
          (if (= n 30)
              #f
              (let ([candidate (build-path temp-dir (format "help-refresh-docs~a" n))])
                (if (directory-exists? candidate)
                    (loop (+ n 1))
                    (begin
                      (make-directory candidate)
                      candidate))))))))
                  

      


                                                        
    ;;                       ;;;                     ;; 
     ;                         ;                      ; 
     ;                         ;                      ; 
  ;;;;   ;;;  ;;; ;;;; ;;;     ;     ;;;   ;;;;    ;;;; 
 ;   ;  ;   ;  ;   ;  ;;  ;    ;    ;   ;      ;  ;   ; 
 ;   ;  ;   ;  ; ; ;  ;   ;    ;    ;   ;   ;;;;  ;   ; 
 ;   ;  ;   ;  ; ; ;  ;   ;    ;    ;   ;  ;   ;  ;   ; 
 ;   ;  ;   ;   ; ;   ;   ;    ;    ;   ;  ;   ;  ;   ; 
  ;;; ;  ;;;    ; ;  ;;;  ;; ;;;;;;  ;;;    ;;; ;  ;;; ;
                                                        
                                                        
                                                        

  ;; download-docs : ... -> (union #f string)
  ;; downloads the docs to the tmp-dir
  (define download-docs
    (lambda (docs-to-install tmp-dir)
      (let loop ([known-docs docs-to-install])
        (cond
          [(null? known-docs) #f]
          [else (let* ([known-doc (car known-docs)]
                       [resp (download-doc tmp-dir (car known-doc) (cdr known-doc))])
                  (if (string? resp)
                      resp
                      (loop (cdr known-docs))))]))))
      
  ;; download-doc : ... -> (union #f string)
  ;; stub is the `drscheme' portion of `drscheme-doc.plt'.
  (define download-doc
    (lambda (tmp-dir stub full-name)
      (let ([url (make-docs-plt-url (path->string stub))]
            [doc-name (make-local-doc-filename tmp-dir stub)])
        (display (format sc-refresh-downloading... full-name))
        (newline)
        (call-with-output-file doc-name
          (lambda (out-port)
            (call/input-url (string->url url) 
                            get-impure-port 
                            (lambda (in-port)
                              (let/ec k
                                (let* ([resp (purify-port in-port)]
                                       [m (regexp-match #rx"HTTP/[^ ]* ([0-9]+)([^\r\n]*)" resp)])
                                  (unless m
                                    (k "malformed response from server ~s" resp))
                                  (let ([code (string->number (cadr m))])
                                    (unless (equal? code 200)
                                      (k (format "error response from server \"~a~a\"" code (caddr m)))))
                                  (copy-port in-port out-port)
                                  #f)))))))))
      
      
                                          
    ;;         ;;;                        
     ;           ;            ;           
     ;           ;            ;           
  ;;;;   ;;;     ;     ;;;   ;;;;;   ;;;  
 ;   ;  ;   ;    ;    ;   ;   ;     ;   ; 
 ;   ;  ;;;;;    ;    ;;;;;   ;     ;;;;; 
 ;   ;  ;        ;    ;       ;     ;     
 ;   ;  ;   ;    ;    ;   ;   ;   ; ;   ; 
  ;;; ;  ;;;   ;;;;;;  ;;;     ;;;   ;;;  
                                          
                                          
                                      
  (define delete-docs
    (lambda (docs)
      (for-each (lambda (known-doc) (delete-known-doc (car known-doc) (cdr known-doc)))
                docs)))
      
  (define delete-known-doc
    (lambda (doc full-name)
      (let ([doc-dir (find-doc-directory doc)])
        (when doc-dir
          (display (format sc-refresh-deleting... full-name))
          (newline)
	  (with-handlers ([exn:fail:filesystem?
			   (lambda (exn)
			     (fprintf (current-error-port)
				      "Warning: delete failed: ~a\n"
				      (exn-message exn)))])
	    (delete-directory/r doc-dir))))))
      
  (define delete-local-plt-files
    (lambda (tmp-dir)
      (delete-directory/r tmp-dir)))
      
  ;; deletes the entire subtree underneath this directory
  ;; (including the dir itself)
  (define delete-directory/r 
    (lambda (dir)
      (when (directory-exists? dir)
        (let loop ([dir dir])
          (let ([children (directory-list dir)])
            (for-each (lambda (f) (when (file-exists? (build-path dir f))
                                    (delete-file (build-path dir f))))
                      children)
            (for-each (lambda (d) (when (directory-exists? (build-path dir d))
                                    (loop (build-path dir d))))
                      children)
            (delete-directory dir))))))

      
                                                 
   ;                                ;;;    ;;;   
                       ;              ;      ;   
                       ;              ;      ;   
 ;;;   ; ;;;    ;;;   ;;;;;  ;;;;     ;      ;   
   ;    ;;  ;  ;   ;   ;         ;    ;      ;   
   ;    ;   ;   ;;;    ;      ;;;;    ;      ;   
   ;    ;   ;      ;   ;     ;   ;    ;      ;   
   ;    ;   ;  ;   ;   ;   ; ;   ;    ;      ;   
 ;;;;; ;;;  ;;  ;;;     ;;;   ;;; ; ;;;;;; ;;;;;;
                                                 
                                                 
    
  (define install-docs
    (lambda (docs-to-install tmp-dir parent)
      (for-each (lambda (pr) 
                  (display (format sc-refresh-installing... (cdr pr)))
                  (newline)
                  (run-single-installer (make-local-doc-filename tmp-dir (car pr))
					(lambda ()
					  (error 'install-docs
						 "expected PLT-relative archive"))))
                docs-to-install))))
