(module auto-language mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide pick-new-language)
  
  (define reader-tag "#reader")
  
  (define (pick-new-language text all-languages module-language module-language-settings)
    (with-handlers ([exn:fail:read? (λ (x) (values #f #f))])
      (let ([found-language? #f]
            [settings #f])
        
        (for-each
         (λ (lang)
           (let ([lang-spec (send lang get-reader-module)])
             (when lang-spec
               (let* ([lines (send lang get-metadata-lines)]
                      [str (send text get-text 
                                 0
                                 (send text paragraph-end-position (- lines 1)))]
                      [sp (open-input-string str)])
                 (when (regexp-match #rx"#reader" sp)
                   (let ([spec-in-file (read sp)])
                     (when (equal? lang-spec spec-in-file)
                       (set! found-language? lang)
                       (set! settings (send lang metadata->settings str))
                       (send text delete 0 (send text paragraph-start-position lines)))))))))
         all-languages)
                    
        ;; check to see if it looks like the module language.
        (unless found-language?
          (when module-language
            (let* ([tp (open-input-text-editor text 0 'end (lambda (s) s) text #t)]
                   [r1 (parameterize ([read-accept-reader #f]) (read tp))]
                   [r2 (parameterize ([read-accept-reader #f]) (read tp))])
              (when (and (eof-object? r2)
                         (pair? r1)
                         (eq? (car r1) 'module))
                (set! found-language? module-language)
                (set! settings module-language-settings)))))
        
        (values found-language?
                settings)))))
