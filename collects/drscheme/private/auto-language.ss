(module auto-language mzscheme
  (require mred
           mzlib/class)
  
  (provide pick-new-language looks-like-module?)
  
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
                       (send text while-unlocked
                             (λ () 
                               (send text delete 0 (send text paragraph-start-position lines)))))))))))
         all-languages)
        
        ;; check to see if it looks like the module language.
        (unless found-language?
          (when module-language
            (when (looks-like-module? text)
              (set! found-language? module-language)
              (set! settings module-language-settings))))
        
        (values found-language?
                settings))))
  
  (define (looks-like-module? text)
    (or (looks-like-new-module-style? text)
        (looks-like-old-module-style? text)))
  
  (define (looks-like-old-module-style? text)
    (with-handlers ((exn:fail:read? (λ (x) #f)))
      (let* ([tp (open-input-text-editor text 0 'end (lambda (s) s) text #t)]
             [r1 (parameterize ([read-accept-reader #f]) (read tp))]
             [r2 (parameterize ([read-accept-reader #f]) (read tp))])
        (and (eof-object? r2)
             (pair? r1)
             (eq? (car r1) 'module)))))
  
  (define (looks-like-new-module-style? text)
    (let* ([tp (open-input-text-editor text 0 'end (lambda (s) s) text #t)]
           [l1 (with-handlers ([exn:fail? (lambda (exn) eof)])
                 ;; If tp contains a snip, read-line fails.
                 (read-line tp))])
      (and (string? l1)
           (regexp-match #rx"#lang .*$" l1)))))
