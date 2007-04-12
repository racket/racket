(module auto-language mzscheme
  (require (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (provide pick-new-language)
  
  (define (pick-new-language text module-spec->language module-language)
    (with-handlers ((exn:fail:read? (λ (x) #f)))
      (let ([found-language? #f])
        (let* ([tp (open-input-text-editor text)]
               [l (with-handlers ([exn:fail:contract? (λ (x) eof)])
                    ;; catch exceptions that occur with GUI syntax in the beginning of the buffer
                    (read-line tp))])
          (unless (eof-object? l)
            (unless (regexp-match #rx"[;#]" l) ;; no comments on the first line
              (when (equal? #\) (send text get-character (- (send text last-position) 1)))
                (let ([sp (open-input-string l)])
                  (when (regexp-match #rx"[(]" sp)
                    (let-values ([(mod name module-spec)
                                  (values (parameterize ([read-accept-reader #f]) (read sp))
                                          (parameterize ([read-accept-reader #f]) (read sp))
                                          (parameterize ([read-accept-reader #f]) (read sp)))])
                      (when (eq? mod 'module)
                        (let ([matching-language (module-spec->language module-spec)])
                          (when matching-language
                            (send text delete (- (send text last-position) 1) (send text last-position))
                            (send text delete 
                                  (send text paragraph-start-position 0)
                                  (send text paragraph-start-position 1))
                            (set! found-language? matching-language)
                            (send text set-modified #f)))))))))))
        (unless found-language?
          (when module-language
            (let* ([tp (open-input-text-editor text 0 'end (lambda (s) s) text #t)]
                   [r1 (parameterize ([read-accept-reader #f]) (read tp))]
                   [r2 (parameterize ([read-accept-reader #f]) (read tp))])
              (when (and (eof-object? r2)
                         (pair? r1)
                         (eq? (car r1) 'module))
                (set! found-language? module-language)
                (send text set-modified #f)))))
        
        found-language?))))
