#cs(module aux-syntax mzscheme 
     
     (provide 
      prefix-id-suffix ; String Identifier String -> Identifier 
      add?             ; Identifier -> Identifier 
      cap-id           ; Identifier -> Identifier 
      )
     
     (require (lib "string.ss"))
     
     (define (prefix-id-suffix prefix e suffix)
       (datum->syntax-object
        e (string->symbol (format "~a~a~a" prefix (syntax-e e) suffix))))
     
     ;; to add a ? at the end of an identifier
     (define (add? e) (prefix-id-suffix "" e "?"))
     
     (define (cap-id id-e)
       (let* ([id-s (symbol->string (syntax-e id-e))]
              [fst (substring id-s 0 1)]
              [rst (substring id-s 1 (string-length id-s))])
         (string-uppercase! fst)
         (datum->syntax-object id-e (string->symbol (string-append fst rst)))))
     
     #| Tests: 
     (define e (datum->syntax-object #f 'e))
     (printf "~s~n" (eq? 'set-e! (syntax-e (prefix-id-suffix "set-" e "!"))))
     (printf "~s~n" (eq? 'e? (syntax-e (add? e))))
     (printf "~s~n" (eq? 'Hello (syntax-e (cap-id (datum->syntax-object #f 'hello)))))
     |#
     
     )
