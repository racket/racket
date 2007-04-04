(module combinator-parser (lib "lazy.ss" "lazy")

  (require (lib "unit.ss")
           (lib "lex.ss" "parser-tools"))
    
  (require "structs.scm" "parser-sigs.ss" "combinator.scm" "errors.scm")
  
  (provide combinator-parser-tools@)
  
  (define-unit main-parser@
    (import error^ out^ error-format-parameters^ language-format-parameters^)
    (export parser^)
    
    (define (sort-used reses)
      (sort reses (lambda (a b) (> (res-used a) (res-used b)))))
    
    (define (parser start)
      (lambda (input file)
        (let* ([result (start input)]
               [out 
                (cond
                  [(and (res? result) (res-a result) (null? (res-rest result)))
                   (car (res-a (!!! result)))]
                  [(and (res? result) (res-a result) (res-possible-error result))
                   (printf "res fail~n")
                   (fail-type->message (!!! (res-possible-error result)))]
                  [(and (res? result) (res-a result))
                   (make-err
                    (format "Found extraneous input after ~a, starting with ~a, at the end of ~a."
                            (!!! (res-msg result)) 
                            (input->output-name (!!! (car (res-rest result)))) input-type)
                    (and src? 
                         (make-src-lst (position-token-start-pos (!!! (car (res-rest result)))))))]
                  [(res? result) 
                   (printf "res fail2~n")
                   (fail-type->message (res-msg (!!! result)))]
                  [(or (choice-res? result) (pair? result))
                   (let* ([options (if (choice-res? result) (choice-res-matches result) result)]
                          [finished-options (filter (lambda (o) 
                                                      (cond [(res? o) (null? (res-rest o))]
                                                            [(repeat-res? o) 
                                                             (eq? (repeat-res-stop o) 'out-of-input)]))
                                                    options)]
                          [possible-errors (filter res-possible-error 
                                                   (map (lambda (a) (if (repeat-res? a) (repeat-res-a a) a))
                                                        options))])
                     (cond 
                       [(not (null? finished-options)) (car (res-a (!!! (car finished-options))))]
                       [(not (null? possible-errors))
                        (printf "choice or pair fail~n")
                        (!!! (fail-type->message
                              (res-possible-error (!!! (car (sort-used possible-errors))))))]                
                       [else
                        (let ([used-sort (sort-used options)])
                          (make-err
                           (format "Found additional content after ~a, begining with ~a." 
                                   (!!! (res-msg (car used-sort)))
                                   (input->output-name (!!! (car (res-rest (car used-sort))))))
                           (and src?
                                (make-src-lst (position-token-start-pos 
                                               (!!! (car (res-rest (car used-sort)))))))))]))]
                  [(and (repeat-res? result) (eq? 'out-of-input (repeat-res-stop (!!! result))))
                   (res-a (repeat-res-a result))]
                  [(and (repeat-res? result) (fail-type? (repeat-res-stop (!!! result))))
                   (printf "repeat-fail~n")
                   (!!! (fail-type->message (!!! (repeat-res-stop (!!! result)))))]
                  [else (error 'parser (format "Internal error: recieved unexpected input ~a" 
                                               (!!! result)))])])
          (cond
            [(err? out)
             (make-err (!!! (err-msg out))
                       (list (!!! file) 
                             (!!! (first (err-src out)))
                             (!!! (second (err-src out)))
                             (!!! (third (err-src out)))
                             (!!! (fourth (err-src out)))))]
            [else (!!! out)]))))
    )
  
  (define-unit rank-defaults@
    (import)
    (export ranking-parameters^)
    (define (rank-choice choices) (apply max choices))
    (define-values 
      (rank-misspell rank-caps rank-class rank-wrong rank-end)
      (4/5 9/10 2/5 1/5 2/5)))
  
  (define-unit out-struct@
    (import)
    (export out^)
    (define-struct err (msg src)))
  
  (define-compound-unit/infer combinator-parser@
    (import error-format-parameters^ language-format-parameters^ language-dictionary^)
    (export combinator-parser-forms^ parser^ out^)
    (link out-struct@ main-parser@ rank-defaults@ error-formatting@ combinators@))
  
  (define-unit/new-import-export combinator-parser-tools@
    (import error-format-parameters^ language-format-parameters^ language-dictionary^)
    (export combinator-parser^ err^)
    ((combinator-parser-forms^ parser^ out^) combinator-parser@ error-format-parameters^ language-format-parameters^
                            language-dictionary^))
           
  )