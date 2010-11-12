(module combinator-parser scheme/base

  (require scheme/list
           scheme/unit
           parser-tools/lex)
  (require "structs.scm" "parser-sigs.ss" "combinator.scm" "errors.scm")

  (provide combinator-parser-tools@)
  
  (define-unit main-parser@
    (import error^ out^ error-format-parameters^ language-format-parameters^ ranking-parameters^)
    (export parser^)
    
    (define (sort-used reses)
      (sort reses
            (lambda (a b) (> (res-used a) (res-used b)))))
    (define (sort-repeats repeats)
      (sort repeats
            (lambda (a b) (> (res-used (repeat-res-a a))
                             (res-used (repeat-res-a b))))))
    
    (define (parser start)
      (lambda (input file)
        (let* ([first-src (and src? (pair? input) 
                               (make-src-lst (position-token-start-pos (car input))
                                             (position-token-end-pos (car input))))]
               [result (if first-src (start input first-src) (start input))]
               [out 
                (cond
                  [(and (res? result) (res-a result) (null? (res-rest result)))
                   (car (res-a result))]
                  [(and (res? result) (res-a result) (res-possible-error result))
                   (fail-type->message (res-possible-error result))]
                  [(and (res? result) (res-a result))
                   (make-err
                    (format "Found extraneous input after ~a, starting with ~a, at the end of ~a."
                            (res-msg result) 
                            (input->output-name (car (res-rest result))) input-type)
                    (and src? 
                         (make-src-lst (position-token-start-pos (car (res-rest result)))
                                       (position-token-end-pos (car (res-rest result))))))]
                  [(res? result) 
                   (fail-type->message (res-msg result))]
                  [(lazy-opts? result)
                   #;(printf "lazy-opts ~a\n" result)
                   (let* ([finished? (lambda (o)
                                       (cond [(res? o)
                                              (and (not (null? (res-a o)))
                                                   (null? (res-rest o)))]
                                             [(repeat-res? o)
                                              (eq? (repeat-res-stop o) 'out-of-input)]
                                             [else #f]))]
                          [possible-errors 
                           (lambda (matches)
                             (map (lambda (r)
                                    (or (and (res? r) (res-possible-error r))
                                        (and (repeat-res? r) (repeat-res-stop r))))
                                  (filter (lambda (r)
                                            (or (and (res? r) (res-possible-error r))
                                                (and (repeat-res? r) (fail-type? (repeat-res-stop r)))))
                                          matches)))]
                          [result-a
                           (lambda (res)
                             (cond 
                               [(res? res) (res-a res)]
                               [(and (repeat-res? res)
                                     (res? (repeat-res-a res)))
                                (res-a (repeat-res-a res))]
                               [else
                                (error 'parser-internal-errorcl (format "~a" res))]))])
                     (let loop ([matched (lazy-opts-matches result)])
                       (cond
                         [(and (pair? matched) (finished? (car matched))) (result-a (car matched))]
                         [(pair? matched) (loop (cdr matched))]
                         [(and matched (finished? matched)) (result-a matched)]
                         [(or (null? matched) matched) (loop (next-opt result))]
                         [else
                          (let ([p-errors (possible-errors (lazy-opts-matches result))])
                            (cond
                              [(pair? p-errors)
                               (let ([fails (cons (lazy-opts-errors result) p-errors)])
                                 #;(printf "\nfails ~a\n\n" fails)
                                 (fail-type->message
                                  (make-options-fail (rank-choice (map fail-type-chance fails))
                                                     #f
                                                     (if (lazy-choice? result) 
                                                         (lazy-choice-name result) "program")
                                                     (rank-choice (map fail-type-used fails))
                                                     (rank-choice (map fail-type-may-use fails))
                                                     fails)))]
                              [(null? p-errors)
                               (fail-type->message (lazy-opts-errors result))]))])))]
                  [(or (choice-res? result) (pair? result))
                   #;(printf "choice-res or pair? ~a\n" result)
                   (let* ([options (if (choice-res? result) (choice-res-matches result) result)]
                          [finished-options (filter (lambda (o) 
                                                      (cond [(res? o) 
                                                             (and (not (null? (res-a o)))
                                                                  (null? (res-rest o)))]
                                                            [(repeat-res? o) 
                                                             (eq? (repeat-res-stop o) 'out-of-input)]))
                                                    options)]
                          [possible-repeat-errors
                           (filter (lambda (r) (and (repeat-res? r)
                                                    (fail-type? (repeat-res-stop r))))
                                   options)]
                          [possible-errors 
                           (filter res-possible-error 
                                   (map (lambda (a) (if (repeat-res? a) (repeat-res-a a) a))
                                        options))])
                     #;(printf "length finished-options ~a\n" finished-options)
                     (cond 
                       [(not (null? finished-options))
                        #;(printf "finished an option\n")
                        (let ([first-fo (car finished-options)])
                          (car (cond 
                                 [(res? first-fo) (res-a first-fo)]
                                 [(and (repeat-res? first-fo)
                                       (res? (repeat-res-a first-fo)))
                                  (res-a (repeat-res-a first-fo))]
                                 [else
                                  (error 'parser-internal-errorcp 
                                         (format "~a" first-fo))])))]
                       #;[(not (null? possible-repeat-errors))
                        (printf "possible-repeat error\n")
                        (fail-type->message 
                         (car (repeat-res-stop 
                               (sort-repeats possible-repeat-errors))))]
                       [(and (choice-res? result) (fail-type? (choice-res-errors result)))
                        #;(printf "choice res and choice res errors \n")
                        (cond
                          [(and (null? possible-repeat-errors)
                                (null? possible-errors)) (fail-type->message (choice-res-errors result))]
                          [(or #;(not (null? possible-repeat-errors))
                               (not (null? possible-errors)))
                           (let ([fails (cons (choice-res-errors result) 
                                              (map res-possible-error possible-errors))])
                             (fail-type->message
                              (make-options-fail (rank-choice (map fail-type-chance fails))
                                                 #f
                                                 (choice-res-name result)
                                                 (rank-choice (map fail-type-used fails))
                                                 (rank-choice (map fail-type-may-use fails))
                                                 fails)))])]
                       [(not (null? possible-errors))
                        ;(printf "choice or pair fail\n")
                        (fail-type->message
                         (res-possible-error (car (sort-used possible-errors))))]
                       [else
                        #;(printf "result ~a\n" result)
                        (let ([used-sort (sort-used options)])
                          (if (and (choice-res? result)
                                   (choice-res-errors result))
                              (fail-type->message (choice-res-errors result))
                              (make-err
                               (format "Found additional content after ~a, beginning with '~a'." 
                                       (res-msg (car used-sort))
                                       (input->output-name (car (res-rest (car used-sort)))))
                               (and src?
                                    (make-src-lst (position-token-start-pos 
                                                   (car (res-rest (car used-sort))))
                                                  (position-token-end-pos
                                                   (car (res-rest (car used-sort)))))))))]))]
                  [(and (repeat-res? result) (eq? 'out-of-input (repeat-res-stop result)))
                   (res-a (repeat-res-a result))]
                  [(and (repeat-res? result) (fail-type? (repeat-res-stop result)))
                   ;(printf "repeat-fail\n")
                   (fail-type->message (repeat-res-stop result))]
                  [else (error 'parser (format "Internal error: received unexpected input ~a" 
                                               result))])])
          (cond
            [(err? out)
             (make-err (err-msg out)
                       (if (err-src out)
                           (list file 
                                 (first (err-src out))
                                 (second (err-src out))
                                 (third (err-src out))
                                 (fourth (err-src out)))
                           (list file 1 0 1 0)))]
            [else out]))))
    )
  
  #;(define-unit rank-defaults@
    (import)
    (export ranking-parameters^)
    (define (rank-choice choices) (apply max choices))
    (define-values 
      (rank-misspell rank-caps rank-class rank-wrong rank-end)
      (values 4/5 9/10 2/5 1/5 2/5)))
  
  (define-unit rank-defaults@
    (import)
    (export ranking-parameters^)
    (define (rank-choice choices) (apply max choices))
    (define-values 
      (rank-misspell rank-caps rank-class rank-wrong rank-end rank-repeat)
      (values 16/71 18/71 8/71 4/71 8/71 17/71)))
 
  
  (define-unit out-struct@
    (import)
    (export out^)
    (define-struct err (msg src) #:mutable))
  
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
