#lang racket/base
(require (for-syntax racket/base
                     racket/syntax
                     syntax/strip-context
                     syntax/keyword))
(provide phase1-eval
         convert-syntax-error
         convert-compile-time-error)

(begin-for-syntax
 (define (exn->raise-syntax e)
   (cond [(exn:fail:syntax? e)
          #`(raise (make-exn:fail:syntax
                    #,(exn-message e)
                    (current-continuation-marks)
                    ;; Lexical context must be stripped to avoid "unsealed local-definition context
                    ;; found in fully expanded form" error in cases like the following:
                    ;;   (convert-syntax-error (let () (define x) x))
                    #,(with-syntax ([(expr ...) (map strip-context (exn:fail:syntax-exprs e))])
                        #'(list (quote-syntax expr) ...))))]
         [(exn? e)
          (with-syntax ([make-exn
                         (cond [(exn:fail? e) #'make-exn:fail]
                               [else #'make-exn])])
            #`(raise (make-exn #,(exn-message e)
                               (current-continuation-marks))))]
         [else
          #`(raise (make-exn #,(format "non-exception value raised: ~e" e)
                             (current-continuation-marks)))])))

(define-syntax (phase1-eval stx)
  (if (eq? (syntax-local-context) 'expression)
      (syntax-case stx ()
        [(phase1-eval ct-expr . options)
         (let ()
           (define opts (parse-keyword-options/eol
                         #'options
                         `((#:quote  ,check-identifier)
                           (#:catch? ,check-stx-boolean))
                         #:no-duplicates? #t
                         #:context stx))
           (define quote-form (options-select-value opts '#:quote #:default #'quote))
           (define catch? (options-select-value opts '#:catch? #:default #t))
           (with-handlers ([(lambda (e) catch?) exn->raise-syntax])
             (with-syntax ([quote quote-form]
                           [result (syntax-local-eval #'ct-expr)])
               #'(quote result)))
           #|
           ;; Alternative version
           (with-syntax ([quote-form quote-form]
                         [catch? catch?])
             #'(let-syntax ([aux-macro
                             (lambda _
                               (with-handlers ([(lambda (e) catch?) exn->raise-syntax])
                                 (with-syntax ([result ct-expr])
                                   ;; want syntax-local-introduce ?
                                   #'(quote-form result))))])
                 (aux-macro)))
           |#)])
      #`(#%expression #,stx)))

(begin-for-syntax
 (define (do-convert-ct-error stx exn-pred?)
   (if (eq? (syntax-local-context) 'expression)
       (syntax-case stx ()
         [(_ expr)
          (with-handlers ([exn-pred? exn->raise-syntax]
                          [void
                           (lambda (e)
                             (eprintf "didn't catch ~e\n" e)
                             (raise e))])
            (local-expand #'expr 'expression null))])
       #`(#%expression #,stx))))

(define-syntax (convert-syntax-error stx)
  (parameterize ((error-print-source-location #f))
    (do-convert-ct-error stx exn:fail:syntax?)))

(define-syntax (convert-compile-time-error stx)
  (do-convert-ct-error stx (lambda (e) #t)))
