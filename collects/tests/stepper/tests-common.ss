(module tests-common mzscheme
  
  (require (lib "private/annotate.ss" "stepper")
           (lib "contract.ss")
           (lib "shared.ss" "stepper" "private"))
  
  (provide/contract (reset-namespaces (-> void?))
                    (annotate-exprs (-> (listof syntax?) break-contract (listof syntax?)))
                    (string->stx-list (-> string? (listof syntax?))))
  
  (provide mz-namespace
           beginner-namespace
           beginner-wla-namespace
           intermediate-namespace
           intermediate/lambda-namespace)
  
  ; : ((listof syntax?) (recon-result recon-result -> (void)) -> (listof syntax)
  (define (annotate-exprs stx-list break)
    (let loop ([env (make-initial-env-package)] [stx-list stx-list])
      (if (null? stx-list)
          null
          (let*-values ([(annotated new-env)
                         (annotate (car stx-list) env break 'foot-wrap)])
            (cons annotated (loop new-env (cdr stx-list)))))))
 
  ; : (string -> (listof syntax)
  (define (string->stx-list stx)
    (let ([port (open-input-string stx)])
      (let loop ([first-stx (read-syntax 'test-program port)])
        (if (eof-object? first-stx)
            null
            (cons first-stx (loop (read-syntax 'test-program port)))))))

  (define source-namespace (current-namespace))
  
  (define mz-namespace #f)
  (define beginner-namespace #f)
  (define beginner-wla-namespace #f)
  (define intermediate-namespace #f)
  (define intermediate/lambda-namespace #f)
  
  (define (new-namespace-from-spec spec)
    (let ([new-namespace (make-namespace 'empty)])
      (parameterize ([current-namespace new-namespace])
        (namespace-attach-module source-namespace 'mzscheme)
        (namespace-require spec))
      new-namespace))
  
  (define (reset-namespaces)
    (set! mz-namespace (new-namespace-from-spec '(lib "plt-mzscheme.ss" "lang")))
    (set! beginner-namespace (new-namespace-from-spec '(lib "htdp-beginner.ss" "lang")))
    (set! beginner-wla-namespace (new-namespace-from-spec '(lib "htdp-beginner-abbr.ss" "lang")))
    (set! intermediate-namespace (new-namespace-from-spec '(lib "htdp-intermediate.ss" "lang")))
    (set! intermediate/lambda-namespace (new-namespace-from-spec '(lib "htdp-intermediate-lambda.ss" "lang")))))
