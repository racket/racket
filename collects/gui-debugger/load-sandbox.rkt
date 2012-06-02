#lang mzscheme
  
  (require syntax/moddep
           mzlib/class
           racket/private/namespace
           mred)  
  
  (provide eval/annotations
           require/annotations
           require/sandbox+annotations
           load-module/annotate)

  ;; Like REQUIRE/ANNOTATIONS but the evaluation happens under the given
  ;; custodian and the given error display handler.
  (define (require/sandbox+annotations custodian err-display-handler initial-module annotate-module? annotator)
    (parameterize ([current-custodian custodian]
                   [current-namespace (make-gui-namespace)]
                   [error-display-handler err-display-handler])
      (require/annotations initial-module annotate-module? annotator)))


  ;; Like EVAL/ANNOTATION, but loads the required spec INITIAL-MODULE using EVAL.
  (define (require/annotations initial-module annotate-module? annotator)
    (eval/annotations #`(require #,initial-module) annotate-module? annotator))

  ;; Evaluates STX. For each module loaded during the evaluation,
  ;; ANNOTATE-MODULE? is queried, if it returns true, ANNOTATOR is ran on the
  ;; expanded module being loaded, and the return value is loaded instead.
  (define (eval/annotations stx annotate-module? annotator)
    (parameterize
      ([current-load/use-compiled
        (let ([ocload/use-compiled (current-load/use-compiled)])
          (lambda (fn m)
            (cond [(annotate-module? fn m)
                   (load-module/annotate annotator fn m)]
                  [else
                   (ocload/use-compiled fn m)])))])
      (eval-syntax (annotator stx))))
  
  ;; Loads the file FN expecting a definition for a module called M.  The
  ;; code read from the file is expanded, then it is annotated using the
  ;; ANNOTATOR function, then it is evaluated
  (define (load-module/annotate annotator fn m)
    (let-values ([(base _ __) (split-path fn)]
                 [(in-port src) (build-input-port fn)])
      (dynamic-wind
       (lambda () (void))
       
       (lambda ()
         (parameterize ([read-accept-compiled #f]
                        [current-load-relative-directory base])
           (unless m (raise 'module-name-not-passed-to-load-module/annotate))
           (with-module-reading-parameterization
            (lambda ()
              (let* ([first (parameterize ([current-namespace (make-base-namespace)])
                              (expand (read-syntax src in-port)))]
                     [module-ized-exp (annotator (check-module-form first m fn))]
                     [second (read in-port)])
                (unless (eof-object? second)
                  (raise-syntax-error
                   'load-module/annotate
                   (format "expected only a `module' declaration for `~s', but found an extra expression" m)
                   second))
                (eval module-ized-exp))))))
       
       (lambda () (close-input-port in-port)))))
  
  
  
  ; taken directly from mred.rkt -- it's not exported...
  (define (build-input-port filename)
    (let ([p (open-input-file filename)])
      (port-count-lines! p)
      (let ([p (cond [(regexp-match-peek "^WXME01[0-9][0-9] ## " p)
                      (let ([t (make-object text%)])
                        (send t insert-file p 'standard)
                        (close-input-port p)
                        (open-input-text-editor t))]
                     [else p])])
        (port-count-lines! p)
        (values p filename))))

