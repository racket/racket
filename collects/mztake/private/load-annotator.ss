;catch oops exception
;catch the other two exceptions that my loaders throw
;detect if the source code for a certain module is missing and throw an error
;do I want to parameterize it over a given namespace?

(module load-annotator mzscheme
  
  (require (lib "moddep.ss" "syntax")
           (lib "class.ss" "mzlib")
           (lib "mred.ss" "mred"))  
  
  (provide require/annotations
           load/annotate)
  
  #|load-with-annotations :

    >initial-module : (union (listof symbol?) string?)
        Takes in a require spec -- "../file.ss", (file "complete-path.ss"), (lib ...), etc
        In other words -
        pass it a relative filename or a quoted lib to require
        "mztake.ss" or '(lib "mztake.ss" "mztake")

    >annotate-module? : (string? symbol? . -> . boolean)
                        (filename module-name)
        If true, loads source file and annotates.
        Else, tries to load compiled or source, no annotation.

    >annotator : (string? symbol? syntax? . -> . syntax?)
|#
  (define (require/annotations initial-module annotate-module? annotator)
    (parameterize
        ([current-load/use-compiled
          (let ([ocload/use-compiled (current-load/use-compiled)])
            (lambda (fn m)
              (with-handlers
                  ([exn:module?
                    (lambda (exn)
                      (raise (format "mztake:client:not-a-module: file:`~a' module:`~a'" fn m)))])
                
                (cond [(annotate-module? fn m)
                       (load/annotate annotator fn m)]
                      [else
                       (ocload/use-compiled fn m)]))))])
      (eval #`(require #,initial-module))))
  
  (define (load/annotate annotator fn m)
    (let-values ([(base _ __) (split-path fn)]
                 [(in-port src) (build-input-port fn)])
      (dynamic-wind
       (lambda () (void))
       
       (lambda ()
         (parameterize ([read-accept-compiled #f]
                        [current-load-relative-directory base])
           (unless m (raise 'module-name-not-passed-to-load/annotate))
           (with-module-reading-parameterization
            (lambda ()
              (let* ([first (expand (read-syntax src in-port))]
                     [module-ized-exp (annotator fn m (check-module-form first m fn))]
                     [second (read in-port)])
                (unless (eof-object? second)
                  (raise-syntax-error
                   'load/annotate
                   (format "expected only a `module' declaration for `~s', but found an extra expression" m)
                   second))
                (eval module-ized-exp))))))
       
       (lambda () (close-input-port in-port)))))
  
  
  
  ; taken directly from mred.ss -- it's not exported...
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
        (let loop ()
          (when (with-handlers ([not-break-exn? (lambda (x) #f)])
                  (regexp-match-peek "^#!" p))
            (let lloop ([prev #f])
              (let ([c (read-char-or-special p)])
                (if (or (eof-object? c)
                        (eq? c #\return)
                        (eq? c #\newline))
                    (when (eq? prev #\\)
                      (loop))
                    (lloop c))))))
        (values p filename))))
  
  
  (define (test annotate-all?)
    (require/annotations '(lib "mztake.ss" "mztake")
                         (lambda (fn m)
                           (printf "~a ~a~n" fn m)
                           annotate-all?)
                         (lambda (fn m stx) stx)))
  ;(test #t) ; slow
  ;(test #f) ; fast
  )