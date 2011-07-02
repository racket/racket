(module testing-shared mzscheme
  (require mzlib/contract
           "shared.rkt"
           syntax/kerncase
           mzlib/file)
  
  (provide/contract [build-stx-with-highlight ((or/c (listof any/c) string?) ; input with one or more '(hilite ...) tags
                                               . -> .
                                               (listof syntax?))]) ; result
  
  (define (build-stx-with-highlight input)
    (let ([temp-file (make-temporary-file)])
      (call-with-output-file temp-file
        (lambda (port)
          (if (string? input)
              (display input port)
              (map (lambda (sexp) (write sexp port) (display #\space port)) input)))
        'truncate)
      (begin0
        (let ([file-port (open-input-file temp-file)])
          (let read-loop ([stx (read-syntax temp-file file-port)])
            (if (eof-object? stx)
                null
                (cons
                 (let stx-loop ([stx stx])
                   (syntax-case stx (hilite)
                     [(hilite x)
                      (stepper-syntax-property (stx-loop #`x) 'stepper-highlight #t)]
                     [(a . rest) (datum->syntax-object stx (cons (stx-loop #`a) (stx-loop #`rest)) stx stx)]
                     [else stx]))
                 (read-loop (read-syntax temp-file file-port))))))
        (delete-file temp-file))))
  
;  (require tests/utils/mz-testing
;           tests/utils/sexp-diff)
; (test `((define a 13) 14 15 #f 1)
;       map
;       syntax-object->datum
;       (build-stx-with-highlight `((define a 13)  14 15 #f 1)))
  
;      (let ([test-run (build-stx-with-highlight `((+ (hilite x) (hilite (+ (hilite 13) (a b))))))])
;        (test #t (lambda () (and (pair? test-run) (null? (cdr test-run)))))
;        (let ([test-stx (car test-run)])
;          (test `(+ x (+ 13 (a b)))
;                syntax-object->datum test-stx)
;          (test #f stepper-syntax-property test-stx 'stepper-highlight)
;          (test #t stepper-syntax-property (car (syntax-e (cdr (syntax-e test-stx)))) 'stepper-highlight)
;          (test #t stepper-syntax-property (syntax-case test-stx ()
;                                     [(+ x target)
;                                      #`target])
;                'stepper-highlight)
;          (test #t stepper-syntax-property (syntax-case test-stx (#%app)
;                                     [(+ x (a target d))
;                                      #`target])
;                'stepper-highlight)))
;  
;
;  
;   (let ([test-sexp `(+ (hilite x) (hilite (+ (hilite 13) (a b))))])
;     (test #t equal? test-sexp (syntax-object->hilite-datum (car (build-stx-with-highlight (list test-sexp))))))
  
  )
