(module repl '#%kernel
  (#%declare #:cross-phase-persistent)
  (#%provide read-eval-print-loop)

  (define-values (read-eval-print-loop)
    (lambda ()
      (letrec-values ([(repl-loop)
                       (lambda ()
                         ;; This prompt catches all error escapes, including from read and print.
                         (call-with-continuation-prompt
                          (lambda ()
                            (let-values ([(v) ((current-prompt-read))])
                              (if (eof-object? v)
                                  (void)
                                  (begin
                                    (call-with-values
                                     (lambda () 
                                       ;; This prompt catches escapes during evaluation.
                                       ;; Unlike the outer prompt, the handler prints
                                       ;; the results.
                                       (call-with-continuation-prompt
                                        (lambda ()
                                          (let-values ([(w) (cons '#%top-interaction v)])
                                            ((current-eval) (if (syntax? v)
                                                                (namespace-syntax-introduce 
                                                                 (datum->syntax #f w v))
                                                                w))))))
                                     (lambda results (for-each (current-print) results)))
                                    ;; Abort to loop. (Calling `repl-loop` directly would not be a tail call.)
                                    (abort-current-continuation (default-continuation-prompt-tag))))))
                          (default-continuation-prompt-tag)
                          (lambda args (repl-loop))))])
        (repl-loop)))))
