#lang scheme/base
(require scribble/base
         scribble/core
         scribble/manual
         scribble/scheme
         (for-syntax scheme/base)
         "image-toc.ss")

(provide image-examples
         exp->filename)

(define-syntax (image-examples stx)
  (syntax-case stx ()
    [(_ exp ...)
     (when (getenv "PLTSHOWIMAGES")
       (for-each (λ (exp) (printf "~s\n" (syntax->datum exp)))
                 (syntax->list #'(exp ...))))
     #'(interleave 
         (list (schemeinput exp) ...)
         (list 'exp ...))]))

(define (interleave expr-paras val-list+outputs)
  (make-table
   plain
   (map list
        (apply append
               (list (make-paragraph plain (format "Example~a:"
                                                   (if (or (null? expr-paras)
                                                           (null? (cdr expr-paras)))
                                                       ""
                                                       "s"))))
               (map (λ (x exp) 
                      (list x 
                            (let ([fn (format "2htdp/scribblings/img/~a" (exp->filename exp))])
                              (if (file-exists? fn)
                                  (schemeblock #,(image fn))
                                  (make-paragraph
                                   error-color 
                                   (format "missing image! ~a" (exp->filename exp)))))))
                    expr-paras
                    val-list+outputs)))))

(define (exp->filename exp) 
  (let ([fn (assoc exp mapping)])
    (cond 
      [fn 
       (cadr fn)]
      [else
       (fprintf (current-error-port) "exp->filename: unknown exp ~s\n" exp)
       "unk.png"])))
