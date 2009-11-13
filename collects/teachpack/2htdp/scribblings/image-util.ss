#lang scheme/base
(require scribble/base
         scribble/core
         scribble/manual
         scribble/scheme
         (for-syntax scheme/base)
         "image-toc.ss")

(provide image-examples)

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
                            (let ([line (exp->line exp)])
                              (case (car line)
                                [(val)
                                 (schemeblock #,(schemeresult #,(cadr line)))]
                                [(image)
                                 (let ([fn (format "2htdp/scribblings/img/~a" (cadr line))])
                                   (schemeblock #,(image fn)))]
                                [(missing)
                                 (make-paragraph
                                  error-color 
                                  "missing result; need to re-run image-gen.ss")]))))
                    expr-paras
                    val-list+outputs)))))

(define (exp->line exp)
  (let ([fn (assoc exp mapping)])
    (cond 
      [fn (cdr fn)]
      [else
       (unless (getenv "PLTSHOWIMAGES")
         (fprintf (current-error-port) "exp->filename: unknown exp ~s\n" exp))
       (list 'missing)])))
