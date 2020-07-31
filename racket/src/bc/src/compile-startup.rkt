(module compile-startup '#%kernel
  (#%require '#%linklet
             "help-startup.rkt")
  
  ;; Decode a linklet S-expression from "startup.inc" (in the source
  ;; directory), compile it, and write it back as "cstartup.inc" (in
  ;; the build directory)
  
  (define-values (dest) (vector-ref (current-command-line-arguments) 0))
  (define-values (zo-dest) (vector-ref (current-command-line-arguments) 1))
  (define-values (src) (vector-ref (current-command-line-arguments) 2))
  (define-values (vers) (vector-ref (current-command-line-arguments) 3))
  (define-values (other-files) (list-tail (vector->list (current-command-line-arguments)) 4))

  (define-values (version-comparisons) (get-version-comparisons vers))
  
  ;; Bail out if we don't need to do anything:
  (if (file-exists? dest)
      (if (call-with-input-file dest (lambda (i)
                                       (begin
                                         (read-line i 'any)
                                         (not (eof-object? (read-line i 'any))))))
          (if (andmap (lambda (f)
                        ((file-or-directory-modify-seconds dest)
                         . > . 
                         (file-or-directory-modify-seconds f)))
                      (list* src vers other-files))
              (exit 0)
              (void))
          (void))
      (void))

  ;; Startup code as an S-expression uses the pattern
  ;;   (lambda <formals> (begin '<id> <expr>))
  ;; or
  ;;   (case-lambda [<formals> (begin '<id> <expr>)] <clause> ...)
  ;; to record a name for a function. Detect that pattern and
  ;; shift to an 'inferred-name property. We rely on the fact
  ;; that the names `lambda`, `case-lambda`, and `quote` are
  ;; never shadowed, so we don't have to parse expression forms
  ;; in general.
  (define-values (rename-functions)
    (lambda (e)
      (if (if (pair? e)
              (eq? 'quote (car e))
              #f)
          e
          (let-values ([(name)
                        (if (pair? e)
                            (let-values ([(begin-name)
                                          (lambda (b)
                                            (if (pair? b)
                                                (if (eq? 'begin (car b))
                                                    (if (pair? (cdr b))
                                                        (if (pair? (cddr b))
                                                            (let-values ([(a) (cadr b)])
                                                              (if (pair? a)
                                                                  (if (eq? 'quote (car a))
                                                                      (cadr a)
                                                                      #f)
                                                                  #f))
                                                            #f)
                                                        #f)
                                                    #f)
                                                #f))])
                              (if (eq? 'lambda (car e))
                                  (let-values ([(b) (caddr e)])
                                    (begin-name b))
                                  (if (eq? 'case-lambda (car e))
                                      (if (pair? (cdr e))
                                          (let-values ([(clause) (cadr e)])
                                            (begin-name (cadr clause)))
                                          #f)
                                      #f)))
                            #f)])
            (if name
                (correlated-property (datum->correlated #f (cons (car e) (rename-functions (cdr e))))
                                     'inferred-name
                                     name)
                (if (pair? e)
                    (cons (rename-functions (car e))
                          (rename-functions (cdr e)))
                    e))))))
  (define-values (datum->correlated) (hash-ref (primitive-table '#%kernel) 'datum->syntax))
  (define-values (correlated-property) (hash-ref (primitive-table '#%kernel) 'syntax-property))

  (define-values (linklet) (compile-linklet (rename-functions (get-linklet src))
                                            #f #f #f
                                            '(serializable unsafe static)))

  (define-values (DIGS-PER-LINE) 20)

  ;; In case someone wants to inspect the output with `raco decompile`:
  (call-with-output-file
   zo-dest
   (lambda (outfile) (write (hash->linklet-bundle (hasheq 'startup linklet)) outfile))
   'truncate)
  
  (call-with-output-file
   dest
   (lambda (outfile)
     (let-values ([(p) (open-output-bytes)])
       (write-linklet-bundle-hash (hasheq 'startup linklet) p)
       (let-values ([(s) (get-output-bytes p)])
         (fprintf outfile "#if 0 ~a\n" version-comparisons)
         (fprintf outfile "# include \"startup.inc\"\n")         
         (fprintf outfile "#else\n")
         (fprintf outfile "static unsigned char expr[] = {\n")
         (letrec-values ([(loop)
                          (lambda (chars pos)
                            (if (null? chars)
                                (void)
                                (begin
                                  (fprintf outfile "~a," (car chars))
                                  (loop (cdr chars)
                                        (if (= pos DIGS-PER-LINE)
                                            (begin
                                              (newline outfile)
                                              0)
                                            (add1 pos))))))])
                        (loop (bytes->list s) 0))
         (fprintf outfile "0};\n")
         (fprintf outfile "# define EVAL_STARTUP EVAL_ONE_SIZED_STR((char *)expr, ~a)\n" (bytes-length s))
         (fprintf outfile "#endif\n"))))
   'truncate))
