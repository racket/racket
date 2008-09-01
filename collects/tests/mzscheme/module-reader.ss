
(load-relative "loadtest.ss")

(Section 'module-reader)

;; ----------------------------------------

;; plain version
(module r0 syntax/module-reader scheme/base)

;; using a simple wrapper to get a case-insensitive reader
(module r1 syntax/module-reader scheme/base
  #:wrapper1 (lambda (t) (parameterize ([read-case-sensitive #f]) (t))))

;; using the more general wrapper to get a case-insensitive reader
(module r2 syntax/module-reader scheme/base
  #:wrapper2 (lambda (in r) (parameterize ([read-case-sensitive #f]) (r in))))

;; using explicit case-insensitive read/-syntax versions
(module r3 syntax/module-reader scheme/base
  #:read (wrap read) #:read-syntax (wrap read-syntax)
  (define ((wrap reader) . args)
    (parameterize ([read-case-sensitive #f]) (apply reader args))))

;; add something to the result
(module r4 syntax/module-reader zzz
  #:wrapper1 (lambda (t) (cons 'foo (t))))

;; same as above, but do it properly, if a syntax or a datum is needed
(module r5 syntax/module-reader zzz
  #:wrapper1 (lambda (t stx?) (cons (if stx? #'foo 'foo) (t))))

;; make an empty module, after reading the contents
(module r6 syntax/module-reader zzz
  #:wrapper1 (lambda (t) '()))

;; fake input port to get an empty module
(module r7 syntax/module-reader zzz
  #:wrapper2 (lambda (in rd) (rd (open-input-string ""))))

;; forget about the input -- just return a fixed empty input module
(module r8 syntax/module-reader whatever
  #:wrapper2 (lambda (in rd)
               (if (syntax? (rd in)) #'(module page zzz) '(module page zzz))))

;; a module that uses the scribble syntax with a specified language
(module r9 syntax/module-reader -ignored-
  #:wrapper2
  (lambda (in rd stx?)
    (let* ([lang (read in)]
           [mod  (parameterize ([current-readtable (make-at-readtable)])
                   (rd in))]
           [mod  (if stx? mod (datum->syntax #f mod))]
           [r (syntax-case mod ()
                [(module name lang* . body)
                 (with-syntax ([lang (datum->syntax
                                      #'lang* lang #'lang*)])
                   (syntax/loc mod (module name lang . body)))])])
      (if stx? r (syntax->datum r))))
  (require scribble/reader))

(define (from-string read str)
  (parameterize ([read-accept-reader #t])
    (read (open-input-string str))))

(define (test-both str result)
  (for ([read (list read
                    ;; same as `read', but using read-syntax
                    (lambda (in) (syntax->datum (read-syntax #f in))))])
    (test result from-string read str)))

(test-both "#reader 'r0 (define FoO #:bAr)"
           '(module page scheme/base (define FoO #:bAr)))

(for ([mod '(r1 r2 r3)])
  (test-both (format "#reader '~a (define FoO #:bAr)" mod)
             '(module page scheme/base (define foo #:bar))))

(test-both "#reader 'r4 (define foo #:bar)"
           '(module page zzz foo (define foo #:bar)))
(test-both "#reader 'r5 (define foo #:bar)"
           '(module page zzz foo (define foo #:bar)))

(test-both "#reader 'r6 (define foo #:bar)"
           '(module page zzz))

(test-both "#reader 'r9 scheme/base (define foo 1)"
           '(module page scheme/base (define foo 1)))
(test-both "#reader 'r9 scheme/base @define[foo]{one}"
           '(module page scheme/base (define foo "one")))

;; ----------------------------------------

(report-errs)
