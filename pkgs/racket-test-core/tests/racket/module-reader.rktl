
(load-relative "loadtest.rktl")

(Section 'module-reader)

;; ----------------------------------------

(define (from-string read str)
  (parameterize ([read-accept-reader #t])
    (read (open-input-string str))))

(define (test-both mods str result)
  (for* ([mod mods]
         [str (in-value (format str mod))]
         [read (list read
                     ;; same as `read', but using read-syntax
                     (lambda (in) (syntax->datum (read-syntax #f in))))])
    (test result from-string read str)))

;; plain version
(module r0 syntax/module-reader scheme/base)
(test-both '(r0) "#reader '~s (define FoO #:bAr)"
           '(module anonymous-module scheme/base
              (#%module-begin (define FoO #:bAr))))

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
;;
(test-both '(r1 r2 r3) "#reader '~s (define FoO #:bAr)"
           '(module anonymous-module scheme/base
              (#%module-begin (define foo #:bar))))

;; add something to the result
(module r4 syntax/module-reader zzz
  #:wrapper1 (lambda (t) (cons 'foo (t))))
;; same, but do it properly, if a syntax or a datum is needed
(module r5 syntax/module-reader zzz
  #:wrapper1 (lambda (t stx?) (cons (if stx? #'foo 'foo) (t))))
;;
(test-both '(r4 r5) "#reader '~s (define foo #:bar)"
           '(module anonymous-module zzz
              (#%module-begin foo (define foo #:bar))))

;; make an empty module, after reading the contents
(module r6 syntax/module-reader zzz
  #:wrapper1 (lambda (t) '()))
;; fake input port to get an empty module
(module r7 syntax/module-reader zzz
  #:wrapper2 (lambda (in rd) (rd (open-input-string ""))))
;; forget about the input -- just return a fixed empty input module
(module r8 syntax/module-reader whatever
  #:wrapper2 (lambda (in rd)
               (if (syntax? (rd in))
                 #'(module anonymous-module zzz (#%module-begin))
                 '(module anonymous-module zzz (#%module-begin)))))
;; the same, the easy way
(module r9 syntax/module-reader
  #:language (lambda () 'zzz)
  #:wrapper1 (lambda (t) '()))
;;
(test-both '(r6 r7 r8 r9) "#reader '~s (define foo #:bar)"
           '(module anonymous-module zzz (#%module-begin)))

;; a module that uses the scribble syntax with a specified language
(module r10 syntax/module-reader -ignored-
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
;; same, using #:language
(module r11 syntax/module-reader
  #:language read
  #:wrapper2 (lambda (in rd stx?)
               (parameterize ([current-readtable (make-at-readtable)])
                 (rd in)))
  (require scribble/reader))
;;
(test-both '(r10 r11) "#reader '~s scheme/base (define foo 1)"
           '(module anonymous-module scheme/base
              (#%module-begin (define foo 1))))
(test-both '(r10 r11) "#reader '~s scheme/base @define[foo]{one}"
           '(module anonymous-module scheme/base
              (#%module-begin (define foo "one"))))

;; ----------------------------------------

(report-errs)
