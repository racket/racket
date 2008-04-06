#lang scheme/base

(require mzlib/match
         "private/short-syntax-helpers.ss"
         "private/data.ss")

(provide (struct-out request)
         spec->req
         pkg-spec->full-pkg-spec
         version->bounds)

(define-struct request (full-pkg-spec file path))

(define (spec->req spec stx)
  (match (cdr spec)
    [(file-name pkg-spec path ...)
     (unless (string? file-name)
       (raise-syntax-error #f (format "File name: expected a string, received: ~s" file-name) stx))
     (unless (andmap string? path)
       ;; special-case to catch a possibly common error:
       (if (ormap number? path)
           (raise-syntax-error #f (format "Module path must consist of strings only, received a number (maybe you intended to specify a package version number?): ~s" path) stx)
           (raise-syntax-error #f (format "Module path must consist of strings only, received: ~s" path) stx)))
     (make-request (pkg-spec->full-pkg-spec pkg-spec stx)
                   file-name
                   path)]
    [((? (lambda (x) (or (symbol? x) (string? x))) s))
     (let ([str (if (symbol? s) (symbol->string s) s)])
       (define (yell msg) (λ (str) (raise-syntax-error #f (format msg str) stx)))
       (try-parsing str
                    ([owner   (get-next-slash #:on-error (yell "Illegal syntax; expected an owner, received ~e"))]
                     [package (get-next-slash-or-end #:on-error (yell "Illegal syntax; expected a package, received ~e"))])
                    (λ (tail)
                      (let-values ([(pkg maj min) (parse-package package stx)])
                        (let* ([pkg-spec `(,owner ,pkg ,@(if maj (list maj) '()) ,@(if min (list min) '()))]
                               [fullspec (pkg-spec->full-pkg-spec pkg-spec stx)]
                               [final-path (if (string=? tail "")
                                               "main.ss"
                                               (string-append tail ".ss"))])
                          (make-request fullspec final-path '()))))))]
    [_ (raise-syntax-error 'require (format "Illegal PLaneT invocation: ~e" (cdr spec)) stx)]))

; pkg-spec->full-pkg-spec : PKG-SPEC syntax -> FULL-PKG-SPEC
(define (pkg-spec->full-pkg-spec spec stx)
  
  (define (pkg name maj lo hi path) (make-pkg-spec name maj lo hi path stx (version)))
  (define (fail* msg)
    (raise-syntax-error 'require (string->immutable-string msg) stx))
  (define (fail)
    (fail* (format "Invalid PLaneT package specifier: ~e" spec)))
  
  (match spec
    [((? string? owner) (? string? package) ver-spec ...)
     (match-let ([(maj min-lo min-hi) (version->bounds ver-spec fail*)])
       (pkg package maj min-lo min-hi (list owner)))]
    [((? (o not string?) owner) _ ...)
     (fail* (format "Expected string [package owner] in first position, received: ~e" owner))]
    [(_ (? (o not string?) pkg) _ ...)
     (fail* (format "Expected string [package name] in second position, received: ~e" pkg))]
    [_ (fail)]))


  ;; version->bounds : VER-SPEC -> (list (number | #f) number (number | #f)) | #f
  ;; determines the bounds for a given version-specifier
  ;; [technically this handles a slightly extended version of VER-SPEC where MAJ may
  ;;  be in a list by itself, because that's slightly more convenient for the above fn]
(define (version->bounds spec-list fail)
  (match spec-list
    [() (list #f 0 #f)]
    [(? number? maj) (version->bounds (list maj))]
    [((? number? maj)) (list maj 0 #f)]
    [((? number? maj) min-spec)
     (let ((pkg (lambda (min max) (list maj min max))))
       (match min-spec
         [(? number? min)                 (pkg min #f)]
         [((? number? lo) (? number? hi)) (pkg lo  hi)]
         [('= (? number? min))            (pkg min min)]
         [('+ (? number? min))            (pkg min #f)]
         [('- (? number? min))            (pkg 0   min)]
         
         ;; failure cases
         [(? (o/and (o not number?) 
                    (o/or (o not list?)
                          (λ (x) (not (= (length x) 2))))))
          (fail (format "Expected number or version range specifier for minor version specification, received: ~e" min-spec))]
         [((? (λ (x) 
                (and (not (number? x))
                     (not (memq x '(= + -)))))
              range)
           _)
          (fail (format "Illegal range specifier in minor version specification. Legal range specifiers are numbers, =, +, -; given: ~e" range))]
         [(_ (? (o not number?) bnd))
          (fail (format "Expected number as range bound in minor version specification, given: ~e" bnd))]
         [_ (fail (format "Illegal minor version specification: ~e" min-spec))]))]
    
    ;; failure cases
    [(? (o/and (o not number?) (o not list?)) v)
     (fail (format "Version specification expected number or sequence, received: ~e" v))]
    [((? (o not number?) maj) _ ...)
     (fail (format "Version specification expected number for major version, received: ~e" maj))]
    [_  (fail "Invalid version specification")]))

(define (o f g) (λ (x) (f (g x))))
(define (o/and . es) (λ (x) (andmap (λ (f) (f x)) es)))
(define (o/or . es) (λ (x) (ormap (λ (f) (f x)) es)))
