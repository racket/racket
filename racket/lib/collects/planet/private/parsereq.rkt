#lang racket/base

(require mzlib/match
         "short-syntax-helpers.rkt"
         "data.rkt")

(provide (struct-out request)
         parse-package-string
         (struct-out exn:parse-failure)
         spec->req
         pkg-spec->full-pkg-spec
         version->bounds
         
         string->longpkg
         string->shortpkg
         short-pkg-string->spec)

(define-struct request (full-pkg-spec file path))

(define (tospec owner pkg maj min)
  `(,owner ,pkg ,@(if maj (list maj) '()) ,@(if min (list min) '())))

(define-struct (exn:parse-failure exn:fail) ())

(define (string->longpkg s)
  (let ([mtch (regexp-match #rx"^[ ]*([^ :/]+)[ ]+([^ :/]+)[ ]*([0-9]*)[ ]*([0-9]*)[ ]*$" s)])
    (if mtch 
        (let-values ([(owner pkg majstr minstr) (apply values (cdr mtch))])
          (tospec owner pkg 
                  (if (string=? majstr "") #f (string->number majstr))
                  (if (string=? minstr "") #f (string->number minstr))))
        #f)))

(define (string->shortpkg s)
  (define ((yell fmt) x) (raise (make-exn:parse-failure (format fmt x) (current-continuation-marks))))
  (with-handlers ([exn:parse-failure? (λ (e) #f)])
    (let* ([pkg-spec/tail (short-pkg-string->spec s yell)]
           [pkg-spec (car pkg-spec/tail)]
           [tail     (cadr pkg-spec/tail)])
      (if (regexp-match #rx"^[ ]*$" tail) pkg-spec #f))))

(define all-parsers (list string->longpkg string->shortpkg))

;; parse-package-string : string -> pkg-spec
;; parses a "package name string", the kind of string that shows up in places where we're only interested
;; in naming a particular package, not a full path
(define (parse-package-string str)
  (define (yell str) (raise (make-exn:parse-failure str (current-continuation-marks))))
  (ormap (λ (p) (p str)) all-parsers))

;; spec->req : sexp[require sexp] stx -> request
;; maps the given require spec to a planet package request structure
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
       (let* ([pkg-spec/tail (short-pkg-string->spec str yell)]
              [pkg-spec (car pkg-spec/tail)]
              [tail (cadr pkg-spec/tail)]
              [fullspec (pkg-spec->full-pkg-spec pkg-spec stx)]
              [final-path (if (string=? tail "")
                              "main.rkt"
                              (if (regexp-match #rx"[.]" tail)
                                  tail
                                  (string-append tail ".ss")))])
         (make-request fullspec final-path '())))]
    [_ (raise-syntax-error 'require (format "Illegal PLaneT invocation: ~.s" (cdr spec)) stx)]))

;; short-pkg-string->spec : string (string -> string -> 'a) -> (list pkg-spec string)
;; extracts the named package from the given short-style string, returning 
;; both that package spec and the leftover string
(define (short-pkg-string->spec str yell)
  (try-parsing str
    ([(consume-whitespace)]
     [owner   (get-next-slash        #:on-error (yell "expected an owner, received ~e"))]
     [package (get-next-slash-or-end #:on-error (yell "expected a package, received ~e"))])
    (λ (tail)
      (let*-values ([(yell!) (yell "~a")]
                    [(pkg maj min) (parse-package package yell!)])
        (list (tospec owner pkg maj min) tail)))))

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
