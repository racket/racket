#lang scheme/base

(require "find-version.ss")

(provide parse-library-reference)

(define (symbolic-identifier=? a b)
  (eq? (syntax-e a) (syntax-e b)))

(define (is-sub-version-reference? stx)
  (syntax-case* stx (<= >= and or not) symbolic-identifier=?
    [n (exact-nonnegative-integer? (syntax-e #'n)) #t]
    [(>= n) (exact-nonnegative-integer? (syntax-e #'n))]
    [(<= n) (exact-nonnegative-integer? (syntax-e #'n))]
    [(and sv ...) (andmap is-sub-version-reference? (syntax->list #'(sv ...)))]
    [(or sv ...) (andmap is-sub-version-reference? (syntax->list #'(sv ...)))]
    [(not sv) (is-sub-version-reference? #'sv)]
    [_ #f]))

(define (is-version-reference? stx)
  (syntax-case* stx (and or not) symbolic-identifier=?
    [(and vr ...)
     (andmap is-version-reference? (syntax->list #'(vr ...)))]
    [(or vr ...)
     (andmap is-version-reference? (syntax->list #'(vr ...)))]
    [(not vr)
     (is-version-reference? #'vr)]
    [(sv ...)
     (andmap is-sub-version-reference? (syntax->list #'(sv ...)))]
    [_ #f]))

(define (parse-library-reference stx err)
  (syntax-case stx ()
    [(id1 id2 ... (vers ...))
     (and (identifier? #'id1)
          (andmap identifier? (syntax->list #'(id2 ...)))
          (is-version-reference? #'(vers ...)))
     (let-values ([(coll file)
                   (let ([strs (map (lambda (id)
                                      (symbol->string (syntax-e id)))
                                    (syntax->list #'(id1 id2 ...)))])
                     (if (= 1 (length strs))
                         (values (list (car strs)) "main")
                         (values (reverse (cdr (reverse strs)))
                                 (car (reverse strs)))))])
       (let ([base (build-path (with-handlers ([exn:fail?
                                                (lambda (exn)
                                                  (err
                                                   (format 
                                                    "cannot find suitable library installed (exception: ~a)"
                                                    (if (exn? exn)
                                                        (exn-message exn)
                                                        exn))))])
                                 (apply collection-path coll))
                               file)])
         (let ([vers (find-version (path->bytes base) (syntax->datum #'(vers ...)))])
           (if vers
               (apply string-append
                      (car coll)
                      (append
                       (map (lambda (s)
                              (string-append "/" s))
                            (append (cdr coll) (list file)))
                       (map (lambda (v)
                              (format "-~a" v))
                            vers)
                       (list ".ss")))
               (err "cannot find suitable installed library")))))]
    [(id1 id2 ...)
     (and (identifier? #'id1)
          (andmap identifier? (syntax->list #'(id2 ...))))
     (parse-library-reference #'(id1 id2 ... ()) err)]
    [_
     (err "ill-formed library reference")]))
