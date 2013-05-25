#lang scheme/base

(require "find-version.rkt"
         "encode-name.rkt"
         (for-template scheme/base))

(provide parse-import)

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
                   (let* ([strs (map (lambda (id)
                                       (symbol->string (syntax-e id)))
                                     (syntax->list #'(id1 id2 ...)))]
                          [len (length strs)]
                          [strs (map
                                 encode-name
                                 (if (and (= 2 len) (regexp-match? #rx"^main_*$" (cadr strs)))
                                     ;; rename (X main_*) => (X main__*)
                                     (list (car strs)
                                           (string-append (cadr strs) "_"))
                                     ;; no rename
                                     strs))])
                     (if (= 1 len) 
                         (values (list (car strs)) "main")
                         (values (reverse (cdr (reverse strs)))
                                 (car (reverse strs)))))])
       (let ([base (with-handlers ([exn:fail?
                                    (lambda (exn)
                                      (err
                                       (format 
                                        "cannot find suitable library installed (exception: ~a)"
                                        (if (exn? exn)
                                            (exn-message exn)
                                            exn))))])
                     (with-handlers ([exn:fail?
                                      (lambda (exn)
                                        ;; Next, try collection (hopefully not spliced):
                                        (build-path (apply collection-path coll)
                                                    file))])
                       ;; First, try specific file in collection, which works
                       ;; when the relevant collection is spliced:
                       (path-replace-suffix
                        (apply collection-file-path (path-add-suffix file #".rkt") coll)
                        #"")))])
         (let ([vers.ext (find-version (path->bytes base) (syntax->datum #'(vers ...)))])
           (if vers.ext
               (apply string-append
                      (car coll)
                      (append
                       (map (lambda (s)
                              (string-append "/" s))
                            (append (cdr coll) (list file)))
                       (map (lambda (v)
                              (format "-~a" v))
                            (car vers.ext))
                       (list (cdr vers.ext))))
               (err "cannot find suitable installed library")))))]
    [(id1 id2 ...)
     (and (identifier? #'id1)
          (andmap identifier? (syntax->list #'(id2 ...))))
     (parse-library-reference #'(id1 id2 ... ()) err)]
    [_
     (err "ill-formed library reference")]))

(define (convert-library-reference orig stx stx-err)
  (datum->syntax
   orig
   `(,#'lib
     ,(parse-library-reference stx
                               (lambda (msg)
                                 (stx-err msg orig stx))))
   orig))

(define (parse-import-set orig stx stx-err)
  (define (bad)
    (stx-err (format "bad `~a' form" 
                     (syntax-e (car (syntax-e stx))))
             orig
             stx))
  (define (check-id id)
    (unless (identifier? id)
      (stx-err (format "not an identifier in `~a' form"
                       (syntax-e (car (syntax-e stx))))
               orig
               id)))
  (syntax-case* stx (library only except prefix rename) symbolic-identifier=?
    [(library lib)
     (convert-library-reference orig #'lib stx-err)]
    [(library . _) (bad)]
    [(only im id ...)
     (for-each check-id (syntax->list #'(id ...)))
     #`(only-in #,(parse-import-set orig #'im stx-err) id ...)]
    [(only . _) (bad)]
    [(except im id ...)
     (for-each check-id (syntax->list #'(id ...)))
     #`(except-in #,(parse-import-set orig #'im stx-err) id ...)]
    [(except . _) (bad)]
    [(prefix im id)
     (check-id #'id)
     #`(prefix-in id #,(parse-import-set orig #'im stx-err))]
    [(prefix . _) (bad)]
    [(rename im (id id2) ...)
     (for-each check-id 
               (apply
                append
                (map syntax->list
                     (syntax->list #'((id id2) ...)))))
     #`(rename-in #,(parse-import-set orig #'im stx-err) [id id2] ...)]
    [(rename . _) (bad)]
    [_ (convert-library-reference orig stx stx-err)]))

(define (parse-import orig im stx-err)
  (syntax-case* im (for) symbolic-identifier=?
    [(for base-im level ...)
     (let* ([levels
             (cons
              #f
              (map (lambda (level)
                     (syntax-case* level (run expand meta) symbolic-identifier=?
                       [run #'0]
                       [expand #'1]
                       [(meta 0) #'0]
                       [(meta n) #'n]
                       [_
                        (stx-err
                         "bad `for' level"
                         orig
                         level)]))
                   (syntax->list #'(level ...))))])
       (with-syntax ([is (parse-import-set orig #'base-im stx-err)])
         (with-syntax ([(level ...) levels]
                       [prelims (datum->syntax orig
                                               'r6rs/private/prelims)])
           #`((for-meta level is prelims) ...))))]
    [(for . _)
     (stx-err
      "bad `for' import form"
      orig
      im)]
    [_ (let ([m (parse-import-set orig im stx-err)])
         (list m `(for-label ,m)))]))
