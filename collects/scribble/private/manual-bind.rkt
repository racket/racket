#lang scheme/base
(require "../struct.rkt"
         "../scheme.rkt"
         "../search.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         (only-in "../core.rkt" make-style)
         "../html-properties.rkt"
         "manual-ex.rkt"
         racket/contract/base
         (for-syntax scheme/base)
         (for-label scheme/base
                    scheme/class))

(provide definition-site
         libs->taglet
         annote-exporting-library
         with-exporting-libraries
         id-to-target-maker
         id-to-form-target-maker
         *sig-elem
         (struct-out sig)
         ;; public:
         ; XXX unknown contract
         make-binding-redirect-elements
         sigelem)
(provide/contract
 ; XXX What is return type?
 [defidentifier ((identifier?) (#:form? boolean? #:index? boolean? #:show-libs? boolean?) . ->* . any/c)])

(define (gen-absolute-tag)
  `(abs ,(make-generated-tag)))

(define-struct sig (id))

(define-syntax-rule (sigelem sig elem)
  (*sig-elem (quote-syntax sig) 'elem))

(define (*sig-elem sig elem)
  (let ([s (to-element/no-color elem)])
    (make-delayed-element
     (lambda (renderer sec ri)
       (let* ([tag (find-scheme-tag sec ri sig #f)]
              [taglet (and tag (append (cadr tag) (list elem)))]
              [vtag (and tag `(sig-val ,taglet))]
              [stag (and tag `(sig-form ,taglet))]
              [sd (and stag (resolve-get/tentative sec ri stag))])
         (make-element
          symbol-color
          (list
           (cond [sd (make-link-element  syntax-link-color (list s) stag)]
                 [vtag (make-link-element value-link-color (list s) vtag)]
                 [else s])))))
     (lambda () s)
     (lambda () s))))

(define hovers (make-weak-hasheq))
(define (intern-hover-style text)
  (let ([text (datum-intern-literal text)])
    (or (hash-ref hovers text #f)
        (let ([s (make-style #f (list (make-hover-property text)))])
          (hash-set! hovers text s)
          s))))

(define (annote-exporting-library e)
  (make-delayed-element
   (lambda (render p ri)
     (let ([from (resolve-get/tentative p ri '(exporting-libraries #f))])
       (if (and from (pair? from))
           (make-element
            (intern-hover-style
             (string-append
              "Provided from: "
              (let loop ([from from])
                (if (null? (cdr from))
                    (format "~s" (car from))
                    (format "~s, ~a" (car from) (loop (cdr from)))))))
            e)
           e)))
   (lambda () e)
   (lambda () e)))

(define (get-exporting-libraries render p ri)
  (resolve-get/tentative p ri '(exporting-libraries #f)))

(define (with-exporting-libraries proc)
  (make-delayed-index-desc
   (lambda (render part ri)
     (proc (or (get-exporting-libraries render part ri) null)))))

(define (definition-site name stx-id form?)
  (let ([sig (current-signature)])
    (if sig
      (*sig-elem (sig-id sig) name)
      (annote-exporting-library
       (to-element (make-just-context name stx-id))))))

(define checkers (make-hash))

(define (libs->taglet id libs source-libs)
  (let ([lib
         (or (ormap (lambda (lib)
                      (let ([checker
                             (hash-ref
                              checkers lib
                              (lambda ()
                                (let ([ns-id 
                                       (let ([ns (make-base-empty-namespace)])
                                         (parameterize ([current-namespace ns])
                                           (namespace-require `(for-label ,lib))
                                           (namespace-syntax-introduce (datum->syntax #f 'x))))])
                                  (let ([checker
                                         (lambda (id)
                                           (free-label-identifier=?
                                            (datum->syntax ns-id (syntax-e id))
                                            id))])
                                    (hash-set! checkers lib checker)
                                    checker))))])
                        (and (checker id) lib)))
                    (or source-libs null))
             (and (pair? libs) (car libs)))])
    (and lib (module-path-index->taglet
              (module-path-index-join lib #f)))))

(define (id-to-target-maker id dep?)
  (*id-to-target-maker 'def id dep?))

(define (id-to-form-target-maker id dep?)
  (*id-to-target-maker 'form id dep?))

(define (*id-to-target-maker sym id dep?)
  (let ([sig (current-signature)])
    (lambda (content mk)
      (make-part-relative-element
       (lambda (ci)
         (let ([e (ormap (lambda (p)
                           (ormap (lambda (e)
                                    (and (exporting-libraries? e) e))
                                  (part-to-collect p)))
                         (collect-info-parents ci))])
           (unless e
             ;; Call raise-syntax-error to capture error message:
             (with-handlers ([exn:fail:syntax?
                              (lambda (exn)
                                (eprintf "~a\n" (exn-message exn)))])
               (raise-syntax-error
                'WARNING
                "no declared exporting libraries for definition" id)))
           (if e
             (let* ([lib-taglet (libs->taglet
                                 (if sig (sig-id sig) id)
                                 (exporting-libraries-libs e)
                                 (exporting-libraries-source-libs e))]
                    [tag (intern-taglet
                          (list (if sig
                                  (case sym
                                    [(def) 'sig-val]
                                    [(form) 'sig-def])
                                  sym)
                                `(,lib-taglet
                                  ,@(if sig (list (syntax-e (sig-id sig))) null)
                                  ,(syntax-e id))))])
               (if (or sig (not dep?))
                   (mk tag)
                   (make-dep (list lib-taglet (syntax-e id))
                             (mk tag))))
             content)))
       (lambda () content)
       (lambda () content)))))

(define (defidentifier id 
                       #:form? [form? #f]
                       #:index? [index? #t]
                       #:show-libs? [show-libs? #t])
  ;; This function could have more optional argument to select
  ;; whether to index the id, include a toc link, etc.
  (let ([dep? #t])
    (let ([maker (if form?
                     (id-to-form-target-maker id dep?)
                     (id-to-target-maker id dep?))]
          [elem (if show-libs?
                    (definition-site (syntax-e id) id form?)
                    (to-element id))])
      (if maker
          (maker elem
                 (lambda (tag)
                   (let ([elem
                          (if index?
                              (make-index-element
                               #f (list elem) tag
                               (list (datum-intern-literal (symbol->string (syntax-e id))))
                               (list elem)
                               (and show-libs?
                                    (with-exporting-libraries
                                     (lambda (libs)
                                       (make-exported-index-desc (syntax-e id)
                                                                 libs)))))
                              elem)])
                     (make-target-element #f (list elem) tag))))
          elem))))

(define (make-binding-redirect-elements mod-path redirects)
  (let ([taglet (module-path-index->taglet 
                 (module-path-index-join mod-path #f))])
    (make-element
     #f
     (map
      (lambda (redirect)
        (let ([id (car redirect)]
              [form? (cadr redirect)]
              [path (caddr redirect)]
              [anchor (cadddr redirect)])
          (let ([make-one
                 (lambda (kind)
                   (make-redirect-target-element
                    #f
                    null
                    (intern-taglet (list kind (list taglet id)))
                    path
                    anchor))])
            (make-element
             #f
             (list (make-one (if form? 'form 'def))
                   (make-dep (list taglet id) null)
                   (let ([str (datum-intern-literal (symbol->string id))])
                     (make-index-element #f
                                         null
                                         (intern-taglet
                                          (list (if form? 'form 'def)
                                                (list taglet id)))
                                         (list str)
                                         (list
                                          (make-element
                                           symbol-color
                                           (list
                                            (make-element
                                             (if form?
                                                 syntax-link-color
                                                 value-link-color)
                                             (list str)))))
                                         ((if form?
                                              make-form-index-desc
                                              make-procedure-index-desc)
                                          id
                                          (list mod-path)))))))))
      redirects))))


(define (make-dep t content)
  (make-collect-element
   #f
   content
   (lambda (ci)
     (collect-put! ci 
                   (intern-taglet (list 'dep t))
                   #t))))
