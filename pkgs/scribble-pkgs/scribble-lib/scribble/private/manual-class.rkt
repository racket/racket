#lang scheme/base
(require "../decode.rkt"
         "../struct.rkt"
         "../scheme.rkt"
         "../search.rkt"
         "../basic.rkt"
         "../manual-struct.rkt"
         "qsloc.rkt"
         scheme/serialize
         scheme/stxparam
         "manual-utils.rkt"
         "manual-style.rkt"
         "manual-scheme.rkt"
         "manual-bind.rkt"
         "manual-method.rkt"
         "manual-proc.rkt"
         "manual-vars.rkt"
         "manual-class-struct.rkt"
         scheme/list
         (for-syntax scheme/base)
         (for-label scheme/base
                    scheme/class))

(provide defclass
         defclass/title
         definterface
         definterface/title
         defmixin
         defmixin/title
         defconstructor
         defconstructor/make
         defconstructor*/make
         defconstructor/auto-super
         defmethod
         defmethod*
         methspec
         methimpl
         this-obj
         method xmethod (rename-out [method ::]))

(define-syntax-parameter current-class #f)

(define-struct decl (name super app-mixins intfs ranges mk-head body))
(define-struct constructor (def))
(define-struct meth (names mode def))
(define-struct spec (def))
(define-struct impl (def))

(define (id-info id)
  (let ([b (identifier-label-binding id)])
    (if b
      (list (caddr b)
            (list-ref b 3)
            (list-ref b 4)
            (list-ref b 5)
            (list-ref b 6))
      (error 'scribble "no class/interface/mixin information for identifier: ~e"
             id))))

(define (make-inherited-table r d ri decl)
  (define start
    (let ([key (find-scheme-tag d ri (decl-name decl) #f)])
      (if key (list (cons key (lookup-cls/intf d ri key))) null)))
  (define supers
    (if (null? start)
      null
      (cdr
       (let loop ([supers start][accum null])
         (cond
           [(null? supers) (reverse accum)]
           [(assoc (caar supers) accum)
            (loop (cdr supers) accum)]
           [else
            (let ([super (car supers)])
              (loop (append (filter-map
                             (lambda (i)
                               (let ([key (find-scheme-tag d ri i #f)])
                                 (and key
                                      (cons key (lookup-cls/intf d ri key)))))
                             (append
                              (reverse (cls/intf-intfs (cdr super)))
                              (if (cls/intf-super (cdr super))
                                (list (cls/intf-super (cdr super)))
                                null)
                              (reverse (cls/intf-app-mixins (cdr super)))))
                            (cdr supers))
                    (cons super accum)))])))))
  (define ht
    (let ([ht (make-hasheq)])
      (for* ([i (decl-body decl)]
             #:when (meth? i)
             [name (meth-names i)])
        (hash-set! ht name #t))
      ht))
  (define inh
    (append-map
     (lambda (super)
       (let ([inh (filter-map
                   (lambda (k)
                     (if (hash-ref ht k #f)
                       #f
                       (begin (hash-set! ht k #t)
                              (cons (datum-intern-literal (symbol->string k))
                                    (**method k (car super))))))
                   (cls/intf-methods (cdr super)))])
         (if (null? inh)
           null
           (cons (make-element #f (list (make-element "inheritedlbl" '("from "))
                                        (cls/intf-name-element (cdr super))))
                 (map cdr (sort inh string<? #:key car))))))
     supers))
  (if (null? inh)
    (make-auxiliary-table "inherited" null)
    (make-auxiliary-table
     "inherited"
     (map (lambda (i) (list (to-flow i)))
          (cons (make-element "inheritedlbl" '("Inherited methods:")) inh)))))

(define (make-decl-collect decl)
  (make-part-collect-decl
   ((id-to-target-maker (decl-name decl) #f)
    (list "ignored")
    (lambda (tag)
      (make-collect-element
       #f null
       (lambda (ci)
         (collect-put!
          ci
          `(cls/intf ,(cadr tag))
          (make-cls/intf
           (make-element
            symbol-color
            (list (make-link-element
                   value-link-color
                   (list (datum-intern-literal
                          (symbol->string (syntax-e (decl-name decl)))))
                   tag)))
           (map id-info (decl-app-mixins decl))
           (and (decl-super decl)
                (not (free-label-identifier=? (quote-syntax object%)
                                              (decl-super decl)))
                (id-info (decl-super decl)))
           (map id-info (decl-intfs decl))
           (append-map (lambda (m)
                         (let loop ([l (meth-names m)])
                           (cond [(null? l) null]
                                 [(memq (car l) (cdr l)) (loop (cdr l))]
                                 [else (cons (car l) (loop (cdr l)))])))
                       (filter meth? (decl-body decl)))))))))))

(define (build-body decl body)
  `(,@(map (lambda (i)
             (cond [(constructor? i) ((constructor-def i))]
                   [(meth? i) ((meth-def i))]
                   [else i]))
           body)
    ,(make-delayed-block (lambda (r d ri) (make-inherited-table r d ri decl)))))

(define (*include-class/title decl)
  (make-splice
   (list* (title #:style 'hidden (to-element (decl-name decl)))
          (make-decl-collect decl)
          (build-body decl (append ((decl-mk-head decl) #t)
                                   (decl-body decl))))))

(define (*include-class decl)
  (make-splice
   (cons
    (make-decl-collect decl)
    (append
     ((decl-mk-head decl) #f)
     (let-values ([(pre post)
                   (let loop ([l (decl-body decl)][accum null])
                     (cond
                      [(null? l) (values (reverse accum) null)]
                      [(or (constructor? (car l)) (meth? (car l)))
                       (values (reverse accum) l)]
                      [else (loop (cdr l) (cons (car l) accum))]))])
       (append
        (flow-paragraphs (decode-flow pre))
        (list
         (make-blockquote
          "leftindent"
          (flow-paragraphs
           (decode-flow (build-body decl post)))))))))))

(define (*class-doc kind stx-id super intfs ranges whole-page? make-index-desc)
  (make-table
   boxed-style
   (append
    (list
     (list 
      ((add-background-label (symbol->string kind))
       (make-flow
        (list
         (make-omitable-paragraph
          (list (let ([target-maker (id-to-target-maker stx-id #t)]
                      [content (annote-exporting-library
                                (to-element #:defn? #t stx-id))]
                      [ref-content (to-element stx-id)])
                  (if target-maker
                      (target-maker
                       content
                       (lambda (tag)
                         ((if whole-page?
                              make-page-target-element
                              (lambda (s c t)
                                (make-toc-target2-element s c t ref-content)))
                          #f
                          (list
                           (make-index-element
                            #f content tag
                            (list (datum-intern-literal
                                   (symbol->string (syntax-e stx-id))))
                            (list ref-content)
                            (with-exporting-libraries
                             (lambda (libs)
                               (make-index-desc (syntax-e stx-id) libs)))))
                          tag)))
                      content))
                spacer ":" spacer
                (case kind
                  [(class) (racket class?)]
                  [(interface) (racket interface?)]
                  [(mixin) (racketblockelem (class? . -> . class?))]))))))))
    (if super
      (list
       (list (make-flow
              (list (t (hspace 2) "superclass:" spacer (to-element super))))))
      null)
    (let ([show-intfs
           (lambda (intfs range?)
             (if (null? intfs)
               null
               (list
                (list
                 (make-flow
                  (list
                   (make-table
                    #f
                    (cons
                     (list (make-flow
                            (list (make-omitable-paragraph
                                   (list (hspace 2)
                                         (case kind
                                           [(interface) "implements:"]
                                           [(class) "extends:"]
                                           [(mixin)
                                            (if range?
                                              "result implements:"
                                              "argument extends/implements:")])
                                         spacer))))
                           (to-flow (to-element (car intfs))))
                     (map (lambda (i)
                            (list flow-spacer (to-flow (to-element i))))
                          (cdr intfs))))))))))])
      (append (show-intfs intfs #f) (show-intfs ranges #t))))))

(define-syntax extract-super
  (syntax-rules ()
    [(_ (mixin base)) (extract-super base)]
    [(_ super) (quote-syntax/loc super)]))

(define-syntax extract-app-mixins
  (syntax-rules ()
    [(_ (mixin base)) (cons (quote-syntax/loc mixin) (extract-app-mixins base))]
    [(_ super) null]))

(define (flatten-splices l)
  (let loop ([l l])
    (cond [(null? l) null]
          [(splice? (car l)) (append (splice-run (car l)) (loop (cdr l)))]
          [else (cons (car l) (loop (cdr l)))])))

(define-syntax-rule (*defclass *include-class name super (intf ...) body ...)
  (*include-class
   (syntax-parameterize ([current-class (quote-syntax name)])
     (make-decl (quote-syntax/loc name)
                (extract-super super)
                (extract-app-mixins super)
                (list (quote-syntax/loc intf) ...)
                null
                (lambda (whole-page?)
                  (list (*class-doc 'class
                                    (quote-syntax/loc name)
                                    (quote-syntax/loc super)
                                    (list (quote-syntax intf) ...)
                                    null
                                    whole-page?
                                    make-class-index-desc)))
                (flatten-splices (list body ...))))))

(define-syntax-rule (defclass name super (intf ...) body ...)
  (*defclass *include-class name super (intf ...) body ...))

(define-syntax-rule (defclass/title name super (intf ...) body ...)
  (*defclass *include-class/title name super (intf ...) body ...))

(define-syntax-rule (*definterface *include-class name (intf ...) body ...)
  (*include-class
   (syntax-parameterize ([current-class (quote-syntax name)])
     (make-decl (quote-syntax/loc name)
                #f
                null
                (list (quote-syntax/loc intf) ...)
                null
                (lambda (whole-page?)
                  (list
                   (*class-doc 'interface
                               (quote-syntax/loc name)
                               #f
                               (list (quote-syntax intf) ...)
                               null
                               whole-page?
                               make-interface-index-desc)))
                (list body ...)))))

(define-syntax-rule (definterface name (intf ...) body ...)
  (*definterface *include-class name (intf ...) body ...))

(define-syntax-rule (definterface/title name (intf ...) body ...)
  (*definterface *include-class/title name (intf ...) body ...))

(define-syntax-rule (*defmixin *include-class name (domain ...) (range ...)
                               body ...)
  (*include-class
   (syntax-parameterize ([current-class (quote-syntax name)])
     (make-decl (quote-syntax/loc name)
                #f
                null
                (list (quote-syntax/loc domain) ...)
                (list (quote-syntax/loc range) ...)
                (lambda (whole-page?)
                  (list
                   (*class-doc 'mixin
                               (quote-syntax/loc name)
                               #f
                               (list (quote-syntax domain) ...)
                               (list (quote-syntax range) ...)
                               whole-page?
                               make-mixin-index-desc)))
                (list body ...)))))

(define-syntax-rule (defmixin name (domain ...) (range ...) body ...)
  (*defmixin *include-class name (domain ...) (range ...) body ...))

(define-syntax-rule (defmixin/title name (domain ...) (range ...) body ...)
  (*defmixin *include-class/title name (domain ...) (range ...) body ...))

(define-syntax (defconstructor*/* stx)
  (syntax-case stx ()
    [(_ mode ((arg ...) ...) desc ...)
     (let ([n (syntax-parameter-value #'current-class)])
       (with-syntax ([name n]
                     [result
                      (datum->syntax
                       #f
                       (list
                        (datum->syntax #'is-a?/c 'is-a?/c (list 'src 1 1 2 1))
                        (datum->syntax n (syntax-e n) (list 'src 1 3 4 1)))
                       (list 'src 1 0 1 5))]
                     [(((kw ...) ...) ...)
                      (map (lambda (ids)
                             (map (lambda (arg)
                                    (if (and (pair? (syntax-e arg))
                                             (eq? (syntax-e #'mode) 'new))
                                      (list (string->keyword
                                             (symbol->string
                                              (syntax-e
                                               (car (syntax-e arg))))))
                                      null))
                                  (syntax->list ids)))
                           (syntax->list #'((arg ...) ...)))])
         #'(make-constructor (lambda ()
                               (defproc* #:mode mode #:within name
                                 [[(make [kw ... . arg] ...) result] ...]
                                 desc ...)))))]))

(define-syntax (defconstructor stx)
  (syntax-case stx ()
    [(_ ([id . arg-rest] ...) desc ...)
     #'(defconstructor*/* new (([id . arg-rest] ...)) desc ...)]))

(define-syntax (defconstructor/make stx)
  (syntax-case stx ()
    [(_ ([id . arg-rest] ...) desc ...)
     #'(defconstructor*/* make (([id . arg-rest] ...)) desc ...)]))

(define-syntax (defconstructor*/make stx)
  (syntax-case stx ()
    [(_ (([id . arg-rest] ...) ...) desc ...)
     #'(defconstructor*/* make (([id . arg-rest] ...) ...) desc ...)]))

(define-syntax (defconstructor/auto-super stx)
  (syntax-case stx ()
    [(_ ([id . arg-rest] ...) desc ...)
     #'(defconstructor*/* new (([id . arg-rest] ... _...superclass-args...))
         desc ...)]))

(define-syntax (defmethod* stx)
  (syntax-case stx ()
    [(_ #:mode mode ([(name arg ...) result-type] ...) desc ...)
     (with-syntax ([cname (syntax-parameter-value #'current-class)]
                   [name1 (car (syntax->list #'(name ...)))])
       (with-syntax ([(extra ...)
                      (let ([finality
                             (lambda ()
                               (case (syntax-e #'mode)
                                 [(override-final public-final extend-final)
                                  #'(" This method is final, so it cannot be overiddden.")]
                                 [(augment-final)
                                  #'(" This method is final, so it cannot be augmented.")]
                                 [else null]))])
                        (case (syntax-e #'mode)
                          [(pubment)
                           #'((t "Refine this method with "
                                 (racket augment) "."))]
                          [(override override-final extend augment)
                           #`((t (case (syntax-e #'mode)
                                   [(override override-final) "Overrides "]
                                   [(extend extend-final) "Extends "]
                                   [(augment augment-final) "Augments "])
                                 (*xmethod/super (quote-syntax/loc cname) 'name1)
                                 "."
                                 #,@(finality)))]
                          [else null]))])
         #'(make-meth '(name ...)
                      'mode
                      (lambda ()
                        (defproc* #:mode send #:within cname
                          ([(name arg ...) result-type] ...)
                          (make-splice
                           (append-map (lambda (f)
                                         (cond [(impl? f) ((impl-def f))]
                                               [(spec? f) ((spec-def f))]
                                               [else (list f)]))
                                       (list extra ... desc ...))))))))]
    [(_ ([(name arg ...) result-type] ...) desc ...)
     #'(defmethod* #:mode public ([(name arg ...) result-type] ...) desc ...)]))

(define-syntax defmethod
  (syntax-rules ()
    [(_ #:mode mode (name arg ...) result-type desc ...)
     (defmethod* #:mode mode ([(name arg ...) result-type]) desc ...)]
    [(_ (name arg ...) result-type desc ...)
     (defmethod #:mode public (name arg ...) result-type desc ...)]))

(define-syntax-rule (methimpl body ...)
  (make-impl (lambda () (list (italic "Default implementation:") " " body ...))))

(define-syntax-rule (methspec body ...)
  (make-spec (lambda () (list (italic "Specification:") " " body ...))))

(define (*this-obj cname)
  (name-this-object cname))

(define-syntax (this-obj stx)
  (syntax-case stx ()
    [(_)
     (with-syntax ([cname (syntax-parameter-value #'current-class)])
       #'(*this-obj 'cname))]))

(define (*xmethod/super cname name)
  (let ([get
         (lambda (d ri key)
           (if key
             (let ([v (lookup-cls/intf d ri key)])
               (if v
                 (append (cls/intf-app-mixins v)
                         (cons (cls/intf-super v)
                               (cls/intf-intfs v)))
                 null))
             null))])
    (make-delayed-element
     (lambda (r d ri)
       (let loop ([search (get d ri (find-scheme-tag d ri cname #f))])
         (cond
           [(null? search)
            (list (make-element #f '("<method not found>")))]
           [(not (car search))
            (loop (cdr search))]
           [else
            (let* ([a-key (find-scheme-tag d ri (car search) #f)]
                   [v (and a-key (lookup-cls/intf d ri a-key))])
              (if v
                (if (member name (cls/intf-methods v))
                  (list
                   (make-element #f
                                 (list (**method name a-key)
                                       " in "
                                       (cls/intf-name-element v))))
                  (loop (append (cdr search)
                                (get d ri (find-scheme-tag d ri (car search)
                                                           #f)))))
                (loop (cdr search))))])))
     (lambda () (format "~a in ~a" (syntax-e cname) name))
     (lambda () (format "~a in ~a" (syntax-e cname) name)))))

(define (lookup-cls/intf d ri tag)
  (let ([v (resolve-get d ri `(cls/intf ,(cadr tag)))])
    (or v (make-cls/intf "unknown" null #f null null))))
