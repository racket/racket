#lang scheme/base
(require "../decode.rkt"
         "../scheme.rkt"
         "../struct.rkt"
         (only-in "../core.rkt" 
                  make-style style-name style-properties
                  nested-flow? nested-flow-blocks nested-flow-style
                  make-nested-flow)
         "../html-properties.rkt"
         racket/contract/base
         (for-syntax scheme/base
                     syntax/kerncase
                     syntax/boundmap)
         (for-label scheme/base
                    scheme/class))

(define-struct (box-splice splice) ())

(provide/contract
 [struct (box-splice splice) ([run list?])]) ; XXX ugly copying
(provide deftogether *deftogether
         with-racket-variables
         with-togetherable-racket-variables
         vertical-inset-style
         boxed-style
         add-background-label)

(define vertical-inset-style 
  (make-style 'vertical-inset null))

(define boxed-style 
  (make-style 'boxed (list (make-attributes (list (cons 'class "RBoxed"))))))

(define ((add-background-label what) l)
  (list
   (make-nested-flow
    (make-style "RBackgroundLabel" (list 'decorative 'command (alt-tag "div")
                                         (make-attributes '((class . "SIEHidden")))))
    (list
     (make-nested-flow
      (make-style "RBackgroundLabelInner" (list (alt-tag "div")))
      (list (make-omitable-paragraph what)))))
   (let* ([a (car l)]
          [remake (if (paragraph? a)
                      (lambda (sa)
                        (paragraph
                         (sa (paragraph-style a))
                         (paragraph-content a)))
                      (lambda (sa)
                        (table
                         (sa (table-style a))
                         (table-blockss a))))])
     (remake
      (lambda (s)
        (make-style (style-name s)
                    (let ([p (style-properties s)])
                      (if (ormap attributes? p)
                          (for/list ([i (in-list p)])
                            (if (attributes? i)
                                (let ([al (attributes-assoc i)])
                                  (if (assq 'class al)
                                      (for/list ([a (in-list al)])
                                        (if (eq? (car a) 'class)
                                            (cons 'class (string-append (cdr a) " RForeground"))
                                            a))
                                      (attributes (cons '(class . "RForeground")
                                                        al))))
                                i))
                          (cons (attributes '((class . "RForeground")))
                                p)))))))))

(begin-for-syntax (define-struct deftogether-tag () #:omit-define-syntaxes))

(define-syntax (with-togetherable-racket-variables stx)
  (syntax-case stx ()
    [(_ lits vars decl)
     (with-syntax ([vars (syntax-property #'vars 'taint-mode 'none)])
       (syntax-property
        #'(with-togetherable-racket-variables* lits vars decl)
        'taint-mode
        'transparent))]))

(define-syntax-rule (with-togetherable-racket-variables* . rest)
  (with-racket-variables . rest))

(define-syntax (with-racket-variables stx)
  (syntax-case stx ()
    [(_ lits ([kind s-exp] ...) body)
     (let ([ht (make-bound-identifier-mapping)]
           [lits (syntax->datum #'lits)])
       (for-each (lambda (kind s-exp)
                   (case (syntax-e kind)
                     [(proc)
                      (letrec ([do-proc
                                (lambda (s-exp)
                                  (let ([s-exp (syntax->list s-exp)])
                                    (for-each
                                     (lambda (arg)
                                       (if (identifier? arg)
                                           (unless (or (eq? (syntax-e arg) '...)
                                                       (eq? (syntax-e arg) '...+)
                                                       (eq? (syntax-e arg) '_...superclass-args...)
                                                       (memq (syntax-e arg) lits))
                                             (bound-identifier-mapping-put! ht arg #t))
                                           (syntax-case arg ()
                                             [(kw arg . rest)
                                              (and (keyword? (syntax-e #'kw))
                                                   (identifier? #'arg))
                                              (bound-identifier-mapping-put! ht #'arg #t)]
                                             [(arg . rest)
                                              (identifier? #'arg)
                                              (bound-identifier-mapping-put! ht #'arg #t)]
                                             [else (void)])))
                                     (cdr s-exp))
                                  (unless (identifier? (car s-exp))
                                    ;; Curried:
                                    (do-proc (car s-exp)))))])
                        (do-proc s-exp))]
                     [(form form/none form/maybe non-term)
                      (define skip-id (case (syntax-e kind)
                                         [(form) 
                                          (syntax-case s-exp ()
                                            [(defined-id actual-s-exp) (let ([id #'defined-id])
                                                                         (and (identifier? id)
                                                                              id))]
                                            [_ #f])]
                                         [else #f]))
                      (let loop ([form (case (syntax-e kind)
                                         [(form) 
                                          (syntax-case s-exp ()
                                            [(defined-id actual-s-exp) #'actual-s-exp])]
                                         [(form/none) s-exp]
                                         [(form/maybe)
                                          (syntax-case s-exp ()
                                            [(#f form) #'form]
                                            [(#t (id . form)) #'form])]
                                         [(non-term) s-exp])])
                        (if (identifier? form)
                            (unless (or (and skip-id
                                             (free-identifier=? skip-id form))
                                        (eq? (syntax-e form) '...)
                                        (eq? (syntax-e form) '...+)
                                        (eq? (syntax-e form) 'code:line)
                                        (eq? (syntax-e form) 'code:blank)
                                        (eq? (syntax-e form) 'code:comment)
                                        (eq? (syntax-e form) '?)
                                        (memq (syntax-e form) lits))
                              (bound-identifier-mapping-put! ht form #t))
                            (syntax-case form (unsyntax)
                              [(unsyntax _) (void)]
                              [(a . b) (loop #'a) (loop #'b)]
                              [#(a ...) (loop #'(a ...))]
                              [_ (void)])))]
                     [else
                      (raise-syntax-error
                       #f
                       "unknown variable mode"
                       stx
                       kind)]))
                 (syntax->list #'(kind ...))
                 (syntax->list #'(s-exp ...)))
       (with-syntax ([(id ...) (bound-identifier-mapping-map ht (lambda (k v) k))])
         #'(letrec-syntaxes ([(id) (make-variable-id 'id)] ...)
             body)))]))


(define (*deftogether boxes body-thunk)
  (make-box-splice
   (cons
    (make-blockquote 
     vertical-inset-style
     (list
      (make-table
       boxed-style
       (map
        (lambda (box)
          (unless (and (box-splice? box)
                       (= 1 (length (splice-run box)))
                       (nested-flow? (car (splice-run box)))
                       (eq? vertical-inset-style (nested-flow-style (car (splice-run box))))
                       (let ([l (nested-flow-blocks (car (splice-run box)))])
                         (= 1 (length l))
                         (table? (car l))
                         (eq? boxed-style (table-style (car l)))))
            (error 'deftogether
                   "element is not a boxing splice containing a single nested-flow with a single table: ~e"
                   box))
          (list (make-flow (list (make-table
                                  "together"
                                  (table-flowss (car (nested-flow-blocks (car (splice-run box))))))))))
        boxes))))
    (body-thunk))))

(define-syntax (deftogether stx)
  (syntax-case stx ()
    [(_ (def ...) . body)
     (with-syntax ([((_ (lit ...) (var ...) decl) ...)
                    (map (lambda (def)
                           (let ([exp-def (local-expand 
                                           def
                                           (list (make-deftogether-tag))
                                           (cons
                                            #'with-togetherable-racket-variables*
                                            (kernel-form-identifier-list)))])
                             (syntax-case exp-def (with-togetherable-racket-variables*)
                               [(with-togetherable-racket-variables* lits vars decl)
                                exp-def]
                               [_
                                (raise-syntax-error
                                 #f
                                 "sub-form is not a documentation form that can be combined"
                                 stx
                                 def)])))
                         (syntax->list #'(def ...)))])
       #'(with-togetherable-racket-variables
          (lit ... ...)
          (var ... ...)
          (*deftogether (list decl ...) (lambda () (list . body)))))]))
