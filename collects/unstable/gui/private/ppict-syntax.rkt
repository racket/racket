#lang racket/base
(require syntax/parse syntax/parse/experimental/contract
         (for-template racket/base
                       racket/contract/base
                       racket/stxparam
                       slideshow/pict
                       "ppict.rkt"))
(provide fragment-sequence)

(define-syntax-class (fragment-sequence who xp-var rpss-var)
  #:commit
  #:local-conventions ([p (elem who)]
                       #|[b (bind-fragment who)]|#
                       [g (go-fragment who)]
                       [s (set-fragment who)]
                       [a (alt-fragment who)]
                       [fs (fragment-sequence who xp-var rpss-var)]
                       [pl (expr/c #'placer? #:name "argument to #:go")])
  (pattern ()
           #:with code
           #`(values #,xp-var
                     (apply append (reverse #,rpss-var))))
  (pattern (p ...+ . fs)
           #:with code
           #`(let*-values ([(#,xp-var picts)
                            (ppict-add/internal '#,who #,xp-var
                                                (syntax-parameterize
                                                    ([ppict-do-state
                                                      (make-rename-transformer #'#,xp-var)])
                                                    (list p.code ...)))]
                           [(#,rpss-var)
                            (cons picts #,rpss-var)])
               fs.code))
  (pattern (g . fs)
           #:with code
           #`(let-values ([(#,xp-var)
                           (syntax-parameterize ([ppict-do-state
                                                  (make-rename-transformer #'#,xp-var)])
                             (ppict-go #,xp-var g.placer))])
               fs.code))
  #|
  (pattern (b . fs)
           #:with code
           #`(let*-values ([(b.var ...)
                            (syntax-parameterize ([ppict-do-state
                                                   (make-rename-transformer #'#,xp-var)])
                              b.rhs)])
               fs.code))
  |#
  (pattern (s . fs)
           #:with code
           #`(let*-values ([(#,xp-var picts)
                            (let ([pict-or-fun
                                   (syntax-parameterize ([ppict-do-state
                                                          (make-rename-transformer #'#,xp-var)])
                                     s.p)])
                              (if (pict? pict-or-fun)
                                  (values pict-or-fun null)
                                  (pict-or-fun)))]
                           [(#,rpss-var) (cons picts #,rpss-var)])
               fs.code))
  (pattern (a . fs)
           #:with code
           #`(let*-values ([(alt-final alt-picts) (a.code #,xp-var)]
                           [(#,rpss-var) (cons (append alt-picts (list alt-final)) #,rpss-var)])
               ;; Note: fs.code continues with new rpss-var (shadowed), old xp-var
               fs.code)))

(define-splicing-syntax-class (go-fragment who)
  #:description "#:go fragment"
  (pattern (~seq #:go pl)
           #:declare pl (expr/c #'placer? #:name "placer argument of #:go fragment")
           #:with placer #'pl.c))

#|
(define-splicing-syntax-class (bind-fragment who)
  #:description "#:bind fragment"
  (pattern (~seq #:bind vs:var/vars rhs:expr)
           #:with (var ...) #'(vs.var ...)))
|#

(define-splicing-syntax-class (set-fragment who)
  #:description "#:set fragment"
  (pattern (~seq #:set p0)
           #:declare p0 (expr/c #'(or/c pict? (-> (values pict? (listof pict?))))
                                #:name "argument of #:set fragment")
           #:with p #'p0.c))

(define-splicing-syntax-class (alt-fragment who)
  #:description "#:alt fragment"
  (pattern (~seq #:alt altfs)
           #:declare altfs (fragment-sequence who #'alt-xp #'alt-rpss)
           #:with code
           #'(lambda (alt-xp) (let ([alt-rpss null]) altfs.code))))

(define-splicing-syntax-class (elem who)
  #:description "element fragment"
  (pattern (~seq #:next)
           #:with code #''next)
  (pattern (~seq e)
           #:declare e (expr/c #'(or/c pict? real? #f) #:name "element")
           #:with code #'e.c))

(define-syntax-class var/vars
  #:description "variable or sequence of variables"
  (pattern var1:id
           #:with (var ...) #'(var1))
  (pattern (var:id ...)))
