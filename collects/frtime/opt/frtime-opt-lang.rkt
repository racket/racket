;; This module defines all the functions necessary to write FrTime programs, 
;; as well as their lowered equivalents.  It doesn't know how to perform
;; optimization, though -- that is left to the frtime-opt module.
(module frtime-opt-lang racket
  ;; TODO(ghcooper/paddymahoney): Fix the duplicate requires and uncomment the
  ;; body of this module.
  #|
  (require (prefix-in frtime: frtime/frtime))
  (require (for-syntax racket/base frtime/opt/lowered-equivs))
  (require (only-in frtime/frtime-big event-receiver send-event 
   nothing null collect-garbage))
  
  ;; Export a function that is just a lifted version of a standard
  ;; function (with the same name).
  ;; TBD: don't import from frtime at all -- just lift the original function
  (define-syntax (provide/lifted stx)
    (syntax-case stx ()
      [(_ MOD FUNC)
       (let ([lowered-equiv-id (make-lowered-equiv-id #'FUNC)])
         #`(begin (require (rename-in frtime/frtime-big [FUNC lifted-func]))
                  (provide (rename-out [lifted-func FUNC]))
                  (require (rename-in MOD [FUNC #,lowered-equiv-id]))
                  (provide #,lowered-equiv-id)))]
      [(_ MOD FUNC FUNCS ...)
       #`(begin (provide/lifted MOD FUNC)
                (provide/lifted MOD FUNCS ...))]))

  (define-syntax (provide/already-lowered stx)
    (syntax-case stx ()
      [(_ FUNC)
       (let ([lowered-equiv-id (make-lowered-equiv-id #'FUNC)])
         #`(begin (require (only-in frtime/frtime-big FUNC))
                  ;; note: the definition is necessary here because otherwise the lowered
                  ;; equiv doesn't become part of the module's namespace, and there's
                  ;; no way to find the list of identifiers exported by a module other
                  ;; than by filtering its namespace (see all-provided-symbols in 
                  ;; lowered-equivs.rkt)
                  (define #,lowered-equiv-id FUNC)
                  (provide FUNC)
                  (provide #,lowered-equiv-id)))]
      [(_ FUNC FUNCS ...)
       #`(begin (provide/already-lowered FUNC)
                (provide/already-lowered FUNCS ...))]))

  (define-syntax provide/no-equiv
    (syntax-rules ()
      [(_ FUNC)
       (begin (require (rename-in frtime/frtime-big [FUNC func]))
              (provide (rename-out [func FUNC])))]
      [(_ FUNC FUNCS ...)
       (begin (provide/no-equiv FUNC)
              (provide/no-equiv FUNCS ...))]))

  (provide/lifted racket
   ;; equality
   eq? equal? eqv?
  
   ;; types
   boolean? symbol? #;vector? number? string? char? pair? void? procedure? #;port? eof-object?
   
   ;; numbers and math
   zero? even? odd? positive? negative? integer? real? rational? complex? exact? inexact?
   + - * / quotient remainder modulo
   = < > <= >=
   add1 sub1 min max
   cos sin tan atan asin acos                                        ;; trig
   abs log sqrt integer-sqrt exp expt floor ceiling round truncate   ;; reals
   numerator denominator rationalize lcm gcd                         ;; fractions
   imag-part real-part magnitude angle make-rectangular make-polar   ;; complex numbers
   bitwise-not bitwise-xor bitwise-and bitwise-ior arithmetic-shift  ;; bits
   

   ;; booleans and conditionals
   and or not when unless cond case
   
   ;; characters
   char>? char<? char=? char-ci>=? char-ci<=? char>=? char<=? 
   char-upper-case? #;char-lower-case? char-alphabetic? char-numeric? char-whitespace? 
   char-upcase char-downcase
   
   ;; strings
   string string-length string-append substring string-ref
   string=? string<? string<=? string>? string>=?
   string-ci=? string-ci<? string-ci<=? #;string-ci>? string-ci>=? 
   string-locale-ci=? string-locale<? string-locale-ci<? string-locale-ci>? 
   format
   
   ;; lists
   null? list? car cdr caar cadr cddr caddr cdddr cadddr cddddr
   length list-ref list-tail
   assq assv #;assoc memq memv #;member

   ;; vectors
   make-vector vector #;vector-length vector-ref
   
   ;; dates
   make-date date? date-dst? seconds->date current-seconds current-milliseconds
   date-year date-month date-day date-year-day date-week-day 
   date-hour date-minute date-second date-time-zone-offset 

   ;; conversion
   char->integer integer->char
   symbol->string string->symbol
   number->string string->number 
   list->string string->list
   list->vector vector->list
   inexact->exact exact->inexact
   
   ;; exceptions
   exn-message exn-continuation-marks exn:fail? continuation-mark-set->list 
   with-handlers
   
   ;; syntax
   expand #;expand-syntax syntax syntax->datum syntax-case syntax-rules
   
   ;; paths
   path? path-string? string->path path->string 
   bytes->path path->bytes build-path absolute-path? relative-path? 
   complete-path? path->complete-path resolve-path path-replace-suffix
   cleanse-path simplify-path normal-case-path split-path 

   ;; I/O
   printf fprintf file-exists? #;link-exists? #;make-file-or-directory-link
   #;file-or-directory-modify-seconds #;file-or-directory-permissions 
   #;rename-file-or-directory #;file-size #;copy-file #;delete-file

   ;; context
   current-error-port current-security-guard collection-path
   #;current-namespace #;current-command-line-arguments #;current-custodian
   current-directory #;current-eventspace

   ;; misc
   eval procedure-arity regexp-match void system-type
   )
  
  (provide/lifted srfi/1
    first second)

  ;; things that serve as their own lowered equivalent
  (provide/already-lowered
   event-receiver send-event 
   nothing null collect-garbage)
  
  ;; functions with no lowered equivalents
  (provide/no-equiv 
   ;; no equiv because these inherently work with signals
   seconds milliseconds value-now value-now/sync value-now/no-copy inf-delay delay-by synchronize
   for-each-e! map-e filter-e merge-e once-e accum-e accum-b collect-e collect-b when-e while-e -=> ==> =#>
   changes hold switch snapshot snapshot/sync snapshot-e integral derivative
   signal? undefined? undefined lift-strict =#=>

   ;; no equiv because we don't support lvalues
   set! set-cell! new-cell
   
   ;; no equiv because we have special handling for these special forms
   begin if let let* let-values letrec #;letrec-values
   define-values values define-syntax define-syntaxes
   
   ;; no lowered equiv because it allocates memory
   list list* cons reverse append

   ;; no equiv because it's a macro that expands into more primitive code
   case-lambda let*-values mk-command-lambda

   ;; no equiv because these accept higher-order functions, which may not 
   ;; have been lowered
   for-each map andmap ormap apply ;build-string #;build-vector

   ;; no equiv because these have non-local control flow (can't get your
   ;; hands on the return value in order to lift it again).
   raise raise-exceptions raise-type-error error exit let/ec

   ;; no equiv because I haven't completely thought through these
   lambda quote unquote unquote-splicing make-parameter parameterize
   procedure-arity-includes? dynamic-require)

  (provide #%app #%top #%datum require for-syntax provide define)
  (provide display) ;; for debugging
  
  #;(require frtime/frlibs/list
           frtime/frlibs/etc
           frtime/frlibs/math
           frtime/frlibs/date)
  
  #;(provide (all-from frtime/frlibs/list)
           (all-from frtime/frlibs/etc)
           (all-from frtime/frlibs/math)
           (all-from frtime/frlibs/date))
  
  ;; this define-struct macro defines a lowered equiv for all the
  ;; accessor functions
  (define-syntax (my-define-struct stx)
    (define (make-lowered-accessor struct-id field-id)
      (let* ([upper-id (datum->syntax
                        field-id
                        (string->symbol
                         (format "~s-~s" 
                                 (syntax-e struct-id)
                                 (syntax-e field-id))))]
             [lower-id (make-lowered-equiv-id upper-id)])
        ;; TBD: can we be smarter?  can we go straight for the field value and
        ;; bypass any signal-checking logic?  *is* there any signal-checking logic?
        #`(define #,lower-id #,upper-id)))
    (define (lowered-equiv-defns struct-id field-ids)
      (let ([lowered-accessors (map (lambda (field-id)
                                      (make-lowered-accessor struct-id field-id))
                                    field-ids)])
        #`(begin . #,lowered-accessors)))
    (syntax-case stx ()
      [(_ (STRUCT BASE) (FIELD ...) . REST)
       #`(begin
           (frtime:define-struct (STRUCT BASE) (FIELD ...) . REST)
           #,(lowered-equiv-defns #'STRUCT (syntax->list #'(FIELD ...))))]
      [(_ STRUCT (FIELD ...) . REST)
       #`(begin
           (frtime:define-struct STRUCT (FIELD ...) . REST)
           #,(lowered-equiv-defns #'STRUCT (syntax->list #'(FIELD ...))))]))
  (provide (rename-out [my-define-struct define-struct]))
|#
  )
