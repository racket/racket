#lang at-exp scheme/base
(require scheme/string)

(define (char->type c)
  (case c
    [(#\s) "Scheme_Object*"]
    [(#\S) "Scheme_Object**"]
    [(#\b) "Scheme_Bucket*"]
    [(#\n) "Scheme_Native_Closure_Data*"]
    [(#\m) "MZ_MARK_STACK_TYPE"]
    [(#\i) "int"]
    [(#\l) "long"]
    [(#\v) "void"]
    [else (error 'char->type "unknown: ~e" c)]))

(define (type->arg-string t)
  (let* ([t (symbol->string t)])
    (substring t 0 (- (string-length t) 2))))

(define (parse-type t)
  (let* ([s (symbol->string t)])
    (values
     (for/list ([c (in-string (type->arg-string t))])
       (char->type c))
     (char->type (string-ref s (sub1 (string-length s)))))))

(define (make-arg-list arg-types arg-names)
  (string-join (map (lambda (t a)
                      (string-append t " " a))
                    arg-types arg-names)
               ", "))

(define (gen-definer t)
  (define-values (arg-types result-type) (parse-type t))
  (define arg-names (map symbol->string (map (lambda (v) (gensym)) arg-types)))
  (define return (if (equal? result-type "void") "" "return"))
  (define args (make-arg-list arg-types arg-names))
  (define ts (symbol->string t))
  (for-each display
   @list{#define define_ts_@|ts|(id) \
     static @|result-type| ts_ ## id(@|args|) \
     { \
       START_XFORM_SKIP; \
       if (scheme_use_rtcall) \
         @|return| scheme_rtcall_@|t|(id, @(string-join arg-names ", ")); \
       else \
         @|return| id(@(string-join arg-names ", ")); \
       END_XFORM_SKIP; \
     }})
   (newline))

(define (gen-future-side t)
  (define-values (arg-types result-type) (parse-type t))
  (define arg-names (map symbol->string (map (lambda (v) (gensym)) arg-types)))
  (define return (if (equal? result-type "void") "" "return"))
  (define args (make-arg-list arg-types arg-names))
  (define ts (symbol->string t))
  (define fretval @string-append{future->retval_@|(substring ts (sub1 (string-length ts)))|})
  (for-each
   display
   @list{
    @|result-type| scheme_rtcall_@|ts|(prim_@|ts| f@|(if (null? arg-types) "" ",")| @|args|) 
   {
     START_XFORM_SKIP;
     future_t *future;
     @(if (string=? result-type "void") "" @string-append{@|result-type| retval;})

     future = current_ft;
     future->prim_protocol = SIG_@|ts|;
     future->prim_func = f;
     @(string-join
       (for/list ([t (in-string (type->arg-string t))]
                  [a arg-names]
                  [i (in-naturals)])
         @string-append{    future->arg_@|(string t)|@|(number->string i)| = @|a|;})
       "\n")
     future_do_runtimecall((void*)f, 0);
     future = current_ft;
     @(if (string=? result-type "void") "" @string-append{retval = @|fretval|;})
     @(if (string=? result-type "void") "" @string-append{@|fretval| = NULL;})
     @(if (string=? result-type "void") "" "return retval;")
     END_XFORM_SKIP;
   }
   })
   (newline))

(define (gen-runtime-side t)
  (define-values (arg-types result-type) (parse-type t))
  (define arg-names (map symbol->string (map (lambda (v) (gensym)) arg-types)))
  (define return (if (equal? result-type "void") "" "return"))
  (define args (make-arg-list arg-types arg-names))
  (define ts (symbol->string t))
  (for-each
   display
   @list{
    case SIG_@|ts|:
      {
         prim_@|ts| f = (prim_@|ts|)future->prim_func;
         @(if (string=? result-type "void") "" @string-append{@|result-type| retval;})
         @(if (string=? result-type "void") "" "retval = ")
         f(@(string-join
              (for/list ([t (in-string (type->arg-string t))]
                         [i (in-naturals)])
                 @string-append{future->arg_@|(string t)|@|(number->string i)|})
             ", "));
         @(if (string=? result-type "void") "" @string-append{future->retval_@(substring ts (sub1 (string-length ts))) = retval;})
         break;
      }
   })
   (newline))

(define proto-counter 5)

(define (gen-protos t)
  (define-values (arg-types result-type) (parse-type t))
  (define arg-names (map symbol->string (map (lambda (v) (gensym)) arg-types)))
  (define return (if (equal? result-type "void") "" "return"))
  (define args (make-arg-list arg-types arg-names))
  (define ts (symbol->string t))
  (printf "#define SIG_~a ~a\n" t proto-counter)
  (set! proto-counter (add1 proto-counter))
  (display
   @string-append{typedef @|result-type| (*prim_@|ts|)(@(string-join arg-types ", "));})
  (newline)
  (display @string-append{@|result-type| scheme_rtcall_@|ts|(prim_@|ts| f@(if (null? arg-types) "" ",") @|args|);})
  (newline))

(define types
  '(siS_s
    iSs_s
    s_s
    n_s
    _s
    ss_s
    ss_m
    Sl_s
    l_s
    bsi_v
    iiS_v
    ss_v
    b_v
    sl_s
    iS_s
    S_s
    s_v
    iSi_s
    siS_v))

(with-output-to-file "jit_ts_def.c"
  #:exists 'replace
  (lambda ()
    (for-each gen-definer types)))

(with-output-to-file "jit_ts_future_glue.c"
  #:exists 'replace
  (lambda ()
    (for-each gen-future-side types)))

(with-output-to-file "jit_ts_runtime_glue.c"
  #:exists 'replace
  (lambda ()
    (for-each gen-runtime-side types)))

(with-output-to-file "jit_ts_protos.h"
  #:exists 'replace
  (lambda ()
    (for-each gen-protos types)))
