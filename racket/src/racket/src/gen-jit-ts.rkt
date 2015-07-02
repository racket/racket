#lang at-exp racket/base
(require racket/string)

(define (char->type c)
  (case c
    [(#\s) "Scheme_Object*"]
    [(#\t) "const Scheme_Object*"]
    [(#\S) "Scheme_Object**"]
    [(#\b) "Scheme_Bucket*"]
    [(#\n) "Scheme_Native_Closure_Data*"]
    [(#\m) "MZ_MARK_STACK_TYPE"]
    [(#\p) "void*"]
    [(#\i) "int"]
    [(#\l) "intptr_t"]
    [(#\z) "size_t"]
    [(#\v) "void"]
    [else (error 'char->type "unknown: ~e" c)]))

(define (is-pointer-type? c)
  (case c
    [(#\s) #t]
    [(#\t) #t]
    [(#\S) #t]
    [(#\b) #t]
    [(#\n) #t]
    [(#\m) #f]
    [(#\p) #t]
    [(#\i) #f]
    [(#\l) #f]
    [(#\z) #f]
    [(#\v) #f]
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
   @list{#define define_ts_@|ts|(id, src_type) \
     static @|result-type| ts_ ## id(@|args|) \
        XFORM_SKIP_PROC \
     { \
       if (scheme_use_rtcall) \
         @|return| scheme_rtcall_@|t|("[" #id "]", src_type, @(string-join (cons "id" arg-names) ", ")); \
       else \
         @|return| id(@(string-join arg-names ", ")); \
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
    @|result-type| scheme_rtcall_@|ts|(const char *who, int src_type, prim_@|ts| f@|(if (null? arg-types) "" ",")| @|args|) 
      XFORM_SKIP_PROC
   {
     Scheme_Future_Thread_State *fts = scheme_future_thread_state;
     future_t *future;
     double tm;
     @(if (string=? result-type "void") "" @string-append{@|result-type| retval;})

     future = fts->thread->current_ft;
     future->prim_protocol = SIG_@|ts|;
     future->prim_func = f;
     tm = get_future_timestamp();
     future->time_of_request = tm;
     future->source_of_request = who;
     future->source_type = src_type;
     @(string-join
       (for/list ([t (in-string (type->arg-string t))]
                  [a arg-names]
                  [i (in-naturals)])
         @string-append{    future->arg_@|(string t)|@|(number->string i)| = @|a|;})
       "\n")
     @(if (equal? arg-types '("Scheme_Object*")) @string-append{send_special_result(future, @(car arg-names));} "")
     future_do_runtimecall(fts, (void*)f, 0, 1, 0);
     fts->thread = scheme_current_thread;
     future = fts->thread->current_ft;
     @(if (string=? result-type "void") "" @string-append{retval = @|fretval|;})
     @(if (string=? result-type "void") "" @string-append{@|fretval| = 0;})
     @(if (string=? result-type "Scheme_Object*") @string-append{receive_special_result(future, retval, 1);} "")
     @(if (string=? result-type "void") "" "return retval;")
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
         @(if (string=? result-type "void") "" @string-append{GC_CAN_IGNORE @|result-type| retval;})
         @(string-join
           (for/list ([t (in-string (type->arg-string t))]
                      [i (in-naturals)])
             @string-append{JIT_TS_LOCALIZE(@(char->type t), arg_@|(string t)|@|(number->string i)|);})
           " ")
         @(if (equal? arg-types '("Scheme_Object*")) @string-append{receive_special_result(future, future->arg_s0, 1);} "")
         @(string-join
           (for/list ([t (in-string (type->arg-string t))]
                      [i (in-naturals)]
                      #:when (is-pointer-type? t))
             @string-append{future->arg_@|(string t)|@|(number->string i)| = NULL;})
           " ")
         @(string-join
           (for/list ([t (in-string (type->arg-string t))]
                      [i (in-naturals)]
                      #:when (eq? t #\S))
             @string-append{ADJUST_RS_ARG(future, arg_@|(string t)|@|(number->string i)|);})
           " ")
         @(if (string=? result-type "void") "" "retval = ")
         f(@(string-join
              (for/list ([t (in-string (type->arg-string t))]
                         [i (in-naturals)])
                 @string-append{arg_@|(string t)|@|(number->string i)|})
             ", "));
         @(if (string=? result-type "void") "" @string-append{future->retval_@(substring ts (sub1 (string-length ts))) = retval;})
         @(if (string=? result-type "Scheme_Object*") @string-append{send_special_result(future, retval);} "")
         break;
      }
   })
   (newline))

(define proto-counter 11)

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
  (display @string-append{@|result-type| scheme_rtcall_@|ts|(const char *who, int src_type, prim_@|ts| f@(if (null? arg-types) "" ",") @|args|);})
  (newline))

(define types
  '(siS_s
    iSs_s
    s_s
    n_s
    _s
    ss_s
    ssi_s
    tt_s
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
    siS_v
    z_p
    si_s
    sis_v
    ss_i
    iSp_v
    sss_s
    _v
    iS_v))

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
