;; Preprocessor utilities for the .rktc file.

#lang at-exp racket/base

(require (for-syntax racket/base) racket/list scribble/text/output)

(provide maplines)
(define (maplines #:semicolons? [semi? #t] fun . ls)
  (add-between
   (apply filter-map
          (lambda xs
            (let ([r (apply fun xs)])
              (cond [(list? r) (if semi? (append r '(";")) r)]
                    [(or (not r) (void? r)) #f]
                    [else (error 'maplines "bad result: ~e" r)])))
          ls)
   "\n"))

;; thunks are forced -- so this can be used as @@IFDEF{...}{...} too!
(provide IFDEF IFNDEF IF)
(define ((((IF*DEF token choose) . c) . t) . e)
  (if (null? e)
    @list{@disable-prefix{#}@token @c
          @t
          @disable-prefix{#}endif /* @c */}
    @list{@disable-prefix{#}@token @c
          @t
          @disable-prefix{#}else /* @c @(choose '("undefined" . "defined")) */
          @e
          @disable-prefix{#}endif /* @c */}))
(define IFDEF  (IF*DEF "ifdef"  car))
(define IFNDEF (IF*DEF "ifndef" cdr))
(define IF     (IF*DEF "if"     (lambda (x) "")))

(provide DEFINE UNDEF)
(define (DEFINE . t) @list{@disable-prefix{#}define @t})
(define (UNDEF  . t) @list{@disable-prefix{#}undef @t})

(provide racket-id->c-name)
(define (racket-id->c-name str)
  (set! str (format "~a" str))
  (for ([subst '([#rx"->" "_to_"] [#rx"[-/]" "_"] [#rx"\\*" "S"]
                 [#rx"\\?$" "_p"] [#rx"!$" "_bang"])])
    (set! str (regexp-replace* (car subst) str (cadr subst))))
  str)

;; Used to avoid bogus compilation errors
(provide hush)
(define hush @'{return NULL@";" /* hush the compiler */})

;; User function definition
(provide cfunctions)
(define cfunctions (make-parameter '()))
(define (_cdefine name minargs maxargs . body)
  (define cname @list{foreign_@(racket-id->c-name name)})
  (cfunctions (cons (list name cname minargs maxargs) (cfunctions)))
  @list{@disable-prefix{#define MYNAME "@name"}
        static Scheme_Object *@|cname|(int argc, Scheme_Object *argv[])
        {
          @body
        }
        @disable-prefix{#undef MYNAME}})
(provide cdefine)
(define-syntax (cdefine stx)
  (syntax-case stx ()
    [(_ name minargs maxargs body ...)
     (number? (syntax-e #'maxargs))
     #'(_cdefine `name minargs maxargs body ...)]
    [(_ name args body ...)
     #'(_cdefine `name args args body ...)]))

;; Struct definitions
(provide cstructs)
(define cstructs (make-parameter '()))
(define (_cdefstruct name slots types)
  (define cname (regexp-replace* #rx"-" (symbol->string name) "_"))
  (define mname (string-upcase (regexp-replace* #rx"_" cname "")))
  (define predname (string->symbol (format "~a?" name)))
  (define (mark/fix mode)
    @list{int @|cname|_@|mode|(void *p) {
            @|cname|_struct *s = (@|cname|_struct *)p;
            @(maplines (lambda (s t)
                         (when (regexp-match #rx"[*]" t)
                           @list{gc@|mode|(s->@s)}))
                       slots types)
            return gcBYTES_TO_WORDS(sizeof(@|cname|_struct));
          }})
  (cstructs (cons (list* name cname slots) (cstructs)))
  @list{/* @name structure definition */
        static Scheme_Type @|cname|_tag;
        typedef struct @|cname|_struct {
          Scheme_Object so;
          @(maplines (lambda (s t) @list{@t @s}) slots types)
        } @|cname|_struct;
        #define SCHEME_@|mname|P(x) (SCHEME_TYPE(x)==@|cname|_tag)
        @_cdefine[predname 1 1]{
          return SCHEME_@|mname|P(argv[0]) ? scheme_true : scheme_false@";"
        }
        /* 3m stuff for @cname */
        #ifdef MZ_PRECISE_GC
        START_XFORM_SKIP;
        int @|cname|_SIZE(void *p) {
          return gcBYTES_TO_WORDS(sizeof(@|cname|_struct));
        }
        @mark/fix{MARK}
        @mark/fix{FIXUP}
        END_XFORM_SKIP;
        #endif})
(provide cdefstruct)
(define-syntax-rule (cdefstruct name [slot type] ...)
  (_cdefstruct `name (list `slot ...) (list type ...)))

;; Tagged object allocation
(define (_cmake var type . values)
  (define cstruct (cdr (assq type (cstructs))))
  (define cname (car cstruct))
  @list{@var = (@|cname|_struct*)scheme_malloc_tagged(sizeof(@|cname|_struct));
        @|var|->so.type = @|cname|_tag;
        @(maplines (lambda (v f) @list{@|var|->@f = (@v)})
                   values (cdr cstruct))})
(provide cmake)
(define-syntax-rule (cmake var type val ...) (_cmake var `type val ...))

;; Pre-allocated symbols
(provide symbols)
(define symbols (make-parameter '()))
(define (add-symbols syms)
  (maplines (lambda (s)
              (define new
                @list{@(regexp-replace #rx"-" (symbol->string s) "_")_sym})
              (when (assq s (symbols))
                (error 'add-symbols "symbol ~s already defined" s))
              (symbols (cons (list s new) (symbols)))
              @list{static Scheme_Object *@new})
            syms))
(provide defsymbols)
(define-syntax defsymbols
  (syntax-rules () [(_ sym ...) (add-symbols '(sym ...))]))

;; warn against manual edits to the generated file
(provide header)
(define (header orig)
  @list{/********************************************
         ** Do not edit this file!
         ** This file is generated from @orig,
         ** to make changes, edit that file and
         ** run it to generate an updated version
         ** of this file.
         ********************************************/})
