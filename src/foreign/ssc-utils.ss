;; Utilities for .ssc preprocessor files.

(define (~ . args) (apply show args) (newline*))
(define \\ newline*)

(define (seplist l sep)
  (cdr (apply append (map (lambda (x) (list sep x)) l))))
(define-syntax push!
  (syntax-rules () [(push! x l) (set! l (cons x l))]))
(define-syntax pop!
  (syntax-rules () [(pop! l) (begin0 (car l) (set! l (cdr l)))]))
(define (upcase x)
  (list->string (map char-upcase (string->list (format "~a" x)))))

(define (scheme-id->c-name str)
  (let loop ([str (format "~a" str)]
             [substs '((#rx"->" "_to_") (#rx"[-/]" "_") (#rx"\\*" "S")
                       (#rx"\\?$" "_p") (#rx"!$" ""))])
    (if (null? substs)
      str
      (loop (regexp-replace* (caar substs) str (cadar substs)) (cdr substs)))))

;; User function definition
(define cfunctions '())
(define (_cdefine name minargs . maxargs)
  (define cname
    (list "foreign_" (scheme-id->c-name name)))
  (set! maxargs (if (null? maxargs) minargs (car maxargs)))
  (push! (list name cname minargs maxargs) cfunctions)
  (list "#undef MYNAME" \\ "#define MYNAME \""name"\""\\
        "static Scheme_Object *"cname"(int argc, Scheme_Object *argv[])"\\))
(define-syntax cdefine
  (syntax-rules ()
    [(_ name minargs maxargs) (_cdefine `name minargs maxargs)]
    [(_ name args) (_cdefine `name args args)]))

;; Struct definitions
(define cstructs '())
(define (_cdefstruct name slots types)
  (define cname
    (regexp-replace* #rx"-" (symbol->string name) "_"))
  (define mname
    (list->string
     (map char-upcase (string->list (regexp-replace* #rx"_" cname "")))))
  (define predname
    (string->symbol (string-append (symbol->string name)"?")))
  (~ "/* "name" structure definition */")
  (~ "static Scheme_Type "cname"_tag;" \\
     "typedef struct "cname"_struct {" \\
     "  Scheme_Object so;")
  (for-each (lambda (s t) (~ "  "t" "s";")) slots types)
  (~ "} "cname"_struct;" \\
     "#define SCHEME_"mname"P(x) (SCHEME_TYPE(x)=="cname"_tag)")
  (~ (_cdefine predname 1)
     "{ return SCHEME_"mname"P(argv[0]) ? scheme_true : scheme_false; }")
  (~ "/* 3m stuff for "cname" */" \\
     "#ifdef MZ_PRECISE_GC" \\
     "START_XFORM_SKIP;"
     "int "cname"_SIZE(void *p) {" \\
     "  return gcBYTES_TO_WORDS(sizeof("cname"_struct));" \\
     "}")
  (let ([mark/fix (lambda (mode)
                    (~ "int "cname"_"mode"(void *p) {" \\
                       "  "cname"_struct *s = ("cname"_struct *)p;")
                    (for-each (lambda (s t)
                                (when (regexp-match #rx"[*]" t)
                                  (~ "  gc"mode"(s->"s");")))
                              slots types)
                    (~ "  return gcBYTES_TO_WORDS(sizeof("cname"_struct));" \\
                       "}"))])
    (mark/fix "MARK")
    (mark/fix "FIXUP"))
  (~ "END_XFORM_SKIP;" \\
     "#endif")
  (push! (list* name cname slots) cstructs))
(define-syntax cdefstruct
  (syntax-rules ()
    [(_ name (slot type) ...)
     (_cdefstruct `name (list `slot ...) (list type ...))]))

;; Tagged object allocation
(define (_cmake-object var type . values)
  (define cstruct (cdr (assq type cstructs)))
  (~ var" = ("(car cstruct)"_struct*)scheme_malloc_tagged(sizeof("
            (car cstruct)"_struct));" \\
     var"->so.type = "(car cstruct)"_tag;")
  (for-each (lambda (v f) (~ var"->"f" = ("v");")) values (cdr cstruct)))
(define-syntax cmake-object
  (syntax-rules () [(_ var type val ...) (_cmake-object var `type val ...)]))

;; Pre-allocated symbols
(define symbols '())
(define (add-symbols syms)
  (map (lambda (s)
         (when (assq s symbols)
           (error 'add-symbols "symbol ~s already defined" s))
         (push! (list s (list (regexp-replace #rx"-" (symbol->string s) "_")
                              "_sym"))
                symbols)
         (list "static Scheme_Object *"(cadar symbols)";"\\))
       syms))
(define-syntax defsymbols
  (syntax-rules () [(_ sym ...) (add-symbols '(sym ...))]))
