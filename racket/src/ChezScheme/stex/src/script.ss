;;; script.ss
;;;
;;; Copyright (c) 2005 R. Kent Dybvig
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

#!chezscheme
(library (script)
  (export command-line-case run-script)
  (import (chezscheme))

  #|

  (command-line-case command-line [(<cmdspec>) <body>]*)

    <cmdspec> -> (keyword <kwd> <required-arg>* <possible-action>]) <cmdspec>
               | (flags [<kwd> <flag-arg>* <possible-action>]*) <cmdspec>
               | <argspec>
    <possible-action> -> <empty> | $ <action>
    <flag-arg> -> <var> | (<type> <var>) | (<type> <var> <default>)
    <argspec> -> <required-arg>* <optional-arg>*
               | <required-arg>* <optional-arg>* <required-arg> ...
    <required-arg> -> <var> | (<type> <var>)
    <optional-arg> -> (optional <var>)
                    | (optional <type> <var>)
                    | (optional <type> <var> <default>)
    <kwd> -> <id> | (<id>+)
    <var> -> <id>
    <type> -> <id>
    <action> -> <expr>
    <default> -> <expr>

  Each <var> must be unique.

  If any <type> is not specified, it defaults to string.

  If any <default> is not specified, it defaults to #f.  Each <default>
  is scoped outside of the command-line-case form.

  Command-line arguments are processed from left to right.

  Command-line elements must appear in the order specified by <cmdspec>,
  except that each the flags in a single flags section may appear in
  any order and any flag may occur more than once.

  For kewyword and flag clauses, the value of the <action>, if specified,
  is evaluated for effect.  Each <action> is scoped where each of the <var>s
  is visible and each <var>'s current value is based on the specified or
  implicit defaults and the arguments seen so far.

  If a flag occurs more than once on a command line, the final value of
  each corresponding <var> is its last specified value.

  If <type> is not string, the procedure string-><type> is applied to the
  string argument to cast the string argument the actual value; it should
  return #f if the cast fails, in which case the clause doesn't match.
  A string-><type> routine should not cause side effects since it may be
  called even for clauses that don't match.

  For flag clauses, variable ?id, where <kwd> = id or <kwd> = (id id1 ...),
  is bound to #t if the argument is specified at least once, otherwise #f.

  Within each <body>, the variable usage is bound to a thunk that prints
  usage information.  usage information is also printed if a command
  line cannot be parsed to fit any of the clauses.

  consider:
   * instead of one keyword, have multiple keywords ALL of which must be
     provided in some order.  use syntax (all (keyword ...) ...), and use
     (some (keyword ...) ...) instead of current "flags" syntax.

   * add a prefix before <kwd> in flags section to allow multiple
     occurrences of the given flag and listification of the flag
     arguments

   * option to print more verbose information when certain matches fail,
     for example, when a keyword or flag argument requires more additional
     arguments than are provided.

   * allowing optional arguments after keywords and flags.  leads to
     ambiguity in some cases.

  Testing:

  scheme
  (import (script))
  (define (exit . args) (void))

  > (command-line-case '("a" "-q" "-v" "-v" "-b" "c")
      [((flags [-b c] [-q $ (write-char #\q)] [-v $ (write-char #\v)]))
       (list ?-b c)])
  qvv(#t "c")
    
  > (let ()
      (define (bar cl)
        (command-line-case cl
          [((keyword --foo (number n))
           (flags [-b (number b)] [-q $ (write-char #\q)] [-v $ (write-char #\v)]))
           (pretty-print (list ?-b b))]))
      (bar '("a" "--foo" "32" "-q" "-v" "-v" "-b" "45"))
      (bar '("a" "--foo" "foo" "-q" "-v" "-v" "-b" "45"))
      (bar '("a" "--foo" "32" "-q" "-v" "-v" "-b" "b")))
  qvv(#t 45)
  usage: a --foo n [ -b b ] [ -q ] [ -v ]
  usage: a --foo n [ -b b ] [ -q ] [ -v ]

  (define (foo cl)
    (define compact? #t)
    (define (register-boot-file x) (printf "registering boot file ~s\n" x))
    (define (register-heap-file x) (printf "registering boot file ~s\n" x))
    (command-line-case  cl
      [((keyword --version))
       (print-version)]
      [((keyword --help (number helplevel)))
       (printf "here's your help: ~s\n" helplevel)]
      [((keyword --help))
       (print-help)]
      [((flags [(--boot -b) bootpath $ (register-boot-file bootpath)]
               [(--compact -c) $ (set! compact? (not compact?))]
               [(--heap -h) heappath $ (register-heap-file heappath)]
               [(--quiet -q)]
               [(--saveheap -s) (number level 0) savepath]
               [--verbose])
        (flags [--])
        a (optional b) (optional number c 666) (number d) ...)
       (let-syntax ([pr (syntax-rules ()
                          [(_ x ...) (begin (printf "  ~s = ~s\n" 'x x) ...)])])
         (pr ?--boot bootpath ?--compact ?--heap heappath ?--quiet
             ?--saveheap level savepath ?--verbose ?--
             compact?
             a b c d))]))

  > (foo '("/usr/local/bin/foo" "--help" "3"))
  here's your help: 3
  > (foo '("/usr/local/bin/foo" "aaa"))
    ?--boot = #f
    bootpath = #f
    ?--compact = #f
    ?--heap = #f
    heappath = #f
    ?--quiet = #f
    ?--saveheap = #f
    level = 0
    savepath = #f
    ?--verbose = #f
    ?-- = #f
    compact? = #t
    a = "aaa"
    b = #f
    c = 666
    d = ()
  > (foo '("/usr/local/bin/foo" "aaa" "bbb"))
    ?--boot = #f
    bootpath = #f
    ?--compact = #f
    ?--heap = #f
    heappath = #f
    ?--quiet = #f
    ?--saveheap = #f
    level = 0
    savepath = #f
    ?--verbose = #f
    ?-- = #f
    compact? = #t
    a = "aaa"
    b = "bbb"
    c = 666
    d = ()
  > (foo '("/usr/local/bin/foo" "aaa" "bbb" "#xccc"))
    ?--boot = #f
    bootpath = #f
    ?--compact = #f
    ?--heap = #f
    heappath = #f
    ?--quiet = #f
    ?--saveheap = #f
    level = 0
    savepath = #f
    ?--verbose = #f
    ?-- = #f
    compact? = #t
    a = "aaa"
    b = "bbb"
    c = 3276
    d = ()
  > (foo '("/usr/local/bin/foo" "aaa" "bbb" "#xccc" "3" "4" "5"))
    ?--boot = #f
    bootpath = #f
    ?--compact = #f
    ?--heap = #f
    heappath = #f
    ?--quiet = #f
    ?--saveheap = #f
    level = 0
    savepath = #f
    ?--verbose = #f
    ?-- = #f
    compact? = #t
    a = "aaa"
    b = "bbb"
    c = 3276
    d = (3 4 5)
  > (foo '("/usr/local/bin/foo" "aaa" "bbb" "ccc"))
  usage: foo --version
         foo --help helplevel
         foo --help
         foo [ --boot|-b bootpath ] [ --compact|-c ] [ --heap|-h heappath ] [ --quiet|-q ] [ --saveheap|-s level savepath ] [ --verbose ] [ -- ] a [ b ] [ c ] d ...
  > (foo '("/usr/local/bin/foo" "aaa" "bbb" "#xccc" "3" "4" "5" "ddd"))
  usage: foo --version
         foo --help helplevel
         foo --help
         foo [ --boot|-b bootpath ] [ --compact|-c ] [ --heap|-h heappath ] [ --quiet|-q ] [ --saveheap|-s level savepath ] [ --verbose ] [ -- ] a [ b ] [ c ] d ...
  > (foo '("/usr/local/bin/foo" "-q" "--boot" "foo.boot" "aaa"))
  registering boot file "foo.boot"
    ?--boot = #t
    bootpath = "foo.boot"
    ?--compact = #f
    ?--heap = #f
    heappath = #f
    ?--quiet = #t
    ?--saveheap = #f
    level = 0
    savepath = #f
    ?--verbose = #f
    ?-- = #f
    compact? = #t
    a = "aaa"
    b = #f
    c = 666
    d = ()
  > (foo '("/usr/local/bin/foo" "-q" "--boot" "foo.boot" "-b"
          "--heap" "-h" "foo.heap" "-s" "7" "foo7.heap" "-c" "-c" "-c"
          "--verbose" "aaa"))
  registering boot file "foo.boot"
  registering boot file "--heap"
  registering boot file "foo.heap"
    ?--boot = #t
    bootpath = "--heap"
    ?--compact = #t
    ?--heap = #t
    heappath = "foo.heap"
    ?--quiet = #t
    ?--saveheap = #t
    level = 7
    savepath = "foo7.heap"
    ?--verbose = #t
    ?-- = #f
    compact? = #f
    a = "aaa"
    b = #f
    c = 666
    d = ()

  (command-line-case (cons "/usr/local/bin/foo" (command-line-arguments))
    [((keyword --build ifn)
      (flags [--verify] [(--output -o) (string ofn "a.out")])
      lib* ...)
     ---]
    [((keyword (--query -q) ifn))
     ---]
    [((flags [(--verify -v)] [(--output -o) (string ofn "a.out")])
      (string x*) ...)
     ---]
    [((flags [(--verify -v)] [(--output -o) ofn])
      (flags [--])
      x* ...)
     ---]
    [(x (optional integer y 0))
     ---]
    [(x (optional integer y) x* ...)
     ---]
  )

  |#

  (define-syntax command-line-case
    (lambda (x)
     ;; Internal representation:
     ;;   ({ keyword | flags }* reqarg* optarg* restarg?)
     ;;   kwd's are strings
     ;;   vars's are syntax objects (identifiers)
     ;;   defaults are syntax objects
     ;;   actions are syntax objects or #f
      (define-record pkeyword ((immutable kwd*)
                               (immutable reqarg*)
                               (immutable action)))
      (define-record pflags ((immutable flag*)))
      (define-record pflag ((immutable kwd*)
                            (immutable ?var)
                            (immutable optarg*)
                            (immutable action)))
      (define-record preqarg ((immutable type)
                              (immutable var)))
      (define-record poptarg ((immutable type)
                              (immutable var)
                              (immutable default)))
      (define-record prestarg ((immutable reqarg)))
      (define-record pend ())

      (module (parse-cmdspec)
        (define (parse-cmdspec cmdspec)
          (unless (syntax-case cmdspec () [(x ...) #t] [_ #f])
            (syntax-error cmdspec "improper argument declaration list"))
          (let parse-cmdspec ([cmdspec cmdspec])
            (syntax-case cmdspec (keyword flags $)
              [((keyword kwd reqarg ... $ actionexpr) . cmdspec)
               (cons (make-pkeyword
                       (map id->string (parse-kwd #'kwd))
                       (map parse-reqarg
                            (syntax->list
                              #'(reqarg ...)))
                       #'actionexpr)
                     (parse-cmdspec #'cmdspec))]
              [((keyword kwd reqarg ...) . cmdspec)
               (cons (make-pkeyword
                       (map id->string (parse-kwd #'kwd))
                       (map parse-reqarg
                            (syntax->list
                              #'(reqarg ...)))
                       #f)
                     (parse-cmdspec #'cmdspec))]
              [((keyword . ignore1) . ignore2)
               (syntax-error (syntax-case cmdspec () [(x . r) #'x])
                 "invalid keyword declaration")]
              [((flags flagdecl ...) . cmdspec)
               (cons (make-pflags
                       (map (lambda (flagdecl)
                              (syntax-case flagdecl ($)
                                [(kwd flagarg ... $ actionexpr)
                                 (let ([kwd* (parse-kwd #'kwd)])
                                   (make-pflag
                                     (map id->string kwd*)
                                     (make-?var (car kwd*))
                                     (map parse-flagarg
                                          (syntax->list #'(flagarg ...)))
                                     #'actionexpr))]
                                [(kwd flagarg ...)
                                 (let ([kwd* (parse-kwd #'kwd)])
                                   (make-pflag
                                     (map id->string kwd*)
                                     (make-?var (car kwd*))
                                     (map parse-flagarg
                                          (syntax->list #'(flagarg ...)))
                                     #f))]
                                [_ (syntax-error flagdecl
                                     "invalid flag declaration")]))
                            (syntax->list #'(flagdecl ...))))
                     (parse-cmdspec #'cmdspec))]
              [((flags . ignore1) . ignore2)
               (syntax-error (syntax-case cmdspec () [(x . r) #'x])
                 "invalid flags declaration")]
              [argspec (parse-argspec #'argspec)])))
        (define (parse-kwd kwd)
          (syntax-case kwd ()
            [id (identifier? #'id) (list #'id)]
            [(id1 id2 ...) (syntax->list #'(id1 id2 ...))]
            [_ (syntax-error kwd "invalid kwd specifier")]))
        (define (parse-argspec argspec)
          (define (dots? x)
            (and (identifier? x)
                 (literal-identifier=? x #'(... ...))))
          (syntax-case argspec ()
            [() (list (make-pend))]
            [(reqarg dots)
             (dots? #'dots)
             (list (make-prestarg (parse-reqarg #'reqarg)))]
            [(arg . argspec)
             (cons (parse-arg #'arg) (parse-argspec #'argspec))]
            [(x . r) (syntax-error #'x "invalid argument declaration")]))
        (define (parse-arg arg)
          (syntax-case arg (optional)
            [(optional . stuff) (parse-optarg arg)]
            [_ (parse-reqarg arg)]))
        (define (parse-reqarg reqarg)
          (syntax-case reqarg ()
            [var (identifier? #'var) (make-preqarg #'string #'var)]
            [(type var)
             (and (identifier? #'type) (identifier? #'var))
             (make-preqarg #'type #'var)]
            [x (syntax-error reqarg "invalid argument specifier")]))
        (define (parse-optarg optarg)
          (syntax-case optarg (optional)
            [(optional var)
             (identifier? #'var)
             (make-poptarg #'string #'var #'#f)]
            [(optional type var)
             (and (identifier? #'type) (identifier? #'var))
             (make-poptarg #'type #'var #'#f)]
            [(optional type var default)
             (and (identifier? #'type) (identifier? #'var))
             (make-poptarg #'type #'var #'default)]
            [x (syntax-error optarg "invalid optional argument specifier")]))
        (define (parse-flagarg flagarg)
          (syntax-case flagarg ()
            [var (identifier? #'var) (make-poptarg #'string #'var #'#f)]
            [(type var)
             (and (identifier? #'type) (identifier? #'var))
             (make-poptarg #'type #'var #'#f)]
            [(type var default)
             (and (identifier? #'type) (identifier? #'var))
             (make-poptarg #'type #'var #'default)]
            [x (syntax-error flagarg "invalid flag argument specifier")]))
        (define (id->string x) (symbol->string (syntax-object->datum x)))
        (define (make-?var x)
          (datum->syntax-object x
            (string->symbol (format "?~a" (id->string x))))))

      (module (usage-printer)
        (define (usage-printer cmdspec+)
          #`(lambda (cl)
              (let ([who (path-last (car cl))])
                #,(usage-cmdspec "usage:" (car cmdspec+))
                #,@(map (lambda (cmdspec) (usage-cmdspec "      " cmdspec)) (cdr cmdspec+)))))
        (define (usage-cmdspec leader cmdspec)
          (define cmdspec-printer
            (lambda (s*)
              #`(printf #,(format "~a ~~a~~a\n" leader) who #,(apply string-append s*))))
          (let-values ([(s* flag**) (usage-cmdspec-helper #t cmdspec)])
            (assert (null? flag**))
            (if (< (apply + (string-length leader) (map string-length s*)) 80)
                (cmdspec-printer s*)
                (let-values ([(s* flag**) (usage-cmdspec-helper #f cmdspec)])
                  (fold-left
                    (lambda (expr flag*)
                      (let ([flag-header (car flag*)] [flag* (cdr flag*)])
                        #`(begin
                            #,expr
                            (display-string #,(format "         where each ~a is one of:\n~{          ~a\n~}"
                                                flag-header
                                                (map (lambda (kwd* optarg*)
                                                       (format " ~a~{~a~}"
                                                         (usage-kwd* kwd*)
                                                         (map usage-optarg optarg*)))
                                                  (map pflag-kwd* flag*)
                                                  (map pflag-optarg* flag*)))))))
                    (cmdspec-printer s*)
                    flag**)))))
        (define (usage-cmdspec-helper inline-flags? cmdspec)
          (let loop ([cmdspec cmdspec]
                     [rs* '()]
                     [rflag** '()]
                     [flagsno (and (not inline-flags?)
                                   (cond [(memp pflags? cmdspec) => (lambda (cmdspec) (memp pflags? (cdr cmdspec)))] [else #f])
                                   1)])
            (if (null? cmdspec)
                (values (reverse rs*) (reverse rflag**))
                (let ([x (car cmdspec)])
                  (if (pflags? x)
                      (if inline-flags?
                          (loop (cdr cmdspec)
                            (cons (let ([flag* (pflags-flag* x)])
                                    (format "~{~a~}"
                                      (map (lambda (kwd* optarg*)
                                             (format " [ ~a~{~a~} ]"
                                               (usage-kwd* kwd*)
                                               (map usage-optarg optarg*)))
                                        (map pflag-kwd* flag*)
                                        (map pflag-optarg* flag*))))
                              rs*)
                            rflag**
                            flagsno)
                          (let ([flag-header (if flagsno (format "flag~s" flagsno) "flag")])
                            (loop (cdr cmdspec)
                              (cons (format " ~a ..." flag-header) rs*)
                              (cons (cons flag-header (pflags-flag* x)) rflag**)
                              (and flagsno (+ flagsno 1)))))
                      (loop (cdr cmdspec)
                        (cons (cond
                                [(pkeyword? x)
                                 (format " ~a~{~a~}"
                                   (usage-kwd* (pkeyword-kwd* x))
                                   (map usage-reqarg (pkeyword-reqarg* x)))]
                                [(preqarg? x) (usage-reqarg x)]
                                [(poptarg? x) (format " [~a ]" (usage-optarg x))]
                                [(prestarg? x) (format "~a ..." (usage-reqarg (prestarg-reqarg x)))]
                                [(pend? x) ""]
                                [else (errorf 'usage-cmdspec "unrecognized cmdspec ~s" x)])
                          rs*)
                        rflag**
                        flagsno))))))
        (define (usage-kwd* kwd*) (format "~a~{|~a~}" (car kwd*) (cdr kwd*)))
        (define (usage-reqarg x)
          (format " ~a" (syntax-object->datum (preqarg-var x))))
        (define (usage-optarg x)
          (format " ~a" (syntax-object->datum (poptarg-var x)))))

      (define (xmap p ls tail)
        (if (null? ls)
            tail
            (p (car ls) (xmap p (cdr ls) tail))))

      (module (findvars)
        (define (findvars cmdspec)
          (findvars-cmdspec cmdspec '()))
        (define (findvars-cmdspec cmdspec tail)
          (xmap
            (lambda (x tail)
              (cond
                [(pkeyword? x) (xmap findvars-reqarg (pkeyword-reqarg* x) tail)]
                [(pflags? x)
                 (xmap (lambda (flag tail)
                         (cons (pflag-?var flag)
                           (xmap findvars-optarg (pflag-optarg* flag) tail)))
                   (pflags-flag* x)
                   tail)]
                [(preqarg? x) (findvars-reqarg x tail)]
                [(poptarg? x) (findvars-optarg x tail)]
                [(prestarg? x) (findvars-reqarg (prestarg-reqarg x) tail)]
                [(pend? x) tail]
                [else (errorf 'findvars-cmdspec "unrecognized cmdspec ~s" x)]))
            cmdspec
            tail))
        (define (findvars-reqarg x tail)
          (cons (preqarg-var x) tail))
        (define (findvars-optarg x tail)
          (cons (poptarg-var x) tail)))

      (module (finddefaults)
        (define (finddefaults cmdspec)
          (finddefaults-cmdspec cmdspec '()))
        (define (finddefaults-cmdspec cmdspec tail)
          (xmap
            (lambda (x tail)
              (cond
                [(pkeyword? x)
                 (xmap finddefaults-reqarg (pkeyword-reqarg* x) tail)]
                [(pflags? x)
                 (xmap (lambda (flag tail)
                         (cons #'#f
                           (xmap finddefaults-optarg (pflag-optarg* flag) tail)))
                   (pflags-flag* x)
                   tail)]
                [(preqarg? x) (finddefaults-reqarg x tail)]
                [(poptarg? x) (finddefaults-optarg x tail)]
                [(prestarg? x) (cons #''() tail)]
                [(pend? x) tail]
                [else (errorf 'finddefaults-cmdspec "unrecognized cmdspec ~s" x)]))
            cmdspec
            tail))
        (define (finddefaults-reqarg x tail)
          (cons #'(void) tail))
        (define (finddefaults-optarg x tail)
          (cons (poptarg-default x) tail)))

      (module (build-clause)
        (define (type->converter x)
          (if (and x (not (literal-identifier=? x #'string)))
              (datum->syntax-object x
                (string->symbol
                  (format "string->~a" (syntax-object->datum x))))
              #'values))
        (define (build-clause-body var* cmdspec body)
          (with-syntax ([(var ...) var*])
            (let ([x (car cmdspec)])
              (cond
                [(pkeyword? x)
                 (let ([reqarg* (pkeyword-reqarg* x)])
                   (with-syntax ([(kwd ...) (pkeyword-kwd* x)]
                                 [(reqvar ...) (map preqarg-var reqarg*)]
                                 [(convert ...)
                                  (map type->converter (map preqarg-type reqarg*))]
                                 [action (or (pkeyword-action x) #'(#2%void))]
                                 [finish (build-clause-body var* (cdr cmdspec) body)]
                                 [n (length reqarg*)])
                     #'(cond
                         [(and (> (length cl) n) (member (car cl) '(kwd ...)))
                          (let ([cl (cdr cl)])
                            (apply (lambda (reqvar ... . ignore)
                                    ; performs unnecessary tests if convert
                                    ; is values; doesn't shortcut out as
                                    ; soon as one convert returns false
                                     (let ([reqvar (convert reqvar)] ...)
                                       (if (and reqvar ...)
                                           (let ([cl (list-tail cl n)]
                                                 [act! (lambda () (act!) action)])
                                             finish)
                                           (next orig-cl))))
                              cl))]
                         [else (next orig-cl)])))]
                [(pflags? x)
                 (let* ([flag* (pflags-flag* x)]
                        [optarg** (map pflag-optarg* flag*)])
                   (with-syntax ([((kwd ...) ...) (map pflag-kwd* flag*)]
                                 [(?var ...) (map pflag-?var flag*)]
                                 [((optvar ...) ...)
                                  (map (lambda (optarg*)
                                         (map poptarg-var optarg*))
                                       optarg**)]
                                 [((convert ...) ...)
                                  (map (lambda (optarg*)
                                         (map type->converter
                                              (map poptarg-type optarg*)))
                                       optarg**)]
                                 [(action ...)
                                  (map (lambda (flag)
                                         (or (pflag-action flag) #'(#2%void)))
                                       flag*)]
                                 [finish (build-clause-body var* (cdr cmdspec) body)]
                                 [(n ...) (map length optarg**)])
                   #'(let ([t (lambda (cl act! ?var ... optvar ... ...) finish)])
                       (let f ([cl cl]
                               [act! act!]
                               [?var ?var] ...
                               [optvar optvar] ... ...)
                         (cond
                           [(null? cl) (t cl act! ?var ... optvar ... ...)]
                           [(and (> (length cl) n) (member (car cl) '(kwd ...)))
                            (let ([cl (cdr cl)])
                              (apply (lambda (optvar ... . ignore)
                                      ; performs unnecessary tests if convert
                                      ; is values; doesn't shortcut out as
                                      ; soon as one convert returns false
                                       (let ([optvar (convert optvar)] ...)
                                         (if (and optvar ...)
                                             (let ([?var #t])
                                               (f (list-tail cl n)
                                                  (lambda () (act!) action)
                                                  ?var ...
                                                  optvar ... ...))
                                             (next orig-cl))))
                                     cl))]
                           ...
                           [else (t cl act! ?var ... optvar ... ...)])))))]
                [(preqarg? x)
                 (with-syntax ([reqvar (preqarg-var x)]
                               [convert (type->converter (preqarg-type x))]
                               [finish (build-clause-body var* (cdr cmdspec) body)])
                   #'(cond
                      ; performs unnecessary test if convert is values
                       [(and (not (null? cl)) (convert (car cl))) =>
                        (lambda (reqvar) (let ([cl (cdr cl)]) finish))]
                       [else (next orig-cl)]))]
                [(poptarg? x)
                 (with-syntax ([optvar (poptarg-var x)]
                               [convert (type->converter (poptarg-type x))]
                               [finish (build-clause-body var* (cdr cmdspec) body)])
                   #'(let ([t (lambda (optvar cl) finish)])
                       (cond
                         [(null? cl) (t optvar cl)]
                        ; performs unnecessary test if convert is values
                         [(convert (car cl)) => (lambda (optvar) (t optvar (cdr cl)))]
                         [else (next orig-cl)])))]
                [(prestarg? x)
                 (let ([reqarg (prestarg-reqarg x)])
                   (with-syntax ([restvar (preqarg-var reqarg)]
                                 [convert (type->converter (preqarg-type reqarg))]
                                 [body body])
                     #'(let f ([cl cl] [rrestvar '()])
                         (cond
                           [(null? cl)
                            (let ([restvar (reverse rrestvar)])
                              (let () (act!) . body))]
                          ; performs unnecessary test if convert is values
                           [(convert (car cl)) =>
                            (lambda (x) (f (cdr cl) (cons x rrestvar)))]
                           [else (next orig-cl)]))))]
                [(pend? x)
                 (with-syntax ([body body])
                   #'(if (null? cl)
                         (let () (act!) . body)
                         (next orig-cl)))]
                [else (errorf 'build-clause-body "unrecognized cmdspec ~s" x)]))))
        (define (build-clause cmdspec body next-expr)
          (let ([var* (findvars cmdspec)])
            (with-syntax ([next-expr next-expr]
                          [(var ...) var*]
                          [(default ...) (finddefaults cmdspec)]
                          [clause-body (build-clause-body var* cmdspec body)])
            #'(let ([next next-expr])
                (lambda (orig-cl)
                  (let ([cl (cdr orig-cl)] [act! #2%void] [var default] ...)
                    clause-body)))))))

      (syntax-case x ()
        [(k clexpr [cmdspeca b1a b2a ...] [cmdspec b1 b2 ...] ...)
         (let ([all-cmdspec* (map parse-cmdspec
                               (cons #'cmdspeca
                                 (syntax->list #'(cmdspec ...))))]
               [body* #'((b1a b2a ...) (b1 b2 ...) ...)])
           (with-implicit (k usage)
             #`(let ([usage-proc #,(usage-printer all-cmdspec*)] [cl clexpr])
                 (let ([usage (lambda () (usage-proc cl))])
                   #,(with-syntax ([p (let f ([cmdspec* all-cmdspec*] [body* body*])
                                        (if (null? cmdspec*)
                                            #'(lambda (cl) (usage-proc cl) (exit 1))
                                            (build-clause (car cmdspec*) (car body*)
                                               (f (cdr cmdspec*) (cdr body*)))))])
                        #'(p cl))))))])))

  ;;; (run-script script arg ...) runs named script in same process
  (define (run-script script . arg*)
    (command-line-arguments arg*)
    (load script))
)
