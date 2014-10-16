#lang racket/base

(require tests/eli-tester (prefix-in scr: scribble/reader) racket/list)

(define the-tests #<<END-OF-TESTS

;; format:
;; * a line with only `-'s marks the boundary between tests
;; * -<token>-> marks a <token> kind of reader test
;;   (put on a new line if whitespace matters)
;; * lines with semicolon comments flushed at the left column ignored,

---
;; -------------------- simple uses, test identifiers
---
@foo  -@->  foo
---
@foo{}  -@->  (foo)
---
@foo[]  -@->  (foo)
---
@foo[]{}  -@->  (foo)
---
foo@  -@->  foo@
---
fo@o  -@->  fo@o
---
\@foo  -@->  @foo
---
|@foo|  -@->  @foo
---
@foo@bar  -@-> foo@bar
---
@foo@bar.  -@-> foo@bar.
---
@foo@bar:  -@-> foo@bar:
---
@foo@bar;  -@-> foo@bar
---
@foo[]@bar{}  -@-> (foo) (bar)
---
@foo{foo@|bar|.}
-@->
(foo "foo" bar ".")
---
@foo{foo@bar;}
-@->
(foo "foo" bar ";")
---
(define |@foo| '\@bar@baz)  -@-> (define @foo '@bar@baz)
---
@foo{foo@2.}
-@->
(foo "foo" 2.0)
---
;; -------------------- simple args and texts
---
@foo{bar}  -@->  (foo "bar")
---
@foo[]{bar}  -@->  (foo "bar")
---
@foo[bar]  -@->  (foo bar)
---
@foo[bar]{}  -@->  (foo bar)
---
@foo[bar][baz]  -@->  (foo bar) [baz]
---
@foo[bar]{baz}  -@->  (foo bar "baz")
---
@foo[bar]{baz}[blah]  -@->  (foo bar "baz") [blah]
---
@foo[bar]{baz}@foo[blah]  -@->  (foo bar "baz") (foo blah)
---
@foo[#:x y]{bar}  -@->  (foo #:x y "bar")
---
@foo[1 (* 2 3)]{bar}  -@->  (foo 1 (* 2 3) "bar")
---
@foo[@bar{...}]{blah}
-@->
(foo (bar "...") "blah")
---
;; -------------------- no exprs or text
---
@{}  -@->  ()
---
@[]  -@->  ()
---
@{foo}  -@->  ("foo")
---
@[foo]  -@->  (foo)
---
@{@foo bar}  -@->  (foo " bar")
---
@|{blah}|  -@->  ("blah")
---
@|{blah|@foo bleh}|  -@-> ("blah" foo " bleh")
---
@|{|@meh blah|@foo bleh}|  -@-> (meh " blah" foo " bleh")
---
;; -------------------- newlines and spaces in text
---
@foo{bar baz}  -@->  (foo "bar baz")
---
@foo{bar  baz}  -@->  (foo "bar  baz")
---
@foo{ bar }  -@->  (foo " bar ")
---
@foo{  bar   }  -@->  (foo "  bar   ")
---
@foo{ }  -@->  (foo " ")
---
@foo{  }  -@->  (foo "  ")
---
@foo[1]{bar baz}  -@->  (foo 1 "bar baz")
---
@foo[1]{bar  baz}  -@->  (foo 1 "bar  baz")
---
@foo[1]{ bar }  -@->  (foo 1 " bar ")
---
@foo[1]{  bar   }  -@->  (foo 1 "  bar   ")
---
@foo[1]{ }  -@->  (foo 1 " ")
---
@foo[1]{  }  -@->  (foo 1 "  ")
---
@foo{bar baz
     blah}
-@->
(foo "bar baz" "\n" "blah")
---
@foo[1]{bar baz
        blah}
-@->
(foo 1 "bar baz" "\n" "blah")
---
@foo{bar baz

     blah}
-@->
(foo "bar baz" "\n" "\n" "blah")
---
@foo{bar baz


     blah}
-@->
(foo "bar baz" "\n" "\n" "\n" "blah")
---
@foo{bar
     }
-@->
(foo "bar")
---
@foo{
     bar}
-@->
(foo "bar")
---
@foo{
     bar
     }
-@->
(foo "bar")
---
@foo{

     bar
     }
-@->
(foo "\n" "bar")
---
@foo{
     bar

     }
-@->
(foo "bar" "\n")
---
@foo{

     bar

     }
-@->
(foo "\n" "bar" "\n")
---
@foo{
     }
-@->
(foo "\n")
---
@foo{

     }
-@->
(foo "\n" "\n")
---
@foo{


     }
-@->
(foo "\n" "\n" "\n")
---
;; -------------------- nested forms
---
@foo{@bar}  -@->  (foo bar)
---
@foo{@bar{}}  -@->  (foo (bar))
---
@foo{111@bar{222}333}  -@->  (foo "111" (bar "222") "333")
---
@foo{111@bar[222]333}  -@->  (foo "111" (bar 222) "333")
---
@foo[111 @bar{222} 333]  -@->  (foo 111 (bar "222") 333)
---
@foo[111 @bar{222}333]  -@->  (foo 111 (bar "222") 333)
---
@foo[111 @bar[222]333]  -@->  (foo 111 (bar 222) 333)
---
@foo[111 @bar 222]  -@->  (foo 111 bar 222)
---
@foo{111 @bar 222}  -@->  (foo "111 " bar " 222")
---
@foo{@bar 111}  -@->  (foo bar " 111")
---
@foo{111 @bar}  -@->  (foo "111 " bar)
---
@foo{ @bar }  -@->  (foo " " bar " ")
---
@foo{bar @baz[3]
     blah}
-@->
(foo "bar " (baz 3) "\n" "blah")
---
@foo{bar @baz{3}
     blah}
-@->
(foo "bar " (baz "3") "\n" "blah")
---
@foo{bar @baz[2 3]{4 5}
     blah}
-@->
(foo "bar " (baz 2 3 "4 5") "\n" "blah")
---
@foo{bar @baz[2 3] {4 5}}
-@->
(foo "bar " (baz 2 3) " {4 5}")
---
;; -------------------- cannot have spaces before args or text
---
@foo [bar]  -@-> foo (bar)
---
@foo {bar}  -@-> foo (bar)
---
@foo[bar] {baz}  -@-> (foo bar) (baz)
---
@foo{bar @baz {bleh}}  -@-> (foo "bar " baz " {bleh}")
---
;; -------------------- expression escapes, operators, currying
---
@foo{1 @(+ 2 3) 4}  -@-> (foo "1 " (+ 2 3) " 4")
---
@(lambda (x) x){blah}  -@->  ((lambda (x) x) "blah")
---
@(lambda (x) x)[blah]  -@->  ((lambda (x) x) blah)
---
@foo{bar}{baz}  -@->  (foo "bar") (baz)
---
@@foo{bar}{baz}  -@->  ((foo "bar") "baz")
---
@@foo{bar} {baz}  -@->  (foo "bar") (baz)
---
@@foo{bar}{baz}{}  -@->  ((foo "bar") "baz") ()
---
@@@foo{bar}{baz}{}  -@->  (((foo "bar") "baz"))
---
@@@foo[]{}[][]  -@->  (((foo)))
---
@@@foo[]{}[][][]  -@->  (((foo))) ()
---
@foo{foo@|3|.}
-@->
(foo "foo" 3 ".")
---
@foo{foo@|(f 1)|{bar}}
-@->
(foo "foo" (f 1) "{bar}")
---
@foo{foo@|bar|[1]{baz}}
-@->
(foo "foo" bar "[1]{baz}")
---
;; -------------------- pulling punctuations outside
---
@'foo  -@->  'foo
---
@'foo[1 2]  -@->  '(foo 1 2)
---
@'foo{bar}  -@->  '(foo "bar")
---
@`foo{bar}  -@->  `(foo "bar")
---
@,foo{bar}  -@->  ,(foo "bar")
---
@,@foo{bar}  -@->  ,@(foo "bar")
---
@`',foo{bar}  -@->  `',(foo "bar")
---
@`',`',foo{bar}  -@->  `',`',(foo "bar")
---
@``'',,foo{bar}  -@->  ``'',,(foo "bar")
---
@`',@foo{bar}  -@->  `',@(foo "bar")
---
@`',@`',@foo{bar}  -@->  `',@`',@(foo "bar")
---
@``'',@,@foo{bar}  -@->  ``'',@,@(foo "bar")
---
@``'',,,@,@foo{bar}  -@->  ``'',,,@,@(foo "bar")
---
@#'foo{bar}  -@->  #'(foo "bar")
---
@#`foo{bar}  -@->  #`(foo "bar")
---
@#,foo{bar}  -@->  #,(foo "bar")
---
@#''foo{bar}  -@->  #''(foo "bar")
---
@#`'#,foo{bar}  -@->  #`'#,(foo "bar")
---
@`foo{123 @,bar{456} 789}
-@->
`(foo "123 " ,(bar "456") " 789")
---
@`(unquote foo){blah}
-@->
`(,foo "blah")
---
;; -------------------- balanced braces are allowed
---
@foo{f{o}o}  -@->  (foo "f{o}o")
---
@foo{{{}}{}}  -@->  (foo "{{}}{}")
---
@foo{f[o]o}  -@->  (foo "f[o]o")
---
@foo{[{}]{}}  -@->  (foo "[{}]{}")
---
;; -------------------- string escapes
---
@foo{x@"y"z}  -@->  (foo "xyz")
---
@foo{A @"}" marks the end}
-@->
(foo "A } marks the end")
---
@foo{The prefix is: @"@".}
-@->
(foo "The prefix is: @.")
--
@foo{@"@x{y}" => (x "y")}
-@->
(foo "@x{y} => (x \"y\")")
---
;; -------------------- alternative delimiters
---
@foo|{...}|  -@->  (foo "...")
---
@foo|{"}" after "{"}|  -@->  (foo "\"}\" after \"{\"")
---
@foo|{Nesting |{is}| ok}|  -@->  (foo "Nesting |{is}| ok")
---
@foo|{Nested @form{not}}|  -@->  (foo "Nested @form{not}")
---
@foo|{Nested |@form|{yes}|}|  -@->  (foo "Nested " (form "yes"))
---
@foo|{Nested |@form{indep@{end}ence}}|
-@->
(foo "Nested " (form "indep" ("end") "ence"))
---
@foo|{Nested |@|name|}|  -@->  (foo "Nested " name)
---
@foo|{With
      |@bar{multiple}
      lines.}|
-@->
(foo "With" "\n" (bar "multiple") "\n" "lines.")
---
@t|{In |@i|{sub|@"@"s}| too}|  -@->  (t "In " (i "sub@s") " too")
---
@foo|<<<{@x{foo} |@{bar}|.}>>>|  -@->  (foo "@x{foo} |@{bar}|.")
---
@foo|<<<{@x{foo} |<<<@{bar}|.}>>>|  -@->  (foo "@x{foo} " ("bar") "|.")
---
@foo|!!{X |!!@b{Y}...}!!|  -@->  (foo "X " (b "Y") "...")
---
;; -------------------- comments
---
(1 2 @; comment
 3 4)
-@->
(1 2 3 4)
---
@foo{bar @; comment
     baz@;
     blah}
-@->
(foo "bar bazblah")
---
@foo{bar @; comment, with space and newline

     baz}
-@->
(foo "bar " "\n" "baz")
---
hello @; comment at eof
-@->
hello
---
@foo{bar @;{a balanced comment} baz}
-@->
(foo "bar  baz")
---
@foo|{bar @;{a non-comment} baz}|
-@->
(foo "bar @;{a non-comment} baz")
---
@foo|{bar |@;{a balanced comment again} baz}|
-@->
(foo "bar  baz")
---
@foo{First line@;{there is still a
                  newline here;}
     Second line}
-@->
(foo "First line" "\n" "Second line")
---
@foo{A long @;
     single-@;
     string arg.}
-@->
(foo "A long single-string arg.")
---
;; -------------------- indentation management
---
@foo{ bar
     baz }
-@->
(foo " bar" "\n" "baz ")
---
@foo{bar
}
-@->
(foo "bar")
---
@foo{
bar}
-@->
(foo "bar")
---
@foo{
  bar
}
-@->
(foo "bar")
---
@foo{

  bar

}
-@->
(foo "\n" "bar" "\n")
---
@foo{
  bar

  baz
}
-@->
(foo "bar" "\n" "\n" "baz")
---
@foo{
}
-@->
(foo "\n")
---
@foo{
  bar
  baz
  blah
}
-@->
(foo "bar" "\n" "baz" "\n" "blah")
---
@foo{
  begin
    x++;
  end}
-@->
(foo "begin" "\n" "  " "x++;" "\n" "end")
---
@foo{
    a
   b
  c}
-@->
(foo "  " "a" "\n" " " "b" "\n" "c")
---
@foo{bar
       baz
     bbb}
-@->
(foo "bar" "\n" "  " "baz" "\n" "bbb")
---
;; requires location tracking
@foo{ bar
        baz
      bbb}
-@->
(foo " bar" "\n" "   " "baz" "\n" " " "bbb")
---
@foo{bar
   baz
   bbb}
-@->
(foo "bar" "\n" "baz" "\n" "bbb")
---
@foo{ bar
   baz
   bbb}
-@->
(foo " bar" "\n" "baz" "\n" "bbb")
---
@foo{ bar
   baz
     bbb}
-@->
(foo " bar" "\n" "baz" "\n" "  " "bbb")
---
@text{Some @b{bold
  text}, and
  more text.}
-@->
(text "Some " (b "bold" "\n" "text") ", and" "\n" "more text.")
---
@code{
  begin
    i = 1, r = 1
    @bold{while i < n do
            r *= i++
          done}
  end
}
-@->
(code "begin" "\n"
      "  " "i = 1, r = 1" "\n"
      "  " (bold "while i < n do" "\n"
                 "  " "r *= i++" "\n"
                 "done") "\n"
      "end")
---
@foo{x1
     x2@;
              y2
     x3@;{
              ;}y3
     x4@|
              |y4
     x5}
-@->
(foo "x1" "\n" "x2y2" "\n" "x3y3" "\n" "x4" "y4" "\n" "x5")
---
;; -------------------- ||-quotes for artificial separators and multi-exprs
---
@foo{x@||z}  -@->  (foo "x" "z")
---
@foo{x@|"y"|z}  -@->  (foo "x" "y" "z")
---
@foo{x@|"y" "z"|}  -@->  (foo "x" "y" "z")
---
@foo{x@|1 (+ 2 3) 4|y}  -@->  (foo "x" 1 (+ 2 3) 4 "y")
---
@foo{x@|*
        *|y}
-@->
(foo "x" * * "y")
---
@foo{Alice@||Bob@|
     |Carol}
-@->
(foo "Alice" "Bob" "Carol")
---
@foo{Alice@||Bob@| x
     |Carol}
-@->
(foo "Alice" "Bob" x "Carol")
---
@foo{@||
     bar
     @||}
-@->
(foo "\n" "bar" "\n")
---
@foo{
  @|| bar @||
  @|| baz}
-@->
(foo " bar " "\n" " baz")
---
@foo{bar
     @|baz| bbb
     @|x1 x2| x3 x4
     @|| waaaah
    }
-@->
(foo "bar" "\n" baz " bbb" "\n" x1 x2 " x3 x4" "\n" " waaaah")
---
;; -------------------- inside-reader
---
foo bar baz  -@i->  "foo bar baz"
---
foo @bar baz  -@i->  "foo " bar " baz"
---
foo @bar{blah} baz  -@i-> "foo " (bar "blah") " baz"
---
{{{  -@i->  "{{{"
---
}}}  -@i->  "}}}"
---
foo
  bar
baz
-@i->
"foo" "\n" "  " "bar" "\n" "baz"
---
  foo
    bar
  baz
-@i->
"  foo" "\n" "    " "bar" "\n" "  " "baz"
---
;; -------------------- using a different command character
---
\foo
-\->
foo
---
\foo[1]{bar
          baz \nested|{\form{}}|
        blah}
-\->
(foo 1 "bar" "\n" "  " "baz " (nested "\\form{}") "\n" "blah")
---
\foo
-\i->
foo
---
\foo[1]{bar
          baz \nested|{\form{}}|
        blah}
\bar[]
-\i->
(foo 1 "bar" "\n" "  " "baz " (nested "\\form{}") "\n" "blah") "\n" (bar)
---
;; -------------------- syntax information
---
foo
-@syntax-> (stx: line= 1 column= 0 position= 1 span= 3)
---
\foo
|foo|
-@syntax->
(stx: line= 1 column= 0 position= 1 span= 4)
(stx: line= 2 column= 0 position= 6 span= 5)
---
(foo bar)
-@syntax-> ((stx: line= 1 column= 1 position= 2 span= 3)
            (stx: line= 1 column= 5 position= 6 span= 3))
---
;; this test should break soon
@foo
-@syntax->
(stx: line= 1 column= 1 position= 2 span= 3)
;; NOT this: (stx: line= 1 column= 0 position= 1 span= 4)
---
;; -------------------- errors
---
(  -@error-> "inp:1:0: read: expected a `)' to close `('" ; check -@error->
---
@foo{ -@error-> #rx":1:0: missing closing `}'$"
---
\foo{ -\error-> #rx":1:0: missing closing `}'$"
---
@foo{@bar{ -@error-> #rx":1:5: missing closing `}'$"
---
\foo{\bar{ -\error-> #rx":1:5: missing closing `}'$"
---
@foo{@bar{} -@error-> #rx":1:0: missing closing `}'$"
---
@foo{@bar|{} -@error-> #rx":1:5: missing closing `}\\|'$"
---
@foo{@bar|-{} -@error-> #rx":1:5: missing closing `}-\\|'$"
---
@foo{@bar|-{} -@error-> #rx":1:5: missing closing `}-\\|'$"
---
\foo{\bar|-{} -\error-> #rx":1:5: missing closing `}-\\|'$"
---
@foo{@" -@error-> #rx":1:6: read: expected a closing '\"'$"
;; " <-- (balance this file)
---
\foo{\" -\error-> #rx":1:6: read: expected a closing '\"'$"
---
@|1 2|
-@error->
#rx"a @|...| form in Scheme mode must have exactly one escaped expression"
---
@||
-@error->
#rx"a @|...| form in Scheme mode must have exactly one escaped expression"
---
\|1 2|
-\error->
#rx"a \\\\|...| form in Scheme mode must have exactly one escaped expression"
---
\||
-\error->
#rx"a \\\\|...| form in Scheme mode must have exactly one escaped expression"
---
;; -------------------- some code tests
---
@string-append{1 @(number->string (+ 2 3)) 4}  -@eval-> "1 5 4"
---
(let* ([formatter (lambda (fmt)
                    (lambda args (format fmt (apply string-append args))))]
       [bf (formatter "*~a*")]
       [it (formatter "/~a/")]
       [ul (formatter "_~a_")]
       [text string-append])
  @text{@it{Note}: @bf{This is @ul{not} a pipe}.})
-@eval->
"/Note/: *This is _not_ a pipe*."
---
(require (for-syntax scheme/base))
(let-syntax ([foo
              (lambda (stx)
                (let ([p (syntax-property stx 'scribble)])
                  (syntax-case stx ()
                    [(_ x ...)
                     (and (pair? p) (eq? (car p) 'form) (even? (cadr p)))
                     (let loop ([n (/ (cadr p) 2)]
                                [as '()]
                                [xs (syntax->list #'(x ...))])
                       (if (zero? n)
                         #`(list 'foo `#,(reverse as) #,@xs)
                         (loop (sub1 n)
                               (cons #`(#,(car xs) ,#,(cadr xs)) as)
                               (cddr xs))))])))])
  @foo[x 1 y (* 2 3)]{blah})
-@eval->
(foo ((x 1) (y 6)) "blah")
---
(let-syntax ([verb
              (lambda (stx)
                (syntax-case stx ()
                  [(_ cmd item ...)
                   #`(cmd
                      #,@(let loop ([items (syntax->list #'(item ...))])
                           (if (null? items)
                             '()
                             (let* ([fst  (car items)]
                                    [prop (syntax-property fst 'scribble)]
                                    [rst  (loop (cdr items))])
                               (cond [(eq? prop 'indentation) rst]
                                     [(not (and (pair? prop)
                                                (eq? (car prop)
                                                     'newline)))
                                      (cons fst rst)]
                                     [else (cons (datum->syntax
                                                  fst (cadr prop) fst)
                                                 rst)])))))]))])
  @verb[string-append]{
    foo
      bar
  })
-@eval->
"foo\n      bar"
---
;; -------------------- empty input tests
---

-@->

---

-@i->

---

-\->

---

-\i->

---


END-OF-TESTS
)

;; get a tester function

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (string->tester name) (eval (string->symbol name) ns))

;; reader utilities

(define the-name (string->path "inp"))

(define (read-all str reader [whole? #f])
  (define i (open-input-string str the-name))
  (if whole?
    (reader i)
    (let loop ()
      (let ([x (reader i)])
        (if (eof-object? x) '() (cons x (loop)))))))

(define read/BS (scr:make-at-reader #:command-char #\\ #:syntax? #f))
(define read-syntax/BS (scr:make-at-reader #:command-char #\\ #:syntax? #t))

(define read-inside/BS
  (scr:make-at-reader #:inside? #t #:command-char #\\ #:syntax? #f))

;; tester makers

(define (x . (mk-reader-test reader) . y)
  (values (read-all x reader) (read-all y read)))

(define (x . (mk-inside-reader-test inside-reader) . y)
  (values (read-all x inside-reader #t) (read-all y read)))

(define (x . (mk-eval-test syntax-reader) . y)
  (define r (void))
  (for ([x (read-all x (lambda (i) (syntax-reader 'test i)))])
    (set! r (call-with-values (lambda () (eval x ns)) list)))
  (values r (read-all y read)))

(define (x . (mk-syntax-test syntax-reader) . y)
  (let ([x (read-all x (lambda (i) (syntax-reader 'test i)))]
        [y (read-all y read)])
    (define (check x y)
      (cond [(or (equal? x y) (eq? y '_)) #t]
            [(not (pair? y)) #f]
            [(eq? 'stx: (car y)) (check-stx x (cdr y))]
            [(pair? x) (and (check (car x) (car y)) (check (cdr x) (cdr y)))]
            [(syntax? x) (check (syntax-e x) y)]
            [else #f]))
    (define (check-stx x y)
      (cond [(null? y) #t]
            [(null? (cdr y)) (check x (car y))]
            [(check
              ((case (car y)
                 [(line=)     syntax-line]
                 [(column=)   syntax-column]
                 [(position=) syntax-position]
                 [(span=)     syntax-span]
                 [else (error 'syntax-test "unknown test form: ~.s" (car y))])
               x)
              (cadr y))
             (check-stx x (cddr y))]
            [else #f]))
    (values #t (check x y))))

(define (x . (mk-error-test reader) . y)
  (define (get-exn-data e)
    (cons (exn-message e)
          null #;
          (append-map (lambda (s) (list (srcloc-line s) (srcloc-column s)))
                      (exn:fail:read-srclocs e))
          ))
  (values (with-handlers ([exn:fail:read? get-exn-data])
            (read-all x reader) "no error!")
          (read-all y read)))

;; testers

(define -@->        (mk-reader-test scr:read))
(define -\\->       (mk-reader-test read/BS))
(define -@i->       (mk-inside-reader-test scr:read-inside))
(define -\\i->      (mk-inside-reader-test read-inside/BS))
(define -@eval->    (mk-eval-test scr:read-syntax))
(define -\\eval->   (mk-eval-test read-syntax/BS))
(define -@syntax->  (mk-syntax-test scr:read-syntax))
(define -\\syntax-> (mk-syntax-test read-syntax/BS))
(define -@error->   (mk-error-test scr:read))
(define -\\error->  (mk-error-test read/BS))

(define (make-@+-readtable #:command-readtable [command-readtable (current-readtable)]
                           #:datum-readtable [datum-readtable (current-readtable)])
  (make-readtable (scr:make-at-readtable #:command-readtable command-readtable
                                         #:datum-readtable datum-readtable)
                  #\+ 'terminating-macro (lambda args 'PLUS)))
(define @+-readtable (make-@+-readtable))
(define @c+-readtable (make-@+-readtable #:command-readtable 'dynamic))
(define @d+-readtable (make-@+-readtable #:datum-readtable 'dynamic))
(define @cd+-readtable (make-@+-readtable #:command-readtable 'dynamic
                                          #:datum-readtable 'dynamic))

(define-syntax-rule (@+checker a b readtable)
  (equal? (parameterize ([current-readtable readtable])
            (read (open-input-string a)))
          b))
(define-syntax-rule (a . -@+> . b) (@+checker a b @+-readtable))
(define-syntax-rule (a . -@c+> . b) (@+checker a b @c+-readtable))
(define-syntax-rule (a . -@d+> . b) (@+checker a b @d+-readtable))
(define-syntax-rule (a . -@cd+> . b) (@+checker a b @cd+-readtable))

;; running the tests
(provide reader-tests)
(module+ main (reader-tests))
(define (reader-tests)
  (define (matching? x y)
    (cond [(equal? x y) #t]
          [(pair? x) (and (pair? y)
                          (matching? (car x) (car y))
                          (matching? (cdr x) (cdr y)))]
          [(and (regexp? x) (string? y)) (matching? y x)]
          [(and (string? x) (regexp? y)) (regexp-match? y x)]
          [(procedure? x) (x y)]
          [(procedure? y) (y x)]
          [else #f]))
  (test do
    (let* ([ts the-tests]
           ;; remove all comment lines
           [ts (regexp-replace* #px"(?m:^;.*\r?\n)" ts "")]
           ;; split the tests
           [ts (regexp-split #px"(?m:^)-+(?:$|\r?\n)" ts)])
      (parameterize ([port-count-lines-enabled #t])
        (for ([t ts] #:unless (regexp-match? #px"^\\s*$" t))
          (let ([m (or (regexp-match #px"^(.*)\n\\s*(-\\S+->)\\s*\n(.*)$"
                                     t)
                       (regexp-match #px"^(.*\\S)\\s+(-\\S+->)\\s+(\\S.*)$"
                                     t))])
            (if (not (and m (= 4 (length m))))
              (error 'bad-test "~a" t)
              (let-values ([(x y)
                            ((string->tester (caddr m)) (cadr m) (cadddr m))])
                (test #:failure-message
                      (format "bad result in\n    ~a\n  results:\n    ~s != ~s"
                              (regexp-replace* #rx"\n" t "\n    ")
                              x y)
                      (matching? x y))))))))

    ;; Check static versus dynamic readtable for command (dynamic when "c" in the
    ;; name) and datum (dynamic when "d" in the name) parts:
    (-@+> "10" 10)
    (-@+> "(+ @+[+] +)" '(PLUS (+ +) PLUS))
    (-@+> "@+[+]" '(+ +))
    (-@d+> "@+[+]" '(+ PLUS))
    (-@d+> "(+ @+[+])" '(PLUS (+ PLUS)))
    (-@c+> "@+[+]" '(PLUS +))
    (-@c+> "@|+|" 'PLUS)
    (-@cd+> "@+[+]" '(PLUS PLUS))))
