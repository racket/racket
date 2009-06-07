#lang scheme/base

(require tests/eli-tester (prefix-in scr: scribble/reader)
         (for-syntax scheme/base))

(provide reader-tests)

(define the-tests #<<END-OF-TESTS

;; format:
;; * a line with only `-'s marks the boundary between tests
;; * -<token>-> marks a <token> kind of reader test
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
@foo@bar  -@-> foo bar
---
@foo@bar.  -@-> foo bar.
---
@foo@bar:  -@-> foo bar:
---
@foo@bar;  -@-> foo bar
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
@foo[bar][baz]  -@->  (foo bar) (baz)
---
@foo[bar]{baz}  -@->  (foo bar "baz")
---
@foo[bar]{baz}[blah]  -@->  (foo bar "baz") (blah)
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
@|{blah}|  -@->  ("blah")
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
;; -------------------- some code test
---
@string-append{1 @(number->string (+ 2 3)) 4}  -@e-> "1 5 4"
---
(let* ([formatter (lambda (fmt)
                    (lambda args (format fmt (apply string-append args))))]
       [bf (formatter "*~a*")]
       [it (formatter "/~a/")]
       [ul (formatter "_~a_")]
       [text string-append])
  @text{@it{Note}: @bf{This is @ul{not} a pipe}.})
-@e->
"/Note/: *This is _not_ a pipe*."
---
(let ([nl (car @'{
                 })]
      [o (open-output-string)])
  (for-each (lambda (x) (display (if (eq? x nl) "\n... " x) o))
            @`{foo
               @,@(list "bar" "\n" "baz")
               blah})
  (newline o)
  (get-output-string o))
-@e->
"foo\n... bar\nbaz\n... blah\n"
---
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
-@e->
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
-@e->
"foo\n      bar"
---

END-OF-TESTS
)

(define-namespace-anchor anchor)
(define ns (namespace-anchor->namespace anchor))
(define (string->tester name) (eval (string->symbol name) ns))

(define (read-all str reader)
  (define i (open-input-string str))
  (port-count-lines! i)
  (let loop ()
    (let ([x (reader i)])
      (if (eof-object? x) '() (cons x (loop))))))

(define (x . -@e-> . y)
  (define r (void))
  (for ([x (read-all x (lambda (i) (scr:read-syntax 'test i)))])
    (set! r (call-with-values (lambda () (eval x ns)) list)))
  (values r (read-all y read)))

(define (x . -@-> . y)
  (values (read-all x scr:read) (read-all y read)))

(define (reader-tests)
  (test do
    (let* ([ts the-tests]
           [ts (regexp-replace* #px"(?m:^;.*\r?\n)" ts "")]
           [ts (regexp-replace #px"^\\s+" ts "")]
           [ts (regexp-replace #px"\\s+$" ts "")]
           [ts (regexp-split #px"\\s*(?:^|\r?\n)-+(?:$|\r?\n)\\s*" ts)])
      (for ([t ts] #:when (not (equal? "" t)))
        (let ([m (regexp-match #px"^(.*\\S)\\s+(-\\S+->)\\s+(\\S.*)$" t)])
          (if (not (and m (= 4 (length m))))
            (error 'bad-test "~a" t)
            (let-values ([(x y)
                          ((string->tester (caddr m)) (cadr m) (cadddr m))])
              (test #:failure-message
                    (format "bad result in\n    ~a\n  results:\n    ~s != ~s"
                            (regexp-replace* #rx"\n" t "\n    ")
                            x y)
                    (equal? x y)))))))))
