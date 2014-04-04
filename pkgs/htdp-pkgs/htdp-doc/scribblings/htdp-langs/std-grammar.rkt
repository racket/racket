#reader scribble/reader
#lang racket/base
(require "common.rkt"
         scribble/decode)

(provide prim-nonterms
         racketgrammar*+library
         racketgrammar*+qq)

(define ex-str "This is a string with \" inside")

(define-syntax-rule (racketgrammar*+library
                     #:literals lits
                     (check-expect check-random check-within check-member-of check-range check-error require)
                     form ...)
  (racketgrammar*
   #:literals lits
   form ...
   [test-case @#,racket[(check-expect expr expr)]
              @#,racket[(check-random expr expr)]
              @#,racket[(check-within expr expr expr)]
              @#,racket[(check-member-of expr expr (... ...))]
              @#,racket[(check-range expr expr expr)]
              @#,racket[(check-error expr expr)]
              @#,racket[(check-error expr)]]
   (...
    [library-require @#,racket[(require string)]
                     @#,racket[(require (lib string string ...))]
                     @#,racket[(require (planet string package))]])
   (...
    [package @#,racket[(string string number number)]])))

(define-syntax-rule (racketgrammar*+qq 
                     #:literals lits
                     (check-expect check-random check-within check-member-of check-range check-error require)
                     form ...)
  (racketgrammar*+library
   #:literals lits
   (check-expect check-random check-within check-member-of check-range check-error require)
   form ...
   (...
    [quoted name
            number
            string
            character
            @#,racket[(quoted ...)]
            @#,elem{@racketvalfont{'}@racket[quoted]}
            @#,elem{@racketvalfont{`}@racket[quoted]}
            @#,elem{@racketfont{,}@racket[quoted]}
            @#,elem{@racketfont[",@"]@racket[quoted]}])
   (...
    [quasiquoted name
                 number
                 string
                 character
                 @#,racket[(quasiquoted ...)]
                 @#,elem{@racketvalfont{'}@racket[quasiquoted]}
                 @#,elem{@racketvalfont{`}@racket[quasiquoted]}
                 @#,elem{@racketfont{,}@racket[expr]}
                 @#,elem{@racketfont[",@"]@racket[expr]}])))

(define-syntax-rule (prim-nonterms (section-prefix) define define-struct)
  
  (make-splice
   (list

@t{An @racket[_name] or a @racket[_variable] is a sequence of characters
not including a space or one of the following:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`}
@litchar{(} @litchar{)} @litchar{[} @litchar{]}
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}

@t{A @racket[_number] is a number such as @racket[123], @racket[3/2], or
@racket[5.5].}

@t{A @racket[_boolean] is one of: @racket[true], @racket[false],
@racket[#t], @racket[#f], @code{#true}, or @code{#false}.}

@t{A @racket[_symbol] is a quote character followed by a name. A
symbol is a value, just like 0 or empty.}

@t{A @racket[_string] is enclosed by a pair of @litchar{"}. Unlike
symbols, strings may be split into characters and manipulated by a
variety of functions.  For example, @racket["abcdef"],
@racket["This is a string"], and @racket[#,ex-str] are all strings.}

@t{A @racket[_character] begins with @litchar{#\} and has the
name of the character. For example, @racket[#\a], @racket[#\b],
and @racket[#\space] are characters.}

@t{In @seclink[(string-append section-prefix "-syntax")]{function calls}, the function appearing
immediatly after the open parenthesis can be any functions defined
with @racket[define] or @racket[define-struct], or any one of the
@seclink[(string-append section-prefix "-pre-defined")]{pre-defined functions}.}

)))

