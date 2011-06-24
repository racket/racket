#reader scribble/reader
#lang scheme/base
(require "common.ss"
         scribble/decode
         (for-label lang/htdp-beginner-abbr))

(provide prim-nonterms
         racketgrammar*+library
         racketgrammar*+qq)

(define ex-str "This is a string with \" inside")

(define-syntax-rule (racketgrammar*+library
                     #:literals lits
                     (check-expect check-within check-error require)
                     form ...)
  (racketgrammar*
   #:literals lits
   form ...
   [test-case @#,racket[(check-expect expr expr)]
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
                     (check-expect check-within check-error require)
                     form ...)
  (racketgrammar*+library
   #:literals lits
   (check-expect check-within check-error require)
   form ...
   (...
    [quoted id
            number
            string
            character
            @#,racket[(quoted ...)]
            @#,elem{@racketvalfont{'}@racket[quoted]}
            @#,elem{@racketvalfont{`}@racket[quoted]}
            @#,elem{@racketfont{,}@racket[quoted]}
            @#,elem{@racketfont[",@"]@racket[quoted]}])
   (...
    [quasiquoted id
                 number
                 string
                 character
                 @#,racket[(quasiquoted ...)]
                 @#,elem{@racketvalfont{'}@racket[quasiquoted]}
                 @#,elem{@racketvalfont{`}@racket[quasiquoted]}
                 @#,elem{@racketfont{,}@racket[expr]}
                 @#,elem{@racketfont[",@"]@racket[expr]}])))

(define prim-nonterms
  (make-splice
   (list

@t{An @racket[_id] is a sequence of characters not including a
space or one of the following:}

@t{@hspace[2] @litchar{"} @litchar{,} @litchar{'} @litchar{`} 
@litchar{(} @litchar{)} @litchar{[} @litchar{]} 
@litchar["{"] @litchar["}"] @litchar{|} @litchar{;}
@litchar{#}}

@t{A @racket[_number] is a number such as @racket[123], @racket[3/2], or
@racket[5.5].}

@t{A @racket[_string] is enclosed by a pair of @litchar{"}. Unlike
symbols, strings may be split into characters and manipulated by a
variety of primitive functions.  For example, @racket["abcdef"],
@racket["This is a string"], and @racket[#,ex-str] are all strings.}

@t{A @racket[_character] begins with @litchar{#\} and has the
name of the character. For example, @racket[#\a], @racket[#\b],
and @racket[#\space] are characters.}

)))
