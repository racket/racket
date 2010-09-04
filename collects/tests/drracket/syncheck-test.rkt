
#|

tests involving object% are commented out, since they
trigger runtime errors in check syntax.

|#
#lang scheme/base

  (require "drracket-test-util.rkt"
           string-constants/string-constant
           tests/utils/gui
           scheme/path
           scheme/class
           scheme/list
           scheme/file
           mred
           framework
           mrlib/text-string-style-desc)
  
  (provide main)
  
  ;; type str/ann = (list (union symbol string) symbol)
  ;; type test = (make-test string
  ;;                        (listof str/ann)
  ;;                        (listof (cons (list number number) (listof (list number number)))))
  (define-struct test (input expected arrows))
  (define-struct (dir-test test) ())
  
  (define build-test
    (λ (input expected [arrow-table '()])
      (make-test input expected arrow-table)))
  
  ;; tests : (listof test)
  (define tests
    (list 
     
     (build-test "12345"
                '(("12345" constant)))
     (build-test "'abcdef"
                '(("'" imported-syntax)
                  ("abcdef" constant)))
     (build-test "(define f 1)"
                '(("("      default-color)
                  ("define" imported-syntax)
                  (" "      default-color)
                  ("f"      lexically-bound-variable)
                  (" "      default-color)
                  ("1"      constant)
                  (")"      default-color)))
     (build-test "(lambda (x) x)"
                '(("("      default-color)
                  ("lambda" imported-syntax)
                  (" ("     default-color)
                  ("x"      lexically-bound-variable)
                  (") "     default-color)
                  ("x"      lexically-bound-variable)
                  (")"      default-color))
                (list '((9 10) (12 13))))
     (build-test "(lambda x x)"
                '(("("      default-color)
                  ("lambda" imported-syntax)
                  (" "      default-color)
                  ("x"      lexically-bound-variable)
                  (" "      default-color)
                  ("x"      lexically-bound-variable)
                  (")"      default-color))
                (list '((8 9) (10 11))))
     (build-test "(lambda (x . y) x y)"
                '(("("      default-color)
                  ("lambda" imported-syntax)
                  (" ("     default-color)
                  ("x"      lexically-bound-variable)
                  (" . "    default-color)
                  ("y"      lexically-bound-variable)
                  (") "     default-color)
                  ("x"      lexically-bound-variable)
                  (" "      default-color)
                  ("y"      lexically-bound-variable)
                  (")"      default-color))
                (list '((9 10) (16 17))
                      '((13 14) (18 19))))
     
     (build-test "(case-lambda [(x) x])"
                 '(("("           default-color)
                   ("case-lambda" imported-syntax)
                   (" [("         default-color)
                   ("x"           lexically-bound-variable)
                   (") "          default-color)
                   ("x"           lexically-bound-variable)
                   ("])"          default-color))
                 (list '((15 16) (18 19))))
     
     (build-test "(if 1 2 3)"
                '(("("  default-color)
                  ("if" imported-syntax)
                  (" "  default-color)
                  ("1"  constant)
                  (" "  default-color)
                  ("2"  constant)
                  (" "  default-color)
                  ("3"  constant)
                  (")"  default-color)))
     (build-test "(if 1 2)"
                '(("("  default-color)
                  ("if" imported-syntax)
                  (" "  default-color)
                  ("1"  constant)
                  (" "  default-color)
                  ("2"  constant)
                  (")"  default-color)))
     
     (build-test "(begin 1 2)"
                '(("("     default-color)
                  ("begin" imported-syntax)
                  (" "     default-color)
                  ("1"     constant)
                  (" "     default-color)
                  ("2"     constant)
                  (")"     default-color)))
     (build-test "(begin0 1 2)"
                '(("("      default-color)
                  ("begin0" imported-syntax)
                  (" "      default-color)
                  ("1"      constant)
                  (" "      default-color)
                  ("2"      constant)
                  (")"      default-color)))
     (build-test "(let ([x x]) x)"
                '(("("   default-color)
                  ("let" imported-syntax)
                  (" ([" default-color)
                  ("x"   lexically-bound-variable)
                  (" "   default-color)
                  ("x"   error)
                  ("]) " default-color)
                  ("x"   lexically-bound-variable)
                  (")"   default-color))
                (list '((7 8) (13 14))))
     (build-test "(letrec ([x x]) x)"
                '(("("      default-color)
                  ("letrec" imported-syntax)
                  (" (["    default-color)
                  ("x"      lexically-bound-variable)
                  (" "      default-color)
                  ("x"      lexically-bound-variable)
                  ("]) "    default-color)
                  ("x"      lexically-bound-variable)
                  (")"      default-color))
                (list '((10 11) (12 13) (16 17))))
     (build-test "(#%top . x)"
                 '(("("     default-color) 
                   ("#%top" imported-syntax)
                   (" . "   default-color)
                   ("x"     error)
                   (")"    default-color)))
     (build-test "(set! x 1)"
                '(("("    default-color)
                  ("set!" imported-syntax)
                  (" "    default-color)
                  ("x"    error)
                  (" "    default-color)
                  ("1"    constant)
                  (")"    default-color)))
     (build-test "(set! x 1) (define x 2)"
                 '(("("      default-color)
                   ("set!"   imported-syntax)
                   (" "      default-color)
                   ("x"      lexically-bound)
                   (" "      default-color)
                   ("1"      constant)
                   (") ("    default-color)
                   ("define" imported-syntax)
                   (" "      default-color)
                   ("x"      set!d)   ;; top-level doesn't help here ....
                   (" 2)"    default-color))
                 (list '((19 20) (6 7))))
     (build-test "(let ([x 1]) (set! x 2))"
                '(("("    default-color)
                  ("let"   imported-syntax)
                  (" (["   default-color)
                  ("x"     set!d)
                  (" "     default-color)
                  ("1"     constant)
                  ("]) ("  default-color)
                  ("set!"  imported-syntax)
                  (" "     default-color)
                  ("x"     set!d)
                  (" "     default-color)
                  ("2"     constant)
                  ("))"    default-color))
                (list '((7 8) (19 20))))
     
     (build-test "object%"
                '(("object%" imported-syntax))) ; used to be lexically-bound-variable
     (build-test "unbound-id"
                '(("unbound-id" error)))
     (build-test "(define bd 1) bd"
                '(("("       default-color)
                  ("define"  imported-syntax)
                  (" "       default-color)
                  ("bd"      lexically-bound-variable)
                  (" "       default-color)
                  ("1"       constant)
                  (") "      default-color)
                  ("bd"      lexically-bound-variable))
                (list '((8 10) (14 16))))
     (build-test "#'abc"
                '(("#'"  imported-syntax)
                  ("abc" constant)))
     (build-test "(with-continuation-mark 1 2 3)"
                '(("("                      default-color)
                  ("with-continuation-mark" imported-syntax)
                  (" "                      default-color)
                  ("1"                      constant)
                  (" "                      default-color)
                  ("2"                      constant)
                  (" "                      default-color)
                  ("3"                      constant)
                  (")"                      default-color)))
     (build-test "(f x)"
                '(("(" default-color)
                  ("f" error)
                  (" " default-color)
                  ("x" error)
                  (")" default-color)))
     (build-test "(define-syntax (f stx) (syntax 1))"
                '(("("             default-color)
                  ("define-syntax" imported-syntax)
                  (" ("            default-color)
                  ("f"             lexically-bound-syntax)
                  (" "             default-color)
                  ("stx"           lexically-bound-variable)
                  (") ("           default-color)
                  ("syntax"        imported-syntax)
                  (" "             default-color)
                  ("1"             constant)
                  ("))"            default-color)))
     
     (build-test "(define-for-syntax (f x) x)"
                '(("("                 default-color)
                  ("define-for-syntax" imported-syntax)
                  (" ("                default-color)
                  ("f"                 lexically-bound-syntax)
                  (" "                 default-color)
                  ("x"                 lexically-bound-variable)
                  (") "                default-color)
                  ("x"                 lexically-bound-variable)
                  (")"                 default-color))
                (list '((22 23) (25 26))))

     (build-test "(module m mzscheme)"
                '(("("        default-color)
                  ("module"   imported-syntax)
                  (" m "      default-color)
                  ("mzscheme" error)
                  (")"        default-color)))
     (build-test "(require-for-syntax mzscheme)"
                '(("("                  default-color)
                  ("require-for-syntax" imported-syntax)
                  (" "          default-color)
                  ("mzscheme"   error)
                  (")"          default-color)))
     (build-test "(require mzlib/list)"
                '(("("                   default-color)
                  ("require"             imported-syntax)
                  (" "                   default-color)
                  ("mzlib/list"          error)
                  (")"                   default-color)))
     (build-test "(module m mzscheme (provide x) (define x 1))"
                '(("("             default-color)
                  ("module"        imported-syntax)
                  (" m mzscheme (" default-color)
                  ("provide"       imported-syntax)
                  (" "             default-color)
                  ("x"             lexically-bound-variable)
                  (") ("           default-color)
                  ("define"        imported-syntax)
                  (" "             default-color)
                  ("x"             lexically-bound-variable)
                  (" "             default-color)
                  ("1"             constant)
                  ("))"            default-color))
                (list '((10 18) (20 27) (32 38))
                      '((39 40) (28 29))))
     
     (build-test "(module m mzscheme (+ 1 2))"
                '(("("             default-color)
                  ("module"        imported-syntax)
                  (" m mzscheme (" default-color)
                  ("+"             imported-variable)
		  (" "             default-color)
		  ("1"             constant)
		  (" "             default-color)
		  ("2"             constant)
                  ("))"            default-color))
                (list '((10 18) (20 21))))
     
     (build-test "(module m mzscheme (require mzlib/list))"
                '(("("                 default-color)
                  ("module"            imported-syntax)
                  (" m mzscheme ("     default-color)
                  ("require"           imported-syntax)
                  (" "                 default-color)
                  ("mzlib/list"        error)
                  ("))"                default-color))
                (list '((10 18) (20 27))))
     
     (build-test "(module m mzscheme (require-for-syntax mzlib/list) (define-syntax s foldl))"
                '(("("                     default-color)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         default-color)
                  ("require-for-syntax"    imported-syntax)
                  (" mzlib/list) ("        default-color)
                  ("define-syntax"         imported-syntax)
                  (" "                     default-color)
                  ("s"                     lexically-bound-syntax)
                  (" "                     default-color)
                  ("foldl"                 imported-variable)
                  ("))"                    default-color))
                (list '((10 18) (20 38) (52 65))
                      '((39 49) (68 73))))
     
     (build-test "(module m mzscheme (require-for-syntax mzlib/etc) (define-syntax s (rec f 1)))"
                '(("("                     default-color)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         default-color)
                  ("require-for-syntax"    imported-syntax)
                  (" mzlib/etc) ("         default-color)
                  ("define-syntax"         imported-syntax)
                  (" "                     default-color)
                  ("s"                     lexically-bound-syntax)
                  (" ("                    default-color)
                  ("rec"                   imported-syntax)
                  (" "                     default-color)
                  ("f"                     lexically-bound-variable)
                  (" "                     default-color)
                  ("1"                     constant)
                  (")))"                   default-color))
                (list '((10 18) (20 38) (51 64))
                      '((39 48) (68 71))))


     (build-test "(define-for-syntax (f x) x) (define (f x) x) f (define-syntax (m x) (f x))"
                 '(("(" default-color)
                   ("define-for-syntax" imported)
                   (" (" default-color)
                   ("f" lexically-bound)
                   (" " default-color)
                   ("x" lexically-bound)
                   (") " default-color)
                   ("x" lexically-bound)
                   (") (" default-color)
                   ("define" imported)
                   (" (" default-color)
                   ("f" lexically-bound)
                   (" " default-color)
                   ("x" lexically-bound)
                   (") " default-color)
                   ("x" lexically-bound)
                   (") " default-color)
                   ("f" lexically-bound)
                   (" (" default-color)
                   ("define-syntax" imported)
                   (" (" default-color)
                   ("m" lexically-bound)
                   (" " default-color)
                   ("x" lexically-bound)
                   (") (" default-color)
                   ("f" lexically-bound)
                   (" " default-color)
                   ("x" lexically-bound)
                   ("))" default-color))
                 '(((20 21) (69 70))
                   ((22 23) (25 26))
                   ((37 38) (45 46))
                   ((39 40) (42 43))
                   ((65 66) (71 72))))
     
     (build-test "(module m mzscheme (define-for-syntax (f x) x) (define (f x) x) f (define-syntax (m stx) (f stx)))"
                 '(("(" default-color)
                   ("module" imported)
                   (" m mzscheme (" default-color)
                   ("define-for-syntax" imported)
                   (" (" default-color)
                   ("f" lexically-bound)
                   (" " default-color)
                   ("x" lexically-bound)
                   (") " default-color)
                   ("x" lexically-bound)
                   (") (" default-color)
                   ("define" imported)
                   (" (" default-color)
                   ("f" lexically-bound)
                   (" " default-color)
                   ("x" lexically-bound)
                   (") " default-color)
                   ("x" lexically-bound)
                   (") " default-color)
                   ("f" lexically-bound)
                   (" (" default-color)
                   ("define-syntax" imported)
                   (" (" default-color)
                   ("m" lexically-bound)
                   (" " default-color)
                   ("stx" lexically-bound)
                   (") (" default-color)
                   ("f" lexically-bound)
                   (" " default-color)
                   ("stx" lexically-bound)
                   (")))" default-color))
                 '(((10 18) (20 37) (48 54) (67 80))
                   ((39 40) (90 91))
                   ((41 42) (44 45))
                   ((56 57) (64 65))
                   ((58 59) (61 62))
                   ((84 87) (92 95))))
     
     (build-test "(define-syntax s (lambda (stx) (syntax-case stx () (_ 123))))"
                '(("("             default-color)
                  ("define-syntax" imported-syntax)
                  (" "             default-color)
                  ("s"             lexically-bound-syntax)
                  (" ("            default-color)
                  ("lambda"        imported-syntax)
                  (" ("            default-color)
                  ("stx"           lexically-bound-variable)
                  (") ("           default-color)
                  ("syntax-case"   imported-syntax)
                  (" "             default-color)
                  ("stx"           lexically-bound-variable)
                  (" () ("         default-color)
                  ("_"             lexically-bound-syntax)
                  (" "             default-color)
                  ("123"           constant)
                  ("))))"          default-color))
                (list '((26 29) (44 47))))

     (build-test "(require mzlib/list) first"
                '(("("                    default-color)
                  ("require"              imported-syntax)
                  (" mzlib/list) "        default-color)
                  ("first"                imported-variable))
                (list '((9 19) (21 26))))

     (build-test "(require mzlib/etc) (rec f 1)"
                '(("("                    default-color)
                  ("require"              imported-syntax)
                  (" mzlib/etc) ("        default-color)
                  ("rec"                  imported-syntax)
                  (" "                    default-color)
                  ("f"                    lexically-bound-variable)
                  (" "                    default-color)
                  ("1"                    constant)
                  (")"                    default-color))
                (list '((9 18) (21 24))))

     (build-test "(define-struct s ())"
                '(("("             default-color)
                  ("define-struct" imported-syntax)
                  (" "             default-color)
                  ("s"             lexically-bound-syntax)
                  (" ())"          default-color)))
     
     (build-test "(define-struct s ()) (define-struct (t s) ())"
                '(("("             default-color)
                  ("define-struct" imported-syntax)
                  (" "             default-color)
                  ("s"             lexically-bound-syntax)
                  (" ()) ("        default-color)
                  ("define-struct" imported-syntax)
                  (" ("            default-color)
                  ("t"             lexically-bound-syntax)
                  (" "             default-color)
                  ("s"             lexically-bound-syntax)
                  (") ())"         default-color))
                (list '((15 16) (39 40))))
     
     (build-test "(let () (define-struct s (x)) 1)"
                 '(("("             default-color)
                   ("let"           imported-syntax)
                   (" () ("         default-color)
                   ("define-struct" imported-syntax)
                   (" "             default-color)
                   ("s"             lexically-bound-syntax)
                   (" (x)) "        default-color)
                   ("1"             constant)
                   (")"             default-color)))
     
     (build-test "(let ([x 12]) (define-struct s (x)) x)"
                 '(("("             default-color)
                   ("let"           imported-syntax)
                   (" (["           default-color)
                   ("x"             lexically-bound-variable)
                   (" "             default-color)
                   ("12"            constant)
                   ("]) ("          default-color)
                   ("define-struct" imported-syntax)
                   (" "             default-color)
                   ("s"             lexically-bound-syntax)
                   (" (x)) "        default-color)
                   ("x"             lexically-bound-variable)
                   (")"             default-color))
                 (list '((7 8) (36 37))))
     
     (build-test "`(1 ,x 2)"
                '(("`"        imported-syntax)
                  ("("        default-color)
                  ("1"        constant)
                  (" ,"       default-color)
                  ("x"        error)
                  (" "        default-color)
                  ("2"        constant)
                  (")"        default-color)))

     (build-test "`(a ,2 b c d)"
                `(("`"  imported-syntax)
                  ("("  default-color)
                  ("a"  constant)
                  (" ," default-color)
                  ("2"  constant)
                  (" "  default-color)
                  ("b"  constant)
                  (" "  default-color)
                  ("c"  constant)
                  (" "  default-color)
                  ("d"  constant)
                  (")"  default-color)))
     
     (build-test "#! /usr/bin/env"
                '(("#! /usr/bin/env" default-color)))
     
     (build-test "#! /usr/bin/env\n"
                '(("#! /usr/bin/env\n" default-color)))
     
     (build-test "#! /usr/bin/env\n1"
                '(("#! /usr/bin/env\n" default-color)
                  ("1"    constant)))
     
     (build-test "#! /usr/bin/env\n1\n1"
                '(("#! /usr/bin/env\n" default-color)
                  ("1"    constant)
                  ("\n"   default-color)
                  ("1"    constant)))
     
     (build-test "#! /usr/bin/env\n(lambda (x) x)"
                 '(("#! /usr/bin/env\n("    default-color)
                   ("lambda"  imported-syntax)
                   (" ("      default-color)
                   ("x"       lexically-bound-variable)
                   (") "      default-color)
                   ("x"       lexically-bound-variable)
                   (")"       default-color))
                 (list '((25 26) (28 29))))
     
     (build-test "(module m mzscheme (lambda (x) x) (provide))"
                '(("("             default-color)
                  ("module"        imported-syntax)
                  (" m mzscheme (" default-color)
                  ("lambda"        imported-syntax)
                  (" ("            default-color)
                  ("x"             lexically-bound-variable)
                  (") "            default-color)
                  ("x"             lexically-bound-variable)
                  (") ("           default-color)
                  ("provide"       imported-syntax)
                  ("))"            default-color))
                (list '((10 18) (20 26) (35 42))
                      '((28 29) (31 32))))
     
     (build-test "(module m mzscheme (define-struct s (a)) s-a make-s s? set-s-a!)"
                '(("("             default-color)
                  ("module"        imported-syntax)
                  (" m mzscheme (" default-color)
                  ("define-struct" imported-syntax)
                  (" "             default-color)
                  ("s"             lexically-bound-syntax)
                  (" (a)) "        default-color)
                  ("s-a"           lexically-bound-variable)
                  (" "             default-color)
                  ("make-s"        lexically-bound-variable)
                  (" "             default-color)
                  ("s?"            lexically-bound-variable)
                  (" "             default-color)
                  ("set-s-a!"      lexically-bound-variable)
                  (")"             default-color))
                (list '((10 18) (20 33))))

     (build-test "(let l () l l)"
                '(("("    default-color)
                  ("let"  imported-syntax)
                  (" "    default-color)
                  ("l"    lexically-bound-variable)
                  (" () " default-color)
                  ("l"    lexically-bound-variable)
                  (" "    default-color)
                  ("l"    lexically-bound-variable)
                  (")"    default-color))
                (list '((5 6) (10 11) (12 13))))
     
     (build-test "(class object% this)"
                '(("("       default-color)
                  ("class"   imported-syntax)
                  (" "       default-color)
                  ("object%" imported-syntax) ; was lexically-bound-variable
                  (" "       default-color)
                  ("this"    imported)
                  (")"       default-color)))
     
     (build-test "(module m mzscheme (require mzlib/list) foldl)"
                '(("("                    default-color)
                  ("module"               imported-syntax)
                  (" m mzscheme ("        default-color)
                  ("require"              imported-syntax)
                  (" mzlib/list) "        default-color)
                  ("foldl"                imported-variable)
                  (")"                    default-color))
                (list '((10 18) (20 27))
                      '((28 38) (40 45))))
     (build-test "(module m lang/htdp-beginner empty)"
                '(("("                        default-color)
                  ("module"                   imported-syntax)
                  (" m lang/htdp-beginner "   default-color)
                  ("empty"                    imported-variable)
                  (")"                        default-color))
                (list '((10 28) (29 34))))
     (build-test "(module m mzscheme (require (prefix x: mzlib/list)) x:foldl)"
                '(("("                                default-color)
                  ("module"                           imported-syntax)
                  (" m mzscheme ("                    default-color)
                  ("require"                          imported-syntax)
                  (" (prefix x: mzlib/list)) "        default-color)
                  ("x:foldl"                          imported-variable)
                  (")"                                default-color))
                (list '((10 18) (20 27))
                      '((28 50) (52 59))))

     (build-test "(module m mzscheme (require (prefix x: mzlib/list) mzlib/list) x:foldl foldl)"
                '(("("                                    default-color)
                  ("module"                               imported-syntax)
                  (" m mzscheme ("                        default-color)
                  ("require"                              imported-syntax)
                  (" (prefix x: mzlib/list) mzlib/list) " default-color)
                  ("x:foldl"                              imported-variable)
                  (" "                                    default-color)
                  ("foldl"                                imported-variable)
                  (")"                                    default-color))
                (list '((10 18) (20 27))
                      '((28 50) (63 70))
                      '((51 61) (71 76))))

     (build-test "(module m mzscheme (require (only mzlib/list foldr) (only mzlib/list foldl)) foldl foldr)"
                 '(("("                                                  default-color)
                   ("module"                                             imported-syntax)
                   (" m mzscheme ("                                      default-color)
                   ("require"                                            imported-syntax)
                   (" (only mzlib/list foldr) (only mzlib/list foldl)) " default-color)
                   ("foldl"                                              imported-variable)
                   (" "                                                  default-color)
                   ("foldr"                                              imported-variable)
                   (")"                                                  default-color))
                 (list '((10 18) (20 27))
                       '((28 51) (77 82) (83 88))
                       '((52 75) (77 82) (83 88))))

     (build-test "(module m mzscheme (require (prefix x: mzscheme)) x:+ +)"
                 '(("("                                                  default-color)
                   ("module"                                             imported-syntax)
                   (" m mzscheme ("                                      default-color)
                   ("require"                                            imported-syntax)
                   (" (prefix x: mzscheme)) "                            default-color)
                   ("x:+"                                                imported-variable)
                   (" "                                                  default-color)
                   ("+"                                                  imported-variable)
                   (")"                                                  default-color))
                 (list '((10 18) (20 27) (54 55))
                       '((28 48) (50 53))))

     (build-test "(module m mzscheme (require mzlib/etc) (rec f 1))"
                '(("("                     default-color)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         default-color)
                  ("require"               imported-syntax)
                  (" mzlib/etc) ("         default-color)
                  ("rec"                   imported-syntax)
                  (" "                     default-color)
                  ("f"                     lexically-bound-variable)
                  (" "                     default-color)
                  ("1"                     constant)
                  ("))"                    default-color))
                (list '((10 18) (20 27))
                      '((28 37) (40 43))))

     (build-test "(module m lang/htdp-intermediate (local ((define x x)) x))"
                '(("("                           default-color)
                  ("module"                      imported-syntax)
                  (" m lang/htdp-intermediate (" default-color)
                  ("local"                       imported-syntax)
                  (" ((define "                  default-color)
                  ("x"                           lexically-bound-variable)
                  (" "                           default-color)
                  ("x"                           lexically-bound-variable)
                  (")) "                         default-color)
                  ("x"                           lexically-bound-variable)
                  ("))"                          default-color))
                (list '((10 32) (34 39))
                      '((49 50) (51 52) (55 56))))

     (build-test "(module m mzscheme (define-syntax rename #f) (require (rename mzscheme ++ +)))"
                 '(("("                         default-color)
                   ("module"                    imported)
                   (" m mzscheme ("             default-color)
                   ("define-syntax"             imported)
                   (" "                         default-color)
                   ("rename"                    lexically-bound)
                   (" #f) ("                    default-color)
                   ("require"                   imported)
                   (" (rename mzscheme ++ +)))" default-color))
                 
                 (list '((10 18) (20 33) (46 53))))
     
     (build-test "(module m mzscheme (define-syntax rename #f) (define f 1) (provide (rename f g)))"
                 '(("("               default-color)
                   ("module"          imported)
                   (" m mzscheme ("   default-color)
                   ("define-syntax"   imported)
                   (" "               default-color)
                   ("rename"          lexically-bound)
                   (" #f) ("          default-color)
                   ("define"          imported)
                   (" "               default-color)
                   ("f"               lexically-bound)
                   (" 1) ("           default-color)
                   ("provide"         imported)
                   (" (rename "       default-color)
                   ("f"               lexically-bound)
                   (" g)))"           default-color))
                 (list '((10 18) (20 33) (46 52) (59 66))
                       '((53 54) (75 76))))
     
     (build-test "(module m mzscheme (define X 1) (provide (all-defined-except X)))"
                 '(("("                     default-color)
                   ("module"                imported)
                   (" m mzscheme ("         default-color)
                   ("define"                imported)
                   (" "                     default-color)
                   ("X"                     lexically-bound)
                   (" 1) ("                 default-color)
                   ("provide"               imported)
                   (" (all-defined-except " default-color)
                   ("X"                     lexically-bound)
                   (")))"                   default-color))
                 
                 (list '((10 18) (20 26) (33 40))
                       '((27 28) (61 62))))     
     
     (build-test "(module m mzscheme (require-for-syntax mzscheme) (require-for-template mzscheme) (quote-syntax +))"
                 '(("("                    default-color)
                   ("module"               imported)
                   (" m mzscheme ("        default-color)
                   ("require-for-syntax"   imported)
                   (" mzscheme) ("         default-color)
                   ("require-for-template" imported)
                   (" mzscheme) ("         default-color)
                   ("quote-syntax"         imported)
                   (" +))"                 default-color))
                 (list
                  '((71 79) (95 96))
                  '((10 18) (20 38) (50 70) (82 94) (95 96))
                  '((39 47) (95 96))))
     
     ;; test case from Chongkai
     (build-test (format "~s\n\n#reader'reader\n1\n"
                         '(module reader mzscheme
                            (provide (rename mrs read-syntax) read)
                            (define (mrs sv p)
                              (datum->syntax-object
                               (read-syntax #f (open-input-string "a"))
                               `(module f mzscheme
                                  (provide x)
                                  (define x 1))
                               (list sv #f #f #f #f)))))
                 '(("(" default-color)
                   ("module" imported)
                   (" reader mzscheme (" default-color)
                   ("provide" imported)
                   (" (rename " default-color)
                   ("mrs" lexically-bound)
                   (" read-syntax) " default-color)
                   ("read" imported)
                   (") (" default-color)
                   ("define" imported)
                   (" (" default-color)
                   ("mrs" lexically-bound)
                   (" " default-color)
                   ("sv" lexically-bound)
                   (" " default-color)
                   ("p" lexically-bound)
                   (") (" default-color)
                   ("datum->syntax-object" imported)
                   (" (" default-color)
                   ("read-syntax" imported)
                   (" #f (" default-color)
                   ("open-input-string" imported)
                   (" \"a\")) (" default-color)
                   ("quasiquote" imported)
                   (" (module f mzscheme (provide x) (define x 1))) (" default-color)
                   ("list" imported)
                   (" " default-color)
                   ("sv" lexically-bound)
                   (" #f #f #f #f))))\n\n#reader'reader\n1\n" default-color))
                 
                 (list '((15 23) (25 32) (58 62) (65 71) (84 104) (106 117) (122 139) (147 157) (205 209))
                       '((77 79) (210 212))
                       '((73 76) (41 44))))
     
     (make-dir-test "(module m mzscheme (require \"~a/list.rkt\") foldl foldl)"
                    '(("("             default-color)
                      ("module"        imported-syntax)
                      (" m mzscheme (" default-color)
                      ("require"       imported-syntax)
                      (" \""           default-color)
                      (relative-path   default-color)
                      ("/list.rkt\") " default-color)
                      ("foldl"         imported-variable)
                      (" "             default-color)
                      ("foldl"         imported-variable)
                      (")"             default-color))
                    #f)
     
     (build-test "#lang scheme/base\n(require scheme)\n(define-syntax m (lambda (x) #'1))"
                 '(("#lang "        default-color)
                   ("scheme/base"   error)
                   ("\n("           default-color)
                   ("require"       imported)
                   (" scheme)\n("   default-color)
                   ("define-syntax" imported)
                   (" "             default-color)
                   ("m"             lexically-bound)
                   (" ("            default-color)
                   ("lambda"        imported)
                   (" ("            default-color)
                   ("x"             lexically-bound)
                   (") "            default-color)
                   ("#'"            imported)
                   ("1))"           default-color))
                 (list '((27 33) (19 26) (36 49) (53 59) (64 66))))))
  
  (define (main)
    (fire-up-drscheme-and-run-tests
     (λ ()
	(let ([drs (wait-for-drscheme-frame)])
	  (set-language-level! (list "Pretty Big"))
	  (do-execute drs)
          (let* ([defs (send drs get-definitions-text)]
		 [filename (make-temporary-file "syncheck-test~a")])
	    (let-values ([(dir _1 _2) (split-path filename)])
              (send defs save-file filename)
	      (preferences:set 'framework:coloring-active #f)
              (close-the-error-window-test drs)
              (for-each (run-one-test (normalize-path dir)) tests)
	      (preferences:set 'framework:coloring-active #t)
	      (send defs save-file) ;; clear out autosave
	      (send defs set-filename #f)
              (delete-file filename)
              
              (printf "Ran ~a tests.\n" total-tests-run)))))))
  
  (define (close-the-error-window-test drs)
    (clear-definitions drs)
    (insert-in-definitions drs "(")
    (click-check-syntax-button drs)
    (wait-for-computation drs)
    (unless (send drs syncheck:error-report-visible?)
      (error 'close-the-error-window-test "error report window never appeared"))
    (do-execute drs)
    (when (send drs syncheck:error-report-visible?)
      (error 'close-the-error-window-test "error report window did not go away after clicking Run")))
    
  (define total-tests-run 0)
  
  (define ((run-one-test save-dir) test)
    (set! total-tests-run (+ total-tests-run 1))
    (let* ([drs (wait-for-drscheme-frame)]
           [defs (send drs get-definitions-text)]
           [input (test-input test)]
           [expected (test-expected test)]
           [arrows (test-arrows test)]
           [relative (find-relative-path save-dir (collection-path "mzlib"))])
      (clear-definitions drs)
      (cond
        [(dir-test? test)
         (insert-in-definitions drs (format input (path->string relative)))]
        [else (insert-in-definitions drs input)])
      (click-check-syntax-button drs)
      (wait-for-computation drs)
      
      (when (send defs in-edit-sequence?)
        (error 'syncheck-test.rkt "still in edit sequence for ~s" input))
      
      (when (send drs syncheck:error-report-visible?)
        (fprintf (current-error-port)
                 "FAILED ~s\n   error report window is visible\n"
                 input))
      
      ;; need to check for syntax error here
      (let ([got (get-annotated-output drs)])
        (compare-output (cond
                          [(dir-test? test)
                           (map (lambda (x)
                                  (list (if (eq? (car x) 'relative-path)
                                            (path->string relative)
                                            (car x))
                                        (cadr x)))
                                expected)]
                          [else
                           expected])
                        got
                        arrows 
                        (send defs syncheck:get-bindings-table)
                        input))))
  
  
  (define remappings
    '((constant default-color)
      (imported-syntax imported)
      (imported-variable imported)
      (lexically-bound-syntax lexically-bound)
      (lexically-bound-variable lexically-bound)))
  
  (define (collapse-and-rename expected)
    (let ([renamed
           (map (lambda (ent)
                  (let* ([str (car ent)]
                         [id (cadr ent)]
                         [matches (assoc id remappings)])
                    (if matches
                        (list str (cadr matches))
                        ent)))
                expected)])
      (let loop ([ids renamed])
        (cond
          [(null? ids) null]
          [(null? (cdr ids)) ids]
          [else (let ([fst (car ids)]
                      [snd (cadr ids)])
                  (if (eq? (cadr fst) (cadr snd))
                      (loop (cons (list (string-append (car fst) (car snd)) (cadr fst))
                                  (cddr ids)))
                      (cons fst (loop (cdr ids)))))]))))
    
  ;; compare-arrows : expression
  ;;                  (listof (cons (list number number) (listof (list number number))))
  ;;                  hash-table[(list text number number) -o> (listof (list text number number))]
  ;;               -> void
  (define (compare-arrows test-exp expected raw-actual)
    (when expected
      (let ()
        (define already-checked (make-hash))
        
        (define actual-ht (make-hash))
        (define stupid-internal-define-syntax1
          (hash-for-each raw-actual
            (lambda (k v)
              (hash-set! actual-ht (cdr k)
                         (sort (map cdr v)
                               (lambda (x y) (< (car x) (car y))))))))
        (define expected-ht (make-hash))
        (define stupid-internal-define-syntax2
          (for-each (lambda (binding) (hash-set! expected-ht (car binding) (cdr binding)))
                    expected))
        ;; binding-in-ht? : hash-table (list number number) (listof (list number number)) -> boolean
        (define (test-binding expected? ht)
          (lambda (pr)
            (let ([frm (car pr)]
                  [to (cdr pr)])
              (hash-ref
               already-checked
               frm
               (lambda ()
                 (hash-set! already-checked frm #t)
                 (let ([ht-ent (hash-ref ht frm (lambda () 'nothing-there))])
                   (unless (equal? ht-ent to)
                     (fprintf (current-error-port)
                              (if expected? 
                                  "FAILED arrow test ~s from ~s\n  expected ~s\n    actual ~s\n"
                                  "FAILED arrow test ~s from ~s\n    actual ~s\n  expected ~s\n")
                             test-exp
                             frm
                             ht-ent
                             to))))))))
        
        (for-each (test-binding #t expected-ht) (hash-map actual-ht cons))
        (for-each (test-binding #f actual-ht) (hash-map expected-ht cons)))))
  
  (define (compare-output raw-expected got arrows arrows-got input)
    (let ([expected (collapse-and-rename raw-expected)])
      (cond
        [(equal? got expected)
         (compare-arrows input arrows arrows-got)]
        [else
         (fprintf (current-error-port)
                  "FAILED: ~s\n      expected: ~s\n           got: ~s\n"
                  input expected got)])))
  
  ;; get-annotate-output : drscheme-frame -> (listof str/ann)
  (define (get-annotated-output drs)
    (let ([chan (make-channel)])
      (queue-callback
       (λ ()
         (channel-put chan (get-string/style-desc (send drs get-definitions-text)))))
      (channel-get chan)))
  
  (define (click-check-syntax-button drs)
    (test:run-one (lambda () (send (send drs syncheck:get-button) command))))
    
  (main)
