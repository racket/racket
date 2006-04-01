
(module syncheck-test mzscheme
  
  (require "drscheme-test-util.ss"
           (lib "gui.ss" "tests" "utils")
           (lib "etc.ss")
           (lib "class.ss")
           (lib "list.ss")
           (lib "file.ss")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "text-string-style-desc.ss" "mrlib"))
  
  (provide run-test)
  
  ;; type str/ann = (list (union symbol string) symbol)
  ;; type test = (make-test string
  ;;                        (listof str/ann)
  ;;                        (listof (cons (list number number) (listof (list number number)))))
  (define-struct test (input expected arrows))
  (define-struct (dir-test test) ())
  
  (define build-test
    (opt-lambda (input expected [arrow-table '()])
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
                   ("x"      lexically-bound-variable)
                   (" "      default-color)
                   ("1"      constant)
                   (") ("    default-color)
                   ("define" imported-syntax)
                   (" "      default-color)
                   ("x"      lexically-bound-variable)
                   (" 2)"    default-color))
                 (list '((19 20) (6 7))))
     (build-test "(let ([x 1]) (set! x 2))"
                '(("("    default-color)
                  ("let"   imported-syntax)
                  (" (["   default-color)
                  ("x"     lexically-bound-variable)
                  (" "     default-color)
                  ("1"     constant)
                  ("]) ("  default-color)
                  ("set!"  imported-syntax)
                  (" "     default-color)
                  ("x"     lexically-bound-variable)
                  (" "     default-color)
                  ("2"     constant)
                  ("))"    default-color))
                (list '((7 8) (19 20))))
     (build-test "object%"
                '(("object%" lexically-bound-variable)))
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
     (build-test "(require (lib \"list.ss\"))"
                '(("("                   default-color)
                  ("require"             imported-syntax)
                  (" "                   default-color)
                  ("(lib \"list.ss\")"   error)
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
     
     (build-test "(module m mzscheme (require (lib \"list.ss\")))"
                '(("("                 default-color)
                  ("module"            imported-syntax)
                  (" m mzscheme ("     default-color)
                  ("require"           imported-syntax)
                  (" "                 default-color)
                  ("(lib \"list.ss\")" error)
                  ("))"                default-color))
                (list '((10 18) (20 27))))
     
     (build-test "(module m mzscheme (require-for-syntax (lib \"list.ss\")) (define-syntax s foldl))"
                '(("("                     default-color)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         default-color)
                  ("require-for-syntax"    imported-syntax)
                  (" (lib \"list.ss\")) (" default-color)
                  ("define-syntax"         imported-syntax)
                  (" "                     default-color)
                  ("s"                     lexically-bound-syntax)
                  (" "                     default-color)
                  ("foldl"                 imported-variable)
                  ("))"                    default-color))
                (list '((10 18) (20 38) (57 70))
                      '((39 54) (73 78))))
     
     (build-test "(module m mzscheme (require-for-syntax (lib \"etc.ss\")) (define-syntax s (rec f 1)))"
                '(("("                     default-color)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         default-color)
                  ("require-for-syntax"    imported-syntax)
                  (" (lib \"etc.ss\")) ("  default-color)
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
                (list '((10 18) (20 38) (56 69))
                      '((39 53) (73 76))))

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

     (build-test "(require (lib \"list.ss\")) first"
                '(("("                    default-color)
                  ("require"              imported-syntax)
                  (" (lib \"list.ss\")) " default-color)
                  ("first"                imported-variable))
                (list '((9 24) (26 31))))
     
     (build-test "(require (lib \"etc.ss\")) (rec f 1)"
                '(("("                    default-color)
                  ("require"              imported-syntax)
                  (" (lib \"etc.ss\")) (" default-color)
                  ("rec"                  imported-syntax)
                  (" "                    default-color)
                  ("f"                    lexically-bound-variable)
                  (" "                    default-color)
                  ("1"                    constant)
                  (")"                    default-color))
                (list '((9 23) (26 29))))
     
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
     
     (build-test "#!"
                '(("#!" default-color)))
     
     (build-test "#!\n"
                '(("#!\n" default-color)))
     
     (build-test "#!\n1"
                '(("#!\n" default-color)
                  ("1"    constant)))
     
     (build-test "#!\n1\n1"
                '(("#!\n" default-color)
                  ("1"    constant)
                  ("\n"   default-color)
                  ("1"    constant)))
     
     (build-test "#!\n(lambda (x) x)"
                 '(("#!\n("    default-color)
                   ("lambda"  imported-syntax)
                   (" ("      default-color)
                   ("x"       lexically-bound-variable)
                   (") "      default-color)
                   ("x"       lexically-bound-variable)
                   (")"       default-color))
                 (list '((12 13) (15 16))))
     
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
     
     (build-test "(define tordu3 '(a . #0=(b c d . #0#)))"
                '(("("        default-color)
                  ("define"   imported-syntax)
                  (" "        default-color)
                  ("tordu3"   lexically-bound-variable)
                  (" "        default-color)
                  ("'"        imported-syntax)
                  ("(a . #0=(b c d . #0#))" constant)
                  (")"        default-color)))

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
                  ("object%" lexically-bound-variable)
                  (" "       default-color)
                  ("this"    imported-identifier)
                  (")"       default-color)))
     (build-test "(module m mzscheme (require (lib \"list.ss\")) foldl)"
                '(("("                    default-color)
                  ("module"               imported-syntax)
                  (" m mzscheme ("        default-color)
                  ("require"              imported-syntax)
                  (" (lib \"list.ss\")) " default-color)
                  ("foldl"                imported-variable)
                  (")"                    default-color))
                (list '((10 18) (20 27))
                      '((28 43) (45 50))))
     (build-test "(module m (lib \"htdp-beginner.ss\" \"lang\") empty)"
                '(("("                                         default-color)
                  ("module"                                    imported-syntax)
                  (" m (lib \"htdp-beginner.ss\" \"lang\") "   default-color)
                  ("empty"                                     imported-variable)
                  (")"                                         default-color))
                (list '((10 41) (42 47))))
     (build-test "(module m mzscheme (require (prefix x: (lib \"list.ss\"))) x:foldl)"
                '(("("                                default-color)
                  ("module"                           imported-syntax)
                  (" m mzscheme ("                    default-color)
                  ("require"                          imported-syntax)
                  (" (prefix x: (lib \"list.ss\"))) " default-color)
                  ("x:foldl"                          imported-variable)
                  (")"                                default-color))
                (list '((10 18) (20 27))
                      '((28 55) (57 64))))
     
     (build-test "(module m mzscheme (require (prefix x: (lib \"list.ss\")) (lib \"list.ss\")) x:foldl foldl)"
                '(("("                                                  default-color)
                  ("module"                                             imported-syntax)
                  (" m mzscheme ("                                      default-color)
                  ("require"                                            imported-syntax)
                  (" (prefix x: (lib \"list.ss\")) (lib \"list.ss\")) " default-color)
                  ("x:foldl"                                            imported-variable)
                  (" "                                                  default-color)
                  ("foldl"                                              imported-variable)
                  (")"                                                  default-color))
                (list '((10 18) (20 27))
                      '((28 55) (73 80) (81 86))
                      '((56 71) (73 80) (81 86))))

     (build-test "(module m mzscheme (require (only (lib \"list.ss\") foldr) (only (lib \"list.ss\") foldl)) foldl foldr)"
                 '(("("                                                  default-color)
                   ("module"                                             imported-syntax)
                   (" m mzscheme ("                                      default-color)
                   ("require"                                            imported-syntax)
                   (" (only (lib \"list.ss\") foldr) (only (lib \"list.ss\") foldl)) " default-color)
                   ("foldl"                                              imported-variable)
                   (" "                                                  default-color)
                   ("foldr"                                              imported-variable)
                   (")"                                                  default-color))
                 (list '((10 18) (20 27))
                       '((28 56) (87 92) (93 98))
                       '((57 85) (87 92) (93 98))))
     
     (build-test "(module m mzscheme (require (lib \"etc.ss\")) (rec f 1))"
                '(("("                     default-color)
                  ("module"                imported-syntax)
                  (" m mzscheme ("         default-color)
                  ("require"               imported-syntax)
                  (" (lib \"etc.ss\")) ("  default-color)
                  ("rec"                   imported-syntax)
                  (" "                     default-color)
                  ("f"                     lexically-bound-variable)
                  (" "                     default-color)
                  ("1"                     constant)
                  ("))"                    default-color))
                (list '((10 18) (20 27))
                      '((28 42) (45 48))))
     
     (build-test "(module m (lib \"htdp-intermediate.ss\" \"lang\") (local ((define x x)) x))"
                '(("("                                            default-color)
                  ("module"                                       imported-syntax)
                  (" m (lib \"htdp-intermediate.ss\" \"lang\") (" default-color)
                  ("local"                                        imported-syntax)
                  (" ((define "                                   default-color)
                  ("x"                                            lexically-bound-variable)
                  (" "                                            default-color)
                  ("x"                                            lexically-bound-variable)
                  (")) "                                          default-color)
                  ("x"                                            lexically-bound-variable)
                  ("))"                                           default-color))
                (list '((10 45) (47 52))
                      '((62 63) (64 65) (68 69))))
     
     (make-dir-test "(module m mzscheme (require \"~a/list.ss\") foldl foldl)"
                    '(("("             default-color)
                      ("module"        imported-syntax)
                      (" m mzscheme (" default-color)
                      ("require"       imported-syntax)
                      (" \""           default-color)
                      (relative-path   default-color)
                      ("/list.ss\") "  default-color)
                      ("foldl"         imported-variable)
                      (" "             default-color)
                      ("foldl"         imported-variable)
                      (")"             default-color))
                    #f)))
  
  (define (run-test)
    (check-language-level #rx"Graphical")
    (let* ([drs (wait-for-drscheme-frame)]
           [defs (send drs get-definitions-text)]
           [filename (make-temporary-file "syncheck-test~a")])
      (let-values ([(dir _1 _2) (split-path filename)])
        (send defs save-file filename)
        (preferences:set 'framework:coloring-active #f)
        (for-each (run-one-test (normalize-path dir)) tests)
        (preferences:set 'framework:coloring-active #t)
        (send defs save-file) ;; clear out autosave
        (send defs set-filename #f)
        (delete-file filename))))
  
  (define ((run-one-test save-dir) test)
    (let* ([drs (wait-for-drscheme-frame)]
           [defs (send drs get-definitions-text)]
           [input (test-input test)]
           [expected (test-expected test)]
           [arrows (test-arrows test)]
           [relative (find-relative-path save-dir (collection-path "mzlib"))])
      (clear-definitions drs)
      (cond
        [(dir-test? test)
         (type-in-definitions drs (format input (path->string relative)))]
        [else (type-in-definitions drs input)])
      (test:button-push (send drs syncheck:get-button))
      (wait-for-computation drs)
      
      ;; this isn't right -- seems like there is a race condition because
      ;; wait-for-computation isn't waiting long enough?
      '(when (send defs in-edit-sequence?)
         (error 'syncheck-test.ss "still in edit sequence for ~s" input))
      
      (when (send drs syncheck:error-report-visible?)
        (printf "FAILED ~s\n   error report window is visible\n"
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
      (imported-syntax imported-identifier)
      (imported-variable imported-identifier)
      (lexically-bound-syntax lexically-bound-identifier)
      (lexically-bound-variable lexically-bound-identifier)))
  
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
    
  ;; compare-arrows : (listof (cons (list number number) (listof (list number number))))
  ;;                  hash-table[(list text number number) -o> (listof (list text number number))]
  ;;               -> void
  (define (compare-arrows test-exp expected raw-actual)
    (when expected
      (let ()
        (define already-checked (make-hash-table 'equal))
        
        (define actual-ht (make-hash-table 'equal))
        (define stupid-internal-define-syntax1
          (hash-table-for-each raw-actual
            (lambda (k v)
              (hash-table-put! actual-ht (cdr k)
                               (sort (map cdr v)
                                     (lambda (x y) (< (car x) (car y))))))))
        (define expected-ht (make-hash-table 'equal))
        (define stupid-internal-define-syntax2
          (for-each (lambda (binding) (hash-table-put! expected-ht (car binding) (cdr binding)))
                    expected))
        ;; binding-in-ht? : hash-table (list number number) (listof (list number number)) -> boolean
        (define (test-binding expected? ht)
          (lambda (pr)
            (let ([frm (car pr)]
                  [to (cdr pr)])
              (hash-table-get
               already-checked
               frm
               (lambda ()
                 (hash-table-put! already-checked frm #t)
                 (let ([ht-ent (hash-table-get ht frm (lambda () 'nothing-there))])
                   (unless (equal? ht-ent to)
                     (printf (if expected? 
                                 "FAILED arrow test ~s from ~s\n  expected ~s\n    actual ~s\n"
                                 "FAILED arrow test ~s from ~s\n    actual ~s\n  expected ~s\n")
                             test-exp
                             frm
                             ht-ent
                             to))))))))
        
        (for-each (test-binding #t expected-ht) (hash-table-map actual-ht cons))
        (for-each (test-binding #f actual-ht) (hash-table-map expected-ht cons)))))
  
  (define (compare-output raw-expected got arrows arrows-got input)
    (let ([expected (collapse-and-rename raw-expected)])
      (cond
        [(equal? got expected)
         (compare-arrows input arrows arrows-got)]
        [else
         (printf "FAILED: ~s\n      expected: ~s\n           got: ~s\n"
                 input expected got)])))
  
  ;; get-annotate-output : drscheme-frame -> (listof str/ann)
  (define (get-annotated-output drs)
    (let ([chan (make-channel)])
      (queue-callback
       (λ ()
         (channel-put chan (get-string/style-desc (send drs get-definitions-text)))))
      (channel-get chan))))

