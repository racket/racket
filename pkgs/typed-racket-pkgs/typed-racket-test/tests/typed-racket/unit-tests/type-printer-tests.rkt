#lang racket/base

;; Tests for type, filter, object, etc. printers for Typed Racket

(require "test-utils.rkt"
         rackunit
         typed-racket/standard-inits
         typed-racket/tc-setup
         typed-racket/rep/type-rep
         typed-racket/types/abbrev
         typed-racket/types/numeric-tower
         typed-racket/types/printer
         typed-racket/types/union
         typed-racket/utils/tc-utils
         (submod typed-racket/base-env/base-types initialize))

(provide tests)
(gen-test-main)

(define (prints-as? thing str)
  (string=? (format "~a" thing) str))

(define (pretty-prints-as? thing str)
  (string=? (pretty-format-type thing) str))

(define-binary-check (check-prints-as? prints-as? actual expected))
(define-binary-check (check-pretty-prints-as? pretty-prints-as? actual expected))

;; Using `do-standard-inits` here does not work when the file is invoked
;; individually (rather than through the test harness)
(initialize-type-names)
(current-type-names (init-current-type-names))

(define tests
  (test-suite
   "Printing tests"
   (test-suite
    "Type printing tests"
    (check-prints-as? (-val 3) "3")
    (check-prints-as? (-val 'a) "'a")
    (check-prints-as? (-val #\a) "#\\a")
    (check-prints-as? Univ "Any")
    (check-prints-as? (Un (-val #t) (-val #f)) "Boolean")
    (check-prints-as? (-lst -Nat) "(Listof Nonnegative-Integer)")
    (check-prints-as? (make-App (-poly (a) (-lst a)) (list -Nat) #'foo)
                      "(Listof Nonnegative-Integer)")
    (check-prints-as? (make-Mu 'x (Un (-val '()) (-pair -Nat (make-F 'x))))
                      "(Listof Nonnegative-Integer)")
    (check-prints-as? (-lst* -String -Symbol) "(List String Symbol)")
    (check-prints-as? -Custodian "Custodian")
    (check-prints-as? (make-Opaque #'integer?) "(Opaque integer?)")
    (check-prints-as? (make-Vector -Nat) "(Vectorof Nonnegative-Integer)")
    (check-prints-as? (make-HeterogeneousVector (list -Symbol -String))
                      "(Vector Symbol String)")
    (check-prints-as? (-box (-val 3)) "(Boxof 3)")
    (check-prints-as? (make-Future -Void) "(Futureof Void)")
    (check-prints-as? (-> -String -Void) "(String -> Void)")
    (check-prints-as? (Un -String -Void) "(U String Void)")
    (check-prints-as? (-pair -String -Void) "(Pairof String Void)")
    (check-prints-as? (make-ListDots -Boolean 'x) "(List Boolean ... x)")
    (check-prints-as? (make-F 'X) "X")
    (check-prints-as? (make-Values (list -String -Symbol))
                      "(values String Symbol)")
    (check-prints-as? (make-ValuesDots (list -String -Symbol) (make-F 'x) 'x)
                      "(values String Symbol x ... x)")
    (check-prints-as? (-polydots (a b) (->... (list a) (b b) a))
                      "(All (a b ...) (a b ... b -> a))")
    (check-prints-as? (-mu x (-lst x)) "(Rec x (Listof x))")
    (check-prints-as? (-seq -String -Symbol) "(Sequenceof String Symbol)")
    (check-prints-as? (-poly (a) (-> a -Void)) "(All (a) (a -> Void))")
    (check-prints-as? (-> -Input-Port (make-Values (list (-result -String (-FS -top -bot) -no-obj)
                                                         (-result -String (-FS -top -bot) -no-obj))))
                      "(Input-Port -> (values (String : (Top | Bot)) (String : (Top | Bot))))")
    ;; this case tests that the Number union is printed with its name
    ;; rather than its expansion (a former bug)
    (check-prints-as? (->* '() -Number -Void) "(Number * -> Void)")
    (check-prints-as? (->key Univ -Pathlike
                             #:exists
                             (one-of/c 'error 'append 'update 'replace
                                       'truncate 'truncate/replace)
                             #f
                             #:mode
                             (one-of/c 'binary 'text)
                             #f
                             -Void)
                      (string-append "(Any Path-String [#:exists (U 'error"
                                     " 'append 'update 'replace 'truncate"
                                     " 'truncate/replace)] [#:mode (U"
                                     " 'binary 'text)] -> Void)"))
    (check-prints-as? (->opt Univ [] -Void) "(Any -> Void)")
    (check-prints-as? (->opt [-String] -Void) "(->* () (String) Void)")
    (check-prints-as? (->opt Univ [-String] -Void) "(->* (Any) (String) Void)")
    (check-prints-as? (->opt Univ -Symbol [-String] -Void)
                      "(->* (Any Symbol) (String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:x -String #f -Void)
                      "(->* (Any) (String #:x String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:x -String #t -Void)
                      "(->* (Any #:x String) (String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:x -String #t -Void)
                      "(->* (Any #:x String) (String) Void)")
    (check-prints-as? (->optkey Univ [-String] #:rest -String #:x -String #t -Void)
                      "(->* (Any #:x String) (String) #:rest String Void)")
    (check-prints-as? (cl->* (->opt -Pathlike [-String] -Void)
                             (->optkey Univ [-String] #:rest -String #:x -String #t -Void))
                      (string-append "(case-> (->* (Path-String) (String) Void) "
                                     "(->* (Any #:x String) (String) #:rest String Void))")))
   (test-suite
    "Pretty printing tests"
    (check-pretty-prints-as? (-val 3) "3")
    (check-pretty-prints-as? (-val 'a) "'a")
    (check-pretty-prints-as? (-val #\a) "#\\a")
    (check-pretty-prints-as? Univ "Any")
    (check-pretty-prints-as? (Un (-val #t) (-val #f)) "Boolean")
    (check-pretty-prints-as? (-lst -Nat) "(Listof Nonnegative-Integer)")
    (check-pretty-prints-as?
     (-polydots (c a b)
       (cl->*
        (-> (-> a c) (-pair a (-lst a)) (-pair c (-lst c)))
        ((list
          ((list a) (b b) . ->... . c)
          (-lst a))
         ((-lst b) b) . ->... .(-lst c))))
     (string-append "(All (c a b ...)\n"
                    "  (case->\n"
                    "   ((a -> c) (Pairof a (Listof a)) -> (Pairof c (Listof c)))\n"
                    "   ((a b ... b -> c) (Listof a) (Listof b) ... b -> (Listof c))))"))
    (check-pretty-prints-as?
     (-poly (a) (cl->* (-> (-Syntax a) Univ Univ (-Syntax a))
                       (-> (-Syntax Univ) Univ Univ)))
     (string-append "(All (a)\n"
                    "  (case-> ((Syntaxof a) Any Any -> (Syntaxof a)) ((Syntaxof Any) Any -> Any)))")))))

