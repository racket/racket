#lang at-exp racket/base

;; This files assumes `(eq? 'null (json-null))`.
(module+ test
  (run-tests json-indent-tests))

(module data racket/base
  (require racket/runtime-path)
  (provide indent-test-data/)
  (module+ contract
    (provide (all-defined-out))
    (require json racket/list racket/contract)
    (define portable-indent-values ; lengths longer than 10 are not portable to JS
      (cons #\tab (inclusive-range 1 10)))
    (define portable-indent/c
      (or/c #\tab (integer-in 1 10)))
    (define compound-jsexpr/c
      (and/c jsexpr? (or/c hash? list?)))
    (define test-datum/c
      (list/c portable-indent/c compound-jsexpr/c)))
  (define-runtime-path indent-test-data/
      "indent-test-data/"))
(require 'data
         json
         racket/file
         racket/match
         rackunit
         rackunit/text-ui)

(define json-indent-tests
  (make-test-suite
   "json #:indent tests"
   (for*/list ([dir (in-list (directory-list indent-test-data/))]
               [abs (in-value (build-path indent-test-data/ dir))]
               #:when (directory-exists? abs))
     (test-suite
      (path->string dir)
      (parameterize ([current-directory abs])
        (match-define (and datum (list indent jsexpr))
          (file->value "datum.rktd"))
        (with-check-info (['directory (path->string dir)])
          (check-equal? (jsexpr->string #:indent indent jsexpr)
                        (let ([str (file->string "node.json")])
                          ;; remove trailing newline we added
                          (substring str 0 (sub1 (string-length str))))
                        "indentation should match")))))))

(define (failing-directories)
  (for*/list ([r (in-list (run-test json-indent-tests))]
              #:when (test-failure? r)
              [info (exn:test:check-stack (test-failure-result r))]
              #:when (eq? 'directory (check-info-name info)))
    (check-info-value info)))

(module* cli racket/base
  ;; In a shell, source `alias.sh` in this directory to be able to run `indent-test-data-cli`.
  (module* main #f
    (require racket/cmdline)
    (command-line
     #:program "indent-test-data-cli"
     #:usage-help "" "If given no <option>s or only `--redo-python`, equivalent to:"
     "  $ indent-test-data-cli --validate-all [ --redo-python ]"
     #:help-labels
     "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     "┃   Actions"
     "┃ First"
     #:multi [("--add-from-file" "-f") datum.rktd "Add test datum read from <datum.rktd>."
              (--add-from-file datum.rktd)]
     #:multi [("--add-s-exp" "-s") S-expression "Add test datum read from <S-expression>."
              (--add-from-file (read (open-input-string S-expression)))]
     #:help-labels
     "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     "┃ Second"
     #:once-each [("--add-random" "-r") count "Add <count> additional test data, chosen at random."
                  (--add-random count)]
     #:help-labels
     "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     "┃ Third"
     #:once-each [("--validate-all" "-a") ("Validate all previously added test data."
                                           "“Validating” does NOT test the `json` library:"
                                           "It instead tests the INPUT to those tests.")
                  (--validate-all #t)]
     #:multi [("--validate" "-v")
              dir ("Validate previously added test from `$indent-test-data/dir`."
                   "(If combined with `--validate-all`, has no additional effect.)"
                   "Data added by `--add-from-nat` and `--add-random` are always validated.")
              (--validate dir)]
     #:help-labels
     "    ──────────────────────────────────────────────"
     #:once-each [("--redo-python" "-p" ) "When validating, replace `python.json` files."
                  (--redo-python #t)]
     #:help-labels
     "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     #:once-each [("--realpath") "Print `$indent-test-data/`, followed by a newline, and exit"
                  (write-bytes (path->bytes indent-test-data/))
                  (newline)
                  (exit 0)]
     #:args ()
     (indent-test-data-cli)))

  (provide indent-test-data-cli
           --add-from-file
           --add-random
           --validate-all
           --validate
           --redo-python)

  ;
  ;
  ;
  ;
  ;
  ;                                                        ;;
  ;   ;;;;                                                 ;;
  ;    ;;             ;                                    ;;
  ;    ;;             ;                                    ;;
  ;    ;;   ;; ;;;  ;;;;;   ;;;;    ;; ;  ;; ;;;    ;;;;   ;;   ;;;;
  ;    ;;   ;;;  ;    ;    ;;   ;   ;;;   ;;;  ;   ;;  ;;  ;;  ;   ;
  ;    ;;   ;;   ;;   ;    ;    ;;  ;;    ;;   ;;       ;  ;;  ;
  ;    ;;   ;;   ;;   ;    ;    ;;  ;;    ;;   ;;       ;  ;;  ;;
  ;    ;;   ;;   ;;   ;    ;;;;;;;  ;;    ;;   ;;   ;;;;;  ;;   ;;;;
  ;    ;;   ;;   ;;   ;    ;        ;;    ;;   ;;  ;    ;  ;;      ;;
  ;    ;;   ;;   ;;   ;    ;        ;;    ;;   ;;  ;    ;  ;;      ;;
  ;    ;;   ;;   ;;   ;    ;;   ;   ;;    ;;   ;;  ;  ;;;  ;;  ;   ;;
  ;   ;;;;  ;;   ;;   ;;;   ;;;;    ;;    ;;   ;;  ;;;; ;; ;;;  ;;;;
  ;
  ;
  ;
  ;
  ;


  (require (for-syntax racket/base
                       racket/syntax)
           syntax/parse/define
           racket/contract
           racket/file
           racket/list
           (submod ".." data)
           (submod ".." data contract)
           (submod ".." add-validate))



  (define not-string/c
    (let ()
      (define (not-string/c x)
        (or (not (string? x))
            (λ (blame)
              (raise-blame-error
               blame x
               `(";\n boolean switches do not parse argument strings"
                 expected: "(not/c string?)" given: "~e")
               x))))
      (flat-contract-with-explanation not-string/c)))

  (define bool-flag-parameter/c
    (parameter/c not-string/c boolean?))

  (define (nat-flag-parameter/c parsed-cli-arg/c multi?)
    (parameter/c (or/c string?
                       parsed-cli-arg/c
                       ; avoid confusion with and/c
                       (if multi? (listof parsed-cli-arg/c) #f))
                 (if multi?
                     (listof parsed-cli-arg/c)
                     (or/c #f parsed-cli-arg/c))))

  (define make-flag-parameter
    (case-lambda
      [(name) ; This is a boolean switch: nothing to parse.
       (make-parameter #f (λ (x) (and x #t)) name)]
      [(name parse-arg parsed-arg-ok? init)
       (define (parse-string str)
         (define n (parse-arg str))
         (unless (parsed-arg-ok? n)
           (raise-arguments-error 'indent-test-data-cli
                                  "bad argument to switch"
                                  "switch" name
                                  "expected" parsed-arg-ok?
                                  "given" (unquoted-printing-string str)))
         n)
       (define (parse-new x)
         (if (string? x)
             (parse-string x)
             x))
       (define guard
         (cond
           [(null? init) ; List parameter: replace list or cons on new value.
            (λ (x)       #; (this atom) #|is short for|# #;(this (cons (parse-new atom) (this)))
              (if (null? x)
                  x
                  (cons (parse-new x) (this))))]
           [else
            parse-new]))
       (define this
         (make-parameter init guard name))
       this]))


  (begin-for-syntax
    (define-splicing-syntax-class flag-formal
      #:description "flag argument clause"
      #:attributes [kw name init-expr fun-arg/c definitions]
      (pattern
        (~seq (~describe "function formal"
                         (~seq kw:keyword
                               (~or* (~describe "argument name"
                                                name:id)
                                     (~describe "identifier with initializer"
                                                [name:id custom-init-expr:expr]))))

              (~describe "parameter description"
                         (~seq
                          --name:id
                          (~optional (~seq (~optional (~and #:multi multi?))
                                           (~var parsed-cli-arg/c-expr
                                                 (expr/c #'flat-contract?
                                                         #:name "cli contract expression"))
                                           (~optional
                                            (~seq #:parse
                                                  (~var parse-arg-expr
                                                        (expr/c #'(-> string? any/c)
                                                                #:name "#:parse expression")))))))))
        #:attr parsed-cli-arg/c (and (attribute parsed-cli-arg/c-expr.c)
                                     (format-id #'foo "~a/parsed/c" #'--name))
        #:attr fun-arg/c #`(~? (~? (and 'multi? (listof parsed-cli-arg/c))
                                   (or/c #f parsed-cli-arg/c))
                               not-string/c)
        #:with (~var param-expr (expr/c #'(~? (nat-flag-parameter/c parsed-cli-arg/c (~? 'multi? #f))
                                              bool-flag-parameter/c)
                                        #:name "flag parameter"))
        (quasisyntax/loc #'--name
          (make-flag-parameter '--name
                               (~? (~@ (~? parse-arg-expr.c string->number)
                                       parsed-cli-arg/c
                                       (~? (and 'multi? null) #f)))))
        #:attr init-expr #`(~? custom-init-expr (--name))
        #:attr definitions #`(begin
                               (~? (define parsed-cli-arg/c parsed-cli-arg/c-expr.c))
                               (define --name param-expr.c)))))


  (define-simple-macro (define-main (~describe "function header"
                                               (name:id arg:flag-formal ...))
                         body:expr ...+)
    #:with (~var lambda-expr (expr/c #'(->* [] [(~@ arg.kw arg.fun-arg/c) ...] any)))
    (quasisyntax/loc this-syntax
      (λ ({~@ arg.kw [arg.name arg.init-expr]} ...)
        body ...))
    (begin arg.definitions ...
           (define name lambda-expr.c)))

  (define-main (indent-test-data-cli
                #:add to-add --add-from-file #:multi test-datum/c #:parse file->value
                #:add-random num-random-to-add --add-random exact-positive-integer?
                #:redo-python? redo-python? --redo-python
                #:validate to-validate --validate #:multi path-string? #:parse values
                #:validate-all? [validate-all? (or (--validate-all)
                                                   (and (null? to-validate)
                                                        (null? to-add)
                                                        (not num-random-to-add)))]
                --validate-all)
    (for-each add to-add)
    (when num-random-to-add
      (define done (add-random num-random-to-add))
      (for-each display
                (if (= 1 num-random-to-add)
                    @list{Added randomly-chosen data in directory "@car[done]/".@"\n"}
                    (cons "Added randomly-chosen data in these directories:\n  "
                          (add-between done '("/\n  ") #:after-last '("/\n") #:splice? #t)))))
    (cond
      [validate-all?
       (validate-all #:redo-python? redo-python?)]
      [else
       (validate-list to-validate #:redo-python? redo-python?)])))

;
;
;
;
;
;
;
;                            ;
;                            ;
;    ;;;;  ;;    ;   ;;;;  ;;;;;   ;;;;    ;;;;;; ;;;
;   ;   ;   ;    ;  ;   ;    ;    ;;   ;   ;;  ;;;  ;;
;   ;       ;   ;;  ;        ;    ;    ;;  ;;   ;;   ;
;   ;;      ;;  ;   ;;       ;    ;    ;;  ;;   ;;   ;
;    ;;;;    ;  ;    ;;;;    ;    ;;;;;;;  ;;   ;;   ;
;       ;;   ;  ;       ;;   ;    ;        ;;   ;;   ;
;       ;;   ; ;;       ;;   ;    ;        ;;   ;;   ;
;   ;   ;;    ;;    ;   ;;   ;    ;;   ;   ;;   ;;   ;
;    ;;;;     ;;     ;;;;    ;;;   ;;;;    ;;   ;;   ;
;             ;;
;             ;
;             ;
;           ;;;
;

(module system racket/base
  (require json
           racket/contract
           racket/list
           racket/string
           racket/system
           (submod ".." data contract))
  (provide (contract-out
            [node-write-json
             (-> portable-indent/c compound-jsexpr/c any)]
            [python-write-json
             (-> portable-indent/c compound-jsexpr/c any)]))
  (define/contract ((make-runner name #:try-first [try-first #f]
                                 make-args)
                    indent js)
    (->* [string?
          (-> string? string? (listof string?))]
         [#:try-first (or/c #f string?)]
         (-> portable-indent/c compound-jsexpr/c any))
    (define args
      (make-args
       (if (eqv? #\tab indent)
           @quote{"\t"}
           (number->string indent))
       ;; no easy JS equivalent to sort_keys, so make Racket do it
       (jsexpr->string js)))
    (define (err msg . extra-fields)
      (apply raise-arguments-error
             (string->symbol name)
             msg
             (flatten (list extra-fields
                            "arguments..."
                            (unquoted-printing-string
                             (string-append* (for/list ([v args])
                                               (format "\n   ~e" v))))))))
    (define prog
      (or (for*/or ([find (list (compose1 getenv string-upcase)
                                find-executable-path)]
                    [try (list try-first name)]
                    #:when try)
            (find try))
          (err "command not found"
               (if try-first
                   `("variants tried" ,(list try-first name))
                   null))))
    (define code
      (parameterize ([current-input-port (open-input-string "")])
        (apply system*/exit-code prog args)))
    (unless (zero? code)
      (err "command failed"
           "exit code" code
           name prog))
    (newline)
    (cons prog args))
  (define node-write-json
    (make-runner
     "node"
     (λ (indent json)
       (list "-e"
             @string-append{
 process.stdout.write(JSON.stringify(@|json|,null,@|indent|))
 }))))
  (define python-write-json
    (make-runner
     "python" #:try-first "python3"
     (λ (indent json)
       (list "-c"
             @string-append{
 import json
 import sys
 json.dump(json.loads(sys.argv[-1]),
 @""       sys.stdout,
 @""       indent=@|indent|,
 @""       sort_keys=True,
 @""       ensure_ascii=False)
}
             json)))))


;
;
;
;
;
;                               ;;
;                               ;;
;                               ;;
;                               ;;
;   ;; ;   ;;;;   ;; ;;;    ;;;;;;   ;;;;    ;;;;;; ;;;
;   ;;;   ;;  ;;  ;;;  ;   ;;   ;;  ;;   ;   ;;  ;;;  ;;
;   ;;         ;  ;;   ;;  ;    ;;  ;    ;;  ;;   ;;   ;
;   ;;         ;  ;;   ;;  ;    ;;  ;     ;  ;;   ;;   ;
;   ;;     ;;;;;  ;;   ;;  ;    ;;  ;     ;  ;;   ;;   ;
;   ;;    ;    ;  ;;   ;;  ;    ;;  ;     ;  ;;   ;;   ;
;   ;;    ;    ;  ;;   ;;  ;    ;;  ;    ;;  ;;   ;;   ;
;   ;;    ;  ;;;  ;;   ;;  ;;   ;;  ;;   ;   ;;   ;;   ;
;   ;;    ;;;; ;; ;;   ;;   ;;;;;;   ;;;;    ;;   ;;   ;
;
;
;
;
;

(module random racket/base
  (require racket/contract
           racket/flonum
           racket/match
           racket/string
           racket/symbol
           (submod ".." data contract))
  (provide (contract-out
            [random-compound-jsexpr
             (-> compound-jsexpr/c)]))

  (define (coin-toss)
    (zero? (random 2)))

  (define vocab
    #(Age Air Art Bean Bell Big Bird Boat Book Cake Cat Cup Day Dog Dry Ear Ever Eye Face Few Fish
Game Gift Hat Hen Hot How Ice Key Kind Leaf Love Low Map May Moon Net New Old Path Put Rain Read
Run Safe Say Ship Sky Sun Swim Talk Tea Tidy Toe Town Use Very Wait Walk Warm Wet Who Why Win
Wise Wish Yard Yet Zoo))
  (define (random-words [n #f])
    (for/list ([__ (in-range (or n (add1 (modulo (random 19) 12))))])
      (vector-ref vocab (random (vector-length vocab)))))
  (define (random-string [n #f])
    (match (random-words)
      [(list sym)
       (symbol->immutable-string sym)]
      [lst
       (string-append* (map symbol->immutable-string lst))]))
  (define (random-symbol [n #f])
    (match (random-words)
      [(list sym)
       sym]
      [lst
       (string->symbol (string-append* (map symbol->immutable-string lst)))]))

  (define (random-portable-number)
    ;; JS does not distinguish inexact integers from exact.
    (define x
      (flsingle
       ((if (coin-toss) - values)
        (* (random) (expt 10 (random 8))))))
    (if (and (rational? x) (not (integer? x)))
        x
        (random-portable-number)))

  (define (random-atomic-jsexpr)
    (match (random 5)
      [0 (random-string)]
      [1 (random-portable-number)]
      [2 'null]
      [3 #f]
      [4 #t]))

  (define (random-compound-jsexpr [max-nesting 8])
    (define max-length 5)
    (define lst
      (for/list ([i (in-range (random max-length))])
        (if (or (zero? max-nesting)
                (coin-toss))
            (random-atomic-jsexpr)
            (random-compound-jsexpr (sub1 max-nesting)))))
    (if (coin-toss)
        lst
        (for/fold ([hsh #hasheq()])
                  ([rhs (in-list lst)])
          (let retry ()
            (define key (random-symbol))
            (if (hash-has-key? hsh key)
                (retry)
                (hash-set hsh key rhs)))))))

;
;
;
;
;
;                ;;       ;;                        ;;  ;;       ;;
;                ;;       ;;                        ;;  ;;       ;;
;                ;;       ;;                        ;;           ;;           ;
;                ;;       ;;                        ;;           ;;           ;
;    ;;;;    ;;;;;;   ;;;;;;       ;;    ;   ;;;;   ;;  ;;   ;;;;;;   ;;;;  ;;;;;   ;;;;
;   ;;  ;;  ;;   ;;  ;;   ;;        ;    ;  ;;  ;;  ;;  ;;  ;;   ;;  ;;  ;;   ;    ;;   ;
;        ;  ;    ;;  ;    ;;        ;   ;;       ;  ;;  ;;  ;    ;;       ;   ;    ;    ;;
;        ;  ;    ;;  ;    ;;  ;;;;  ;;  ;        ;  ;;  ;;  ;    ;;       ;   ;    ;    ;;
;    ;;;;;  ;    ;;  ;    ;;         ;  ;    ;;;;;  ;;  ;;  ;    ;;   ;;;;;   ;    ;;;;;;;
;   ;    ;  ;    ;;  ;    ;;         ;  ;   ;    ;  ;;  ;;  ;    ;;  ;    ;   ;    ;
;   ;    ;  ;    ;;  ;    ;;         ; ;    ;    ;  ;;  ;;  ;    ;;  ;    ;   ;    ;
;   ;  ;;;  ;;   ;;  ;;   ;;          ;;    ;  ;;;  ;;  ;;  ;;   ;;  ;  ;;;   ;    ;;   ;
;   ;;;; ;;  ;;;;;;   ;;;;;;          ;;    ;;;; ;; ;;; ;;   ;;;;;;  ;;;; ;;  ;;;   ;;;;
;
;
;
;
;

(module add-validate racket/base
  (require racket/contract
           racket/file
           racket/list
           racket/match
           racket/pretty
           (only-in file/sha1 bytes->hex-string)
           rackunit
           json
           (submod ".." data)
           (submod ".." data contract)
           (submod ".." system)
           (submod ".." random))
  (provide (contract-out
            [add
             (-> test-datum/c string?)]
            [add-random
             (-> exact-positive-integer? (listof string?))]
            [validate
             (->* [string?]
                  [#:redo-python? any/c]
                  any)]
            [validate-list
             (->* [(listof string?)]
                  [#:redo-python? any/c]
                  any)]
            [validate-all
             (->* []
                  [#:redo-python? any/c]
                  any)]))

  (define (file->short-hash pth)
    (substring (bytes->hex-string (call-with-input-file* pth
                                    sha256-bytes))
               0 7))


  (define (pretty-write-to-file x pth)
    (call-with-output-file* pth
      (λ (out)
        (pretty-write x out))))

  (define-values [node-write-datum
                  python-write-datum]
    (let ()
      (define (rm-f pth)
        (delete-directory/files pth #:must-exist? #f))
      (define (run-writer name proc datum #:exists [exists 'error])
        (define json-pth (string-append name ".json"))
        (define rktd-pth (string-append "args." name ".rktd"))
        (when (eq? exists 'redo)
          (for-each rm-f (list json-pth rktd-pth)))
        (unless (and (eq? exists 'ignore)
                     (file-exists? json-pth))
          (rm-f rktd-pth)
          (pretty-write-to-file (with-output-to-file json-pth
                                  (λ ()
                                    (apply proc datum)))
                                rktd-pth)))
      (define (node-write-datum datum)
        (run-writer "node" node-write-json datum))
      (define (python-write-datum datum #:redo-python? [redo-python? #f])
        (run-writer "python" python-write-json datum #:exists (if redo-python?
                                                                  'redo
                                                                  'ignore)))
      (values node-write-datum
              python-write-datum)))

  (define (add datum)
    (define incoming
      (make-temporary-directory #:base-dir indent-test-data/))
    (define dir
      (parameterize ([current-directory incoming])
        (pretty-write-to-file datum "datum.rktd")
        (node-write-datum datum)
        (file->short-hash "datum.rktd")))
    (rename-file-or-directory incoming (build-path indent-test-data/ dir))
    (validate dir)
    dir)

  (define (add-random num-random-to-add)
    (for/list ([i (in-inclusive-range 1 num-random-to-add)]
               [indent (in-cycle (shuffle portable-indent-values))])
      ;; Use all indentations as equally with as possible,
      ;; with a random selection for the remainder.
      (add (list indent (random-compound-jsexpr)))))

  (define (file->jsexpr pth)
    (string->jsexpr ; ensure whole file is consumed
     (file->string pth)))

  (define (validate dir #:redo-python? [redo-python? #f])
    (parameterize ([current-directory (build-path indent-test-data/ dir)])
      (match-define (and datum (list indent jsexpr))
        (file->value "datum.rktd"))
      (python-write-datum datum #:redo-python? redo-python?)
      (call-with-atomic-output-file
       "debug.json"
       (λ (out _tmp)
         (write-json #:indent indent jsexpr out)
         (newline out)))
      (with-check-info
          (['directory dir])
        (check-equal? (file->short-hash "datum.rktd")
                      dir
                      "datum.rktd should contain correct value")
        (check-equal? (file->jsexpr "node.json")
                      jsexpr
                      "node.json should contain correct value")
        (check-equal? (file->string "python.json")
                      (file->string "node.json")
                      "python.json should be identical to node.json"))))

  (define (validate-list dirs #:redo-python? [redo-python? #f])
    (for-each (λ (dir)
                (validate dir #:redo-python? redo-python?))
              dirs))

  (define (validate-all #:redo-python? [redo-python? #f])
    (validate-list #:redo-python? redo-python?
                   (parameterize ([current-directory indent-test-data/])
                     (for/list ([pth (in-list (directory-list))]
                                #:when (directory-exists? pth))
                       (path->string pth))))))
