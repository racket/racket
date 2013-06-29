#lang racket/base

(require (for-syntax racket/base)
         racket/list
         racket/string
         racket/contract
         racket/match
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         (rename-in srfi/26 [cut //])
         (only-in srfi/1 break)
         unstable/contract)

(define-syntax (test stx) #'(begin)) ;; TODO: convert my test into DrRacket's test framework

;; An error message has many fragments. The fragments will be concatenated
;; before being presented to the user. Some fragment are simply string.
(struct msg-fragment:str (str) #:transparent)

;; Some fragment are arbitrary values. They will be turned into snips if the error message display
;; context supports them. Otherwise, they will be turned into a string.
(struct msg-fragment:v (v) #:transparent)

;; colored-msg-fragment represent a fragment of text in the error message that refers (in English) 
;; to a particular piece of the code. DrRacket will highlight both the fragment text of the error message
;; and the piece with the same color.
;; 
;; locs      : which srcloc to highlight, one or many
;; frags     : which fragments of text to highlight. (nested coloring in the error test is not allowed)
;; important : when true, the srcloc corresponding to this fragment will be highlighted even in contexts 
;;             that do not support highlighting of the error message text.
;; color     : if false, DrRacket will assign a color to each fragment, from left to right order of the 
;;             messag text. Otherwise it should be a symbolic color (TBA).
(struct colored-msg-fragment (locs frags important color) #:transparent)

;; msg-fragment? : Returns true if v is a fragment.
(define (msg-fragment? v) (or (msg-fragment:str v) (msg-fragment:v v) (colored-msg-fragment v)))

;; srcloc-syntax/c : Contract for specifications of code piece to highlight.
(define srcloc-syntax/c (rename-contract (or/c srcloc? syntax? (listof (or/c srcloc? syntax?))) 'srcloc-syntax/c))

;; additional-highligts can specify their color
(define additional-highlights/c (listof (or/c srcloc-syntax/c (list/c srcloc-syntax/c symbol?))))
(define color/c (or/c #f symbol?))

;; colored-error-message : Exceptions that support highlighting contain a colored-error-message
;; in addition to a string. The string message is automatically generated from 
;; the colored-error-message for backward compatibility.
;; A colored-error-message has a list of fragments (some of which will be highlighted) and a list 
;; of additional source locations. These additional location will also be highlighted in the code,
;; even though they do not correspond to any section of the text of the error message.
(struct colored-error-message (fragments additional-highlights) #:transparent)
(provide/contract [struct colored-error-message
                          ([fragments (listof msg-fragment?)]
                           [additional-highlights additional-highlights/c])]
                  [struct msg-fragment:str ([str string?])]
                  [struct msg-fragment:v ([v any/c])]
                  [struct colored-msg-fragment ([locs srcloc-syntax/c]
                                                [frags (listof (or/c msg-fragment:str? msg-fragment:v?))]
                                                [important boolean?]
                                                [color color/c])])

;; prop:exn:colored-message : The property of exceptions that contain colored-message information.
;;     The property's value is a function that when given an exception, returns the colored-error-message.
(provide prop:exn:colored-message exn:colored-message? exn:colored-message-accessor)
(define-values (prop:exn:colored-message
                exn:colored-message?
                exn:colored-message-accessor)
  (make-struct-type-property
   'colored-message
   (lambda (v str-info)
     (contract (exn? . -> . colored-error-message?) v
               'struct-definition 'color-error-accessor))))

;; get-error-message/color : When given an exception, if that exception contains coloring information,
;;    returns it, otherwise, returns a colored-error-message that capture the information provided by
;;    by field message and the srclocs property (if any) of the exception.
(provide/contract [get-error-message/color (exn? . -> . colored-error-message?)])
(define (get-error-message/color exn)
  (cond [(exn:colored-message? exn) ((exn:colored-message-accessor exn) exn)]
        [(exn:srclocs? exn)
         (colored-error-message (list (msg-fragment:str (exn-message exn)))
                                ((exn:srclocs-accessor exn) exn))]
        [else
         (colored-error-message (list (msg-fragment:str (exn-message exn))) empty)]))

(provide/contract [get-error-colored-srclocs (exn? . -> . (listof (list/c srcloc-syntax/c color/c)))])
(define (get-error-colored-srclocs exn)
  (get-message-colored-srclocs (get-error-message/color exn)))

(provide/contract [get-message-colored-srclocs (colored-error-message? . -> . (listof (list/c srcloc-syntax/c color/c)))])
(define (get-message-colored-srclocs msg)
  (define (promote srcloc) (if (list? srcloc) srcloc (list srcloc #f)))
  (map promote
       (append
        (append*
         (filter-map (// match <> [(colored-msg-fragment locs _ _ c)
                                   (map (// list <> c) (if (list? locs) locs (list locs)))] [_ #f])
                     (colored-error-message-fragments msg)))
        (colored-error-message-additional-highlights msg))))

(define lex (lexer
             [(eof) empty]
             ["~|" (cons 'TildaPipe (lex input-port))]
             ["|~" (cons 'PipeTilda (lex input-port))]
             ["~v" (cons 'TildaV (lex input-port))]
             [(:or any-char "~~")
              (begin
                (define rst (lex input-port))
                (match rst
                  [(list (? string? str) tail ...)
                   (cons (string-append lexeme str) tail)]
                  [_ (cons lexeme rst)]))]))

(test 'lex (check-equal? (lex (open-input-string "~~foo ~| ~~| bar ~v|~ foo ~ "))
                         '("~~foo " TildaPipe " ~~| bar " TildaV PipeTilda " foo ~ ")))

(define-syntax (match? stx)
  (syntax-case stx ()
    [(_ v pattern) (syntax/loc stx
                     (match v [pattern #t] [_ #f]))]))

(define (check-tildas-are-paired parsed)
  (let loop ([tildas (filter (// match? <> (or 'TildaPipe 'PipeTilda)) parsed)] [i 1])
    (match tildas
      [(list) (void)]
      [(list 'PipeTilda rst ...)
       (error 'colored-format "The ~ath tilda-pipe is an unexpected close" i)]
      [(list one) (error 'colored-format "There is an dangling ~a" one)]
      [(list 'TildaPipe 'TildaPipe rst ...)
       (error 'colored-format "The ~ath tilda-pipe is an unexpected open" (add1 i))]
      [(list 'TildaPipe 'PipeTilda rst ...)
       (loop rst (+ 2 i))])))

(test 'check-tildas-are-paired
      (define (go str) (check-tildas-are-paired (lex (open-input-string str))))
      (check-exn-msg exn:fail? "dangling" (lambda () (go "~| |~ ~|")))
      (check-exn-msg exn:fail? #rx"3.*close" (lambda () (go "~| |~ |~ |~")))
      (check-exn-msg exn:fail? #rx"6.*open" (lambda () (go "~| |~ ~| |~ ~| ~|")))
      (check-exn-msg exn:fail? #rx"2.*open" (lambda () (go "~| ~| |~ |~")))
      (check-exn-msg exn:fail? #rx"3.*close" (lambda () (go "~| ~~| |~ |~"))))

(define (count-tildas fmt)
  (- (length (regexp-match* #rx"~" fmt))
     (* 2 (length (regexp-match* #rx"~~" fmt)))))

(define (check-arg who args n)
  (when (< (length args) n)
    (error 'colored-error-message "Missing arguments for ~a" who)))

(define (colored-format:v args)
  (check-arg "~v" args 1)
  (values (msg-fragment:v (first args))
          (rest args)))

(define (colored-format:str fmt args)
  (define n (count-tildas fmt))
  (check-arg fmt args n)
  (values (msg-fragment:str (apply format fmt (take args n)))
          (drop args n)))

(define (colored-format:str-or-v fmt-or-v args)
  (if (eq? fmt-or-v 'TildaV)
      (colored-format:v args)
      (colored-format:str fmt-or-v args)))

(define (colored-format:TildaPipe fragments args)
  (check-arg "~|" args 1)
  (define-values (sub rest-args)
    (let loop ([fragments fragments] [args (rest args)])
      (if (empty? fragments)
          (values empty args)
          (let ()
            (define-values (f rest-args) (colored-format:str-or-v (first fragments) args))
            (define-values (rest-fs rest-rest-args) (loop (rest fragments) rest-args))
            (values (cons f rest-fs) rest-rest-args)))))
  (define the-arg (first args))
  (match the-arg
    [(list loc imp col other ..1)
     (error 'colored-format "Extraneous arguments to TildaPipe: ~a" other)]
    [_ (void)])
  
  (contract (or/c srcloc-syntax/c
                  (list/c srcloc-syntax/c boolean?)
                  (list/c srcloc-syntax/c symbol?)
                  (list/c srcloc-syntax/c boolean? symbol?)
                  (list/c srcloc-syntax/c symbol? boolean?))
            the-arg 'caller 'TildaPipe)
  
  (define is-important (and (list? the-arg) (findf (// eq? <> #t) the-arg)))
  (define color (and (list? the-arg) (findf symbol? the-arg)))
  (values (colored-msg-fragment (if (list? the-arg) (first the-arg) the-arg) sub is-important color) rest-args))

(define colored-format/c (([fmt string?]) (#:additional-highlights [additional-highlights additional-highlights/c]) #:rest [_ any/c]
                                                   . ->i . [_ colored-error-message?]))
(provide/contract [colored-format colored-format/c])

;; colored-format : Takes a format string and a number of arguments, and produces a string where each
;;                  format marker has been replaced by their corresponding argument. This function support 
;;                  all the formatting option of fprintf, plus:
;;
;;    ~| ...  |~  : The text between ~| and |~ will be highlighted with the same color as the corresponding piece
;;                  of code. Arbitrary text and formatting options can occur between ~| and |~, but not another ~| |~
;;                  (there is no nesting of ~| |~).
;;                  The argument is either a srcloc-syntax/c, or a list contain a srcloc-syntax/c in first position and
;;                  then one or two of (in either order): a boolean indicating whether this highlight is important
;;                                                        a symbol, indication the highlight color
;;                  The argument to ~| |~ should be given first, before the arguments for the formatting options appearing
;;                  between ~| and |~.
;;
;;     ~v         : Inserts an arbitrary value in this position. If the value can be converted to a snip, it will be,
;;                  otherwise ~v is equivalent to ~a. 
;;
(define (colored-format fmt #:additional-highlights [additional-highlights empty] . args)
  (define parsed (lex (open-input-string fmt)))

  (check-tildas-are-paired parsed)
  
  (define fragments
    (let loop ([parsed parsed] [args args])
      (match parsed
        [(list) (unless (empty? args)
                  (error 'colored-format "There are ~a unused arguments" (length args)))
                empty]
        [(list 'TildaPipe tail ...)
         (define-values (left right) (break (// match? <> 'PipeTilda) tail))
         (define-values (result rest-args) (colored-format:TildaPipe left args))
         (cons result (loop (rest right) rest-args))]
        [(list f tail ...)
         (define-values (result rest-args) (colored-format:str-or-v f args))
         (cons result (loop tail rest-args))])))
  
  (colored-error-message fragments additional-highlights))

(test 'colored-format
      (check-equal? (colored-format "  ") (colored-error-message (list (msg-fragment:str "  ")) empty))
      (check-equal? (colored-format "--~a--" 5)
                    (colored-error-message (list (msg-fragment:str "--5--")) empty))
      (check-match (colored-format "~|--~a--|~ foo" #'here 5)
                   (colored-error-message (list (colored-msg-fragment (app syntax-e 'here) (list (msg-fragment:str "--5--")) #f #f) (msg-fragment:str " foo")) empty))
      
      (check-match (colored-format "~|--~a--~v--~a--|~ foo ~v bar ~||~ ~a" #'first 1 "inserted" 2 'inserted-too (list #'second 'red) 3 #:additional-highlights '(a b))
                   (colored-error-message (list (colored-msg-fragment
                                                 (app syntax-e 'first)
                                                 (list (msg-fragment:str "--1--")
                                                       (msg-fragment:v "inserted")
                                                       (msg-fragment:str "--2--")) #f #f)
                                                (msg-fragment:str " foo ")
                                                (msg-fragment:v 'inserted-too)
                                                (msg-fragment:str " bar ")
                                                (colored-msg-fragment (app syntax-e 'second) empty #f 'red)
                                                (msg-fragment:str " 3"))
                                          '(a b)))
      (check-match (colored-format " ~~ ~a ~a ~a ~a ~||~~~|" 1 2 3 4 #'here)
                   (colored-error-message (list (msg-fragment:str " ~ 1 2 3 4 ")
                                                (colored-msg-fragment (app syntax-e 'here) empty #f #f)
                                                (msg-fragment:str "~|")) empty))
      (check-exn-msg exn:fail? #rx"2.*unused" (lambda () (colored-format " ~a " 1 2 3))))

(define (uncolor-message msg)
  (let loop ([f (colored-error-message-fragments msg)])
    (match f
      [(msg-fragment:str str) str]
      [(msg-fragment:v v) (format "~a" v)]
      [(colored-msg-fragment locs frags imp col) (loop frags)]
      [(? list?) (string-append* (map loop f))])))

(define (important-srclocs msg)
  (append
   (flatten
    (filter-map (// match <> [(colored-msg-fragment locs _ #t _) locs] [_ #f])
                (colored-error-message-fragments msg)))
   (colored-error-message-additional-highlights msg)))

(struct exn:fail:colored:syntax exn:fail:syntax (colored-message)
        #:transparent
        #:property prop:exn:colored-message (lambda (v)
                                              (define vec (struct->vector v))
                                              (vector-ref vec (sub1 (vector-length vec)))))

;; raise-colored-syntax-error : Formats the message string with colored-format, then raises a exn:fail:colored:syntax error. 
;;                              The message and srcloc fields of the exception are populated from the information 
;;                              in the fmt. additional-highlights specifies srclocs that should be highlighted, in addition
;;                              to the highlights used to explicate the correspondance between the text and the piece of codes.
(provide/contract [raise-colored-syntax-error colored-format/c])
(define (raise-colored-syntax-error fmt #:additional-highlights [additional-highlights empty] . args)
  (define formatted (apply colored-format fmt #:additional-highlights additional-highlights args))
  (raise (exn:fail:colored:syntax (uncolor-message formatted)
                                  (current-continuation-marks)
                                  (important-srclocs formatted)
                                  formatted)))

(define (catch-the-error thunk) (with-handlers ([void (lambda (e) e)]) (thunk)))

(test 'raise-colored-syntax-error
      (check-exn-msg exn:fail:colored:syntax? #rx"only one part"
                     (lambda () (raise-colored-syntax-error "~|cond|~: expected a clause with a question and answer, but found a clause with only ~|one part|~"
                                                            #'stx #'question)))
      
      (check-match (get-error-message/color (catch-the-error (lambda () (raise-colored-syntax-error "~|cond|~: expected a clause with a question and answer, but found a clause with only ~|one part|~"
                                                                                                    #'stx #'question))))
                   (colored-error-message (list (colored-msg-fragment
                                                 (? syntax?)
                                                 (list (msg-fragment:str "cond")) #f #f)
                                                (msg-fragment:str ": expected a clause with a question and answer, but found a clause with only ")
                                                (colored-msg-fragment (? syntax?) (list (msg-fragment:str "one part")) #f #f))
                                          empty)))


(test 'get-error-messages/color
      (check-equal? (get-error-message/color (exn:fail:colored:syntax "ah!" (current-continuation-marks) (list #'here) (colored-error-message 1 2)))
                    (colored-error-message 1 2))
      
      (check-match (important-srclocs (colored-format "~|1|~" (list #'first #t) #:additional-highlights '(a b)))
                   (list (app syntax-e 'first) a b))
      (check-match (get-error-colored-srclocs (catch-the-error (lambda () (raise-colored-syntax-error "~|1|~ ~||~" (list #'first #t) (list #'second 'red) #:additional-highlights (list #'a #'b)))))
                   (list (list (app syntax-e 'first) #f) (list (app syntax-e 'second) 'red) (list (app syntax-e 'a) #f) (list (app syntax-e 'b) #f))))

#|
(provide insert-colored-message)
(define (insert-colored-message msg repl clickback)
  (send repl begin-edit-sequence)
  (define port (send repl get-err-port))
  (let loop ([fragments (colored-error-message-fragments msg)])
    (for ([f fragments])
      (match f
        [(msg-fragment:str str) (display str port)]
        [(msg-fragment:v v) (write v port)]
        [(colored-msg-fragment locs frags imp col)
         (define start (send repl get-insertion-point))
         (for-each loop frags)
         (define end (send repl get-insertion-point))
         (send repl higlight-range start end col #t)
         (send repl set-clickback start end (lambda () (clickback f)))])))
  (send repl highlight-errors (get-message-colored-srclocs msg))
  
  ;; need to flush here so that error annotations inserted in next line
  ;; don't get erased if this output were to happen after the insertion
  (flush-output (send repl get-err-port))
  
  (send repl end-edit-sequence))
|#
