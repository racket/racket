#lang scheme/base

(require mzlib/etc
         mzlib/list
         (for-syntax "firstorder.rkt"
                     scheme/base))

(provide rewrite-contract-error-message
         reraise-rewriten-lookup-error-message
         get-rewriten-error-message
         plural
         raise-not-bound-error
         argcount-error-message)

(define (reraise-rewriten-lookup-error-message e id was-in-app-position)
  (let ([var-or-function (if was-in-app-position "function" "variable")])
    (raise-syntax-error
     #f
     (format "this ~a is not defined" var-or-function)
     id)))

(define (exn-needs-rewriting? exn)
  (exn:fail:contract? exn))

(define (ensure-number n-or-str)
  (if (string? n-or-str) (string->number n-or-str) n-or-str))

(define (plural n)
  (if (> (ensure-number n) 1) "s" ""))

(define (raise-not-bound-error id)
  (if (syntax-property id 'was-in-app-position)
      (raise-syntax-error
       #f
       "this function is not defined"
       id)
      (raise-syntax-error
       #f
       "this variable is not defined"
       id)))

(define (argcount-error-message name arity found [at-least #f])
  (define arity:n (ensure-number arity))
  (define found:n (ensure-number found))
  (define fn-is-large (> arity:n found:n))
  (format "~a~aexpects ~a~a~a argument~a, but found ~a~a"
          (or name "") (if name ": " "")
          (if at-least "at least " "")
          (if (or (= arity:n 0) fn-is-large) "" "only ")
          (if (= arity:n 0) "no" arity:n) (plural arity:n)
          (if (and (not (= found:n 0)) fn-is-large) "only " "")
          (if (= found:n 0) "none" found:n)))

(define (format-enum conj l)
  (if (= (length l) 2)
      (format "~a ~a ~a" (car l) conj (cadr l))
      (apply string-append
             (let loop ([l l])
               (cond
                [(null? (cdr l)) l]
                [(null? (cddr l))
                 (list* (car l) ", " conj " " (loop (cdr l)))]
                [else
                 (list* (car l) ", " (loop (cdr l)))])))))

(define (contract-to-desc ctc)
  (with-handlers ([exn:fail:read? (lambda (exn) ctc)])
    (define s (read (open-input-string ctc)))
    (let loop ([s s])
      (cond
       [(not s) "false"]
       [(and (symbol? s) (regexp-match? #rx"[?]$" (symbol->string s)))
        (define str (symbol->string s))
        (format "a~a ~a"
                (if (and ((string-length str) . > . 0)
                         (memv (string-ref str 0) '(#\a #\e #\i #\o #\u)))
                    "n"
                    "")
                (substring str 0 (sub1 (string-length str))))]
       [(null? s) "an impossible value"]
       [(not (list? s)) ctc] ;; ???
       [(eq? 'or/c (car s))
        (format-enum "or" (map loop (cdr s)))]
       [(eq? 'and/c (car s))
        (string-append "a value that is " (format-enum "and" (map loop (cdr s))))]
       [(eq? 'not/c (car s))
        (format "a value that is not ~a" (loop (cadr s)))]
       [(and (eq? '>/c (car s)) (zero? (cadr s)))
        "a positive number"]
       [(and (eq? '</c (car s)) (zero? (cadr s)))
        "a negative number"]
       [(and (eq? '>=/c (car s)) (zero? (cadr s)))
        "a non-negative number"]
       [else ctc]))))

(define (contract-error-message ctc given pos)
  (define d (contract-to-desc ctc))
  (format "expects ~a~a~a~a, given ~a"
          d
          (if pos " as " "")
          (or pos "")
          (if pos " argument" "")
          given))

(define (rewrite-contract-error-message msg)
  (define replacements
    (list (list #rx"application: not a procedure;\n [^\n]*?\n  given: ([^\n]*)(?:\n  arguments[.][.][.]:(?: [[]none[]]|(?:\n   [^\n]*)*))?"
                (lambda (all one)
                  (format "function call: expected a function after the open parenthesis, but received ~a" one)))
          (list #rx"([^\n]*): undefined;\n cannot reference an identifier before its definition"
                (lambda (all one) (format "~a is used here before its definition" one)))
          (list #rx"expects argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expected argument of type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #rx"expects type (<([^>]+)>)"
                (lambda (all one two) (format "expects a ~a" two)))
          (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: at least (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
                (lambda (all one two three) (argcount-error-message one two three #t)))
          (list #px"([^\n]*): arity mismatch;\n[^\n]*\n  expected[^:]*: (\\d+)\n  given[^:]*: (\\d+)(?:\n  arguments[.][.][.]:(?:\n   [^\n]*)*)?"
                (lambda (all one two three) (argcount-error-message one two three)))
          (list #px"contract violation\n  expected: (.*?)\n  given: ([^\n]*)(?:\n  argument position: ([^\n]*))?"
                (lambda (all ctc given pos) (contract-error-message ctc given pos)))
          (list #rx"^procedure "
                (lambda (all) ""))
          (list #rx", given: "
                (lambda (all) ", given "))
          (list #rx"; other arguments were:.*"
                (lambda (all) ""))
          (list #px"(?:\n  other arguments[.][.][.]:(?:\n   [^\n]*)*)"
                (lambda (all) ""))
          (list #rx"expects a (struct:)"
                (lambda (all one) "expects a "))
          (list #rx"list or cyclic list"
                (lambda (all) "list"))
          (list #rx"assignment disallowed;\n cannot set variable before its definition\n  variable:"
                (lambda (all) "cannot set variable before its definition:"))
          ;; When do these show up? I see only `#<image>' errors, currently.
          (list (regexp-quote "#(struct:object:image% ...)")
                (lambda (all) "an image"))
          (list (regexp-quote "#(struct:object:image-snip% ...)")
                (lambda (all) "an image"))
          (list (regexp-quote "#(struct:object:cache-image-snip% ...)")
                (lambda (all) "an image"))))
  (for/fold ([msg msg]) ([repl. replacements])
    (regexp-replace* (first repl.) msg (second repl.))))

(define (get-rewriten-error-message exn)
  (if (exn-needs-rewriting? exn)
      (rewrite-contract-error-message (exn-message exn))
      (exn-message exn)))
