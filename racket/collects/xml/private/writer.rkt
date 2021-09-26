#lang racket/base
(require racket/contract
         "structures.rkt")

; Within this file, `Dent` is used in comments and `dent` is used in code
; to refer to an indentation
(define indentation? (or/c 'none 'classic 'peek 'scan))

(provide/contract
 [write-xml ((document?) (output-port?) . ->* . void?)]
 [display-xml ((document?) (output-port? #:indentation indentation?) . ->* . void?)]
 [write-xml/content ((content/c) (output-port?) . ->* . void?)]
 [display-xml/content ((content/c) (output-port? #:indentation indentation?) . ->* . void?)]
 [empty-tag-shorthand (parameter/c (or/c (symbols 'always 'never) (listof symbol?)))]
 [html-empty-tags (listof symbol?)]
 [html-unescaped-tags (listof symbol?)]
 [current-unescaped-tags (parameter/c (listof symbol?))])

(define html-empty-tags
  '(param meta link isindex input img hr frame col br basefont base area))

;; (empty-tag-shorthand) : (U 'always 'never (listof Symbol))
(define empty-tag-shorthand
  (make-parameter 
   html-empty-tags
   (lambda (x)
     (if (or (eq? x 'always) (eq? x 'never) (and (list? x) (andmap symbol? x)))
         x
         (error 'empty-tag-shorthand "expected 'always, 'never, or a list of symbols: received ~e" x)))))

(define html-unescaped-tags
  '(script style))

(define current-unescaped-tags
  (make-parameter null))

;; indent : Nat Output-port -> Void
(define (indent n out)
  (newline out)
  (let loop ([n n])
    (unless (zero? n)
      (display #\space out)
      (loop (sub1 n)))))

;; write-xml/content : Content [Output-port] -> Void
(define (write-xml/content c [out (current-output-port)])
  (display-xml/content c out #:indentation 'none))

;; display-xml/content : Content [Output-port] [#:indentation Dent] -> Void
(define (display-xml/content c [out (current-output-port)]
                             #:indentation [dent 'classic])
  (write-xml-content c 0 dent out))

;; write-xml : Document [Output-port] -> Void
(define (write-xml doc [out (current-output-port)])
  (display-xml doc out #:indentation 'none))

;; display-xml : Document [Output-port] [#:indentation Dent] -> Void
(define (display-xml doc [out (current-output-port)]
                     #:indentation [dent 'classic])
  (define doc-misc (document-misc doc))
  (let ([prolog (document-prolog doc)])
    (display-outside-misc (prolog-misc prolog) out)
    (display-dtd (prolog-dtd prolog) out)
    (display-outside-misc (prolog-misc2 prolog) out))
  (display-xml/content (document-element doc) out #:indentation dent)
  (case dent
    [(none classic) (void)]
    [else (when (not (null? doc-misc))
            (newline out))])
  (display-outside-misc doc-misc out))

; display-dtd : document-type oport -> void
(define (display-dtd dtd out)
  (when dtd
    (fprintf out "<!DOCTYPE ~a" (document-type-name dtd))
    (let ([external (document-type-external dtd)])
      (cond
        [(external-dtd/public? external)
         (fprintf out " PUBLIC \"~a\" \"~a\""
                  (external-dtd/public-public external)
                  (external-dtd-system external))]
        [(external-dtd/system? external)
         (fprintf out " SYSTEM \"~a\"" (external-dtd-system external))]
        [(not external) (void)]))
    (display ">" out)
    (newline out)))

;; display-outside-misc : (listof Misc) Output-port -> Void
(define (display-outside-misc misc out)
  (for-each (lambda (x)
              ((cond
                 [(comment? x) write-xml-comment]
                 [(p-i? x) write-xml-p-i]) x 0 'none out)
              (newline out))
            misc))

;; write-xml-content : Content Nat Dent Output-Stream -> Void
(define (write-xml-content el over dent out)
  ((cond
     [(element? el) write-xml-element]
     [(pcdata? el) write-xml-pcdata]
     [(cdata? el) write-xml-cdata]
     [(entity? el) write-xml-entity]
     [(comment? el) write-xml-comment]
     [(p-i? el) write-xml-p-i]
     [else (error 'write-xml-content "received ~e" el)])
   el over dent out))

(define (whitespace-sensitive? x)
  (or (pcdata? x)
      (entity? x)))

;; write-xml-element : Element Nat Dent Output-Stream -> Void
(define (write-xml-element el over dent out)
  (let* ([name (element-name el)]
         [start (lambda (str dent)
                  (write-xml-base str over dent out)
                  (display name out))]
         [content (element-content el)])
    (start "<" dent)
    (for ([att (in-list (element-attributes el))])
      (fprintf out " ~a=\"~a\"" (attribute-name att)
               (escape (attribute-value att) escape-attribute-table)))
    (if (and (null? content)
             (let ([short (empty-tag-shorthand)])
               (case short
                 [(always) #t]
                 [(never) #f]
                 [else (memq (lowercase-symbol name) short)])))
        (display " />" out)
        (let ([dent-kids (case dent
                           [(peek)
                            (if (or (null? content) (whitespace-sensitive? (car content)))
                                'none
                                dent)]
                           [(scan)
                            (if (ormap whitespace-sensitive? content)
                                'none
                                dent)]
                           [else dent])])
          (display ">" out)
          (for ([c (in-list content)])
            (write-xml-content c (incr over) dent-kids out))
          (start "</" dent-kids)
          (display ">" out)))))

; : sym -> sym
(define lowercases (make-weak-hash))
(define (lowercase-symbol x)
  (or (hash-ref lowercases x #f)
      (let ([s (symbol->string x)])
        (let ([s (string->symbol (string-downcase s))])
          (hash-set! lowercases x s)
          s))))

;; write-xml-base : (U String Char Symbol) Nat Dent Output-Stream -> Void
(define (write-xml-base el over dent out)
  (case dent
    [(classic peek scan)
     (indent over out)]
    [(none)
     (void)]
    [else
     ; Either we have a bug or someone bypassed the contract
     (error "xml: unexpected indentation: " dent)])
  (display el out))

;; write-xml-pcdata : Pcdata Nat Dent Output-Stream -> Void
(define (write-xml-pcdata str over dent out)
  (write-xml-base (escape (pcdata-string str) escape-table) over dent out))

;; write-xml-cdata : Cdata Nat Dent Output-Stream -> Void
(define (write-xml-cdata cdata over dent out)
  ;; XXX: Different kind of quote is needed, for assume the user includes the <![CDATA[...]]> with proper quoting
  (write-xml-base (format "~a" (cdata-string cdata)) over dent out))

;; write-xml-p-i : Processing-instruction Nat Dent Output-Stream -> Void
(define (write-xml-p-i p-i over dent out)
  (write-xml-base (format "<?~a ~a?>" (p-i-target-name p-i) (p-i-instruction p-i)) over dent out))

;; write-xml-comment : Comment Nat Dent Output-Stream -> Void
(define (write-xml-comment comment over dent out)
  (write-xml-base (format "<!--~a-->" (comment-text comment)) over dent out))

;; write-xml-entity : Entity Nat Dent Output-stream -> Void
(define (write-xml-entity entity over dent out)
  (let ([n (entity-text entity)])
    (fprintf out (if (number? n) "&#~a;" "&~a;") n)))

(define escape-table #px"[<>&]")
(define escape-attribute-table #rx"[<>&\"]")

(define (replace-escaped s)
  (define c (string-ref s 0))
  (case c
    [(#\<) "&lt;"]
    [(#\>) "&gt;"]
    [(#\&) "&amp;"]
    [(#\") "&quot;"]
    [else c]))

;; escape : String Regexp -> String
(define (escape x table)
  (regexp-replace* table x replace-escaped))

;; write-string/excape: String Regexp Output-Port -> Void
;; Writes the string to the output port, with a fast-path 
;; that tries to avoid using string escapes unless necessary.
(define (write-string/escape str table out)
  (cond [(regexp-match table str)
         (write-string (escape str table) out)]
        [else
         (write-string str out)]))

(provide escape
         write-string/escape
         escape-table
         escape-attribute-table
         lowercase-symbol
         write-xml-cdata
         write-xml-comment
         write-xml-p-i
         write-xml-element)

;; incr : Nat -> Nat
(define (incr n) (+ n 2))
