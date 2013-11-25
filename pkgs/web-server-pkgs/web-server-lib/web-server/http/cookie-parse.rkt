#lang racket/base
(require racket/port
         racket/match
         web-server/http/request-structs
         net/cookie
         web-server/private/util         
         racket/contract)

(define-struct client-cookie 
  (name value domain path)
  #:prefab)

(provide/contract
 [struct client-cookie 
         ([name string?]
          [value string?]
          [domain (or/c false/c valid-domain?)]
          [path (or/c false/c string?)])]
 [request-cookies (request? . -> . (listof client-cookie?))])

;; ============================================================
;; utilities for retrieving cookies

(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

#|
   cookie          =       "Cookie:" cookie-version
                           1*((";" | ",") cookie-value)
   cookie-value    =       NAME "=" VALUE [";" path] [";" domain]
   cookie-version  =       "$Version" "=" value
   NAME            =       attr
   VALUE           =       value
   path            =       "$Path" "=" value
   domain          =       "$Domain" "=" value

   value          = token | quoted-string

   token          = 1*<any CHAR except CTLs or tspecials>

   quoted-string  = ( <"> *(qdtext) <"> )
   qdtext         = <any TEXT except <">>
|#
(define-lex-abbrevs
  (illegal (char-set "()<>@:/[]?{}"))
  (tspecial (:or (char-set "()<>@,;\\\"/[]?={}") whitespace #\tab))
  (token-char (:- any-char tspecial iso-control)))

(define-tokens regular (TOKEN QUOTED-STRING ILLEGAL))
(define-empty-tokens keywords (EQUALS SEMI COMMA PATH DOMAIN VERSION EOF))

(define lex-cookie
  (lexer-src-pos
   [(eof) (token-EOF)]
   [whitespace (return-without-pos (lex-cookie input-port))]
   ["=" (token-EQUALS)]
   [";" (token-SEMI)]
   ["," (token-COMMA)]
   [(:+ illegal) (token-ILLEGAL lexeme)]
   ["$Path" (token-PATH)]
   ["$Domain" (token-DOMAIN)]
   ["$Version" (token-VERSION)]
   [(:: #\" (:* (:or (:~ #\") "\\\"")) #\")
    (token-QUOTED-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ token-char) (token-TOKEN lexeme)]))

(define current-source-name (make-parameter #f))

(define (make-srcloc start-pos end-pos)
  (list (current-source-name) 
        (position-line start-pos)
        (position-col start-pos)
        (position-offset start-pos)
        (- (position-offset end-pos) (position-offset start-pos))))

(define parse-raw-cookies
  (parser (src-pos)
          (start items)
          (tokens regular keywords)
          (grammar (items [(item separator items) (cons $1 $3)]
                          [(item) (list $1)])
                   (separator [(COMMA) #t]
                              [(SEMI) #t])
                   (item [(lhs EQUALS rhs) (cons $1 $3)]
                         ; This is not part of the spec. It is illegal
                         [(lhs EQUALS) (cons $1 "")])
                   (lhs [(VERSION) "$Version"]
                        [(DOMAIN) 'domain]
                        [(PATH) 'path]
                        [(TOKEN) $1])
                   (rhs [(TOKEN) $1] ; This is legal, but is subsumed by the illegal rule
                        [(QUOTED-STRING) (regexp-replace* (regexp-quote "\\\"") $1 "\"")]
                        ; This is not part of the spec. It is illegal
                        [(illegal) $1])
                   (illegal
                    [(EQUALS) "="]
                    [(ILLEGAL) $1]
                    [(illegal illegal) (string-append $1 $2)]
                    [(TOKEN) $1]))
          (suppress) ; The illegal rule creates many conflicts
          (end EOF)
          (error (lambda (tok-ok? tok-name tok-value start-pos end-pos)
                   (raise-syntax-error
                    'parse-cookies
                    (format 
                     (if tok-ok? 
                         "Did not expect token ~a"
                         "Invalid token ~a")
                     tok-name)
                    (datum->syntax #f tok-value (make-srcloc start-pos end-pos)))))))

(define (parse-cookie-likes ip)
  (parse-raw-cookies (λ () (lex-cookie ip))))

(define (parse-cookies str)
  (with-input-from-string 
      str
    (λ () 
      (define ip (current-input-port))
      (port-count-lines! ip)
      (parameterize ([current-source-name (object-name ip)])
        (raw->cookies (parse-cookie-likes ip))))))

;; raw->cookies : flat-property-list -> (listof cookie)
(define raw->cookies
  (match-lambda
    [(list-rest (cons (? string? key) val) l)
     (let loop ([l l] [c (make-client-cookie key val #f #f)])
       (match l
         [(list)
          (list c)]
         [(list-rest (cons (? string? key) val) l)
          (list* c (loop l (make-client-cookie key val #f #f)))]
         [(list-rest (cons 'domain val) l)
          (loop l (struct-copy client-cookie c [domain val]))]
         [(list-rest (cons 'path val) l)
          (loop l (struct-copy client-cookie c [path val]))]))]))

;; request-cookies* : request -> (listof cookie)
(define (request-cookies req)
  (define hdrs (request-headers/raw req))
  (apply append
         (map (compose parse-cookies bytes->string/utf-8 header-value)
              (filter (lambda (h)
                        (bytes-ci=? #"Cookie" (header-field h)))
                      hdrs))))
