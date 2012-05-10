#lang racket/base
(require racket/port
         racket/match
         racket/contract
         net/base64
         file/md5
         web-server/http/request-structs)

;; Requesting
(define (make-digest-auth-header realm private-key opaque)
  (define timestamp
    (number->string (current-seconds)))
  (define nonce
    (base64-encode 
     (string->bytes/utf-8
      (format "~a ~a"
              timestamp
              (md5 (string->bytes/utf-8 (string-append timestamp ":" private-key)))))))
  (make-header 
   #"WWW-Authenticate" 
   (string->bytes/utf-8
    (format "Digest realm=\"~a\", qop=\"auth\", nonce=\"~a\" opaque=\"~a\""
            realm nonce opaque))))

;; Receiving
(require parser-tools/lex
         parser-tools/yacc
         (prefix-in : parser-tools/lex-sre))

#|
       auth-param     = token "=" ( token | quoted-string )
       realm       = "realm" "=" realm-value
       realm-value = quoted-string

      challenge        =  "Digest" digest-challenge

      digest-challenge  = 1#( realm | [ domain ] | nonce |
                          [ opaque ] |[ stale ] | [ algorithm ] |
                          [ qop-options ] | [auth-param] )


      domain            = "domain" "=" <"> URI ( 1*SP URI ) <">
      URI               = absoluteURI | abs_path
      nonce             = "nonce" "=" nonce-value
      nonce-value       = quoted-string
      opaque            = "opaque" "=" quoted-string
      stale             = "stale" "=" ( "true" | "false" )
      algorithm         = "algorithm" "=" ( "MD5" | "MD5-sess" |
                           token )
      qop-options       = "qop" "=" <"> 1#qop-value <">
      qop-value         = "auth" | "auth-int" | token

       credentials      = "Digest" digest-response
       digest-response  = 1#( username | realm | nonce | digest-uri
                       | response | [ algorithm ] | [cnonce] |
                       [opaque] | [message-qop] |
                           [nonce-count]  | [auth-param] )

       username         = "username" "=" username-value
       username-value   = quoted-string
       digest-uri       = "uri" "=" digest-uri-value
       digest-uri-value = request-uri   ; As specified by HTTP/1.1
       message-qop      = "qop" "=" qop-value
       qop-value        = "auth" | "auth-int" | token
       cnonce           = "cnonce" "=" cnonce-value
       cnonce-value     = nonce-value
       nonce-count      = "nc" "=" nc-value
       nc-value         = 8LHEX
       response         = "response" "=" request-digest
       request-digest = <"> 32LHEX <">
       LHEX             =  "0" | "1" | "2" | "3" |
                           "4" | "5" | "6" | "7" |
                           "8" | "9" | "a" | "b" |
                           "c" | "d" | "e" | "f" 
|#
(define-lex-abbrevs
  (tspecial (:or (char-set "()<>@,;:\\\"/[]?={}") whitespace #\tab))
  (hex-char (char-set "0123456789abcdef"))
  (token-char (:- any-char tspecial iso-control)))

(define-tokens regular (TOKEN QUOTED-STRING 8LHEX 32LHEX))
(define-empty-tokens keywords (EQUALS COMMA DIGEST USERNAME REALM OPAQUE ALGORITHM MD5 MD5-SESS NONCE URI QOP AUTH AUTH-INT CNONCE NC RESPONSE EOF))

(define digest-lexer
  (lexer
   [(eof) (token-EOF)]
   [whitespace (digest-lexer input-port)]
   ["=" (token-EQUALS)]
   ["," (token-COMMA)]
   ["Digest" (token-DIGEST)]
   ["username" (token-USERNAME)]
   ["realm" (token-REALM)]
   ["nonce" (token-NONCE)]
   ["uri" (token-URI)]
   ["qop" (token-QOP)]
   ["auth" (token-AUTH)]
   ["opaque" (token-OPAQUE)]
   ["auth-int" (token-AUTH-INT)]
   ["cnonce" (token-CNONCE)]
   ["nc" (token-NC)]
   ["response" (token-RESPONSE)]
   [(repetition 8 8 hex-char) (token-8LHEX lexeme)]
   #;[(:: #\" (repetition 32 32 hex-char) #\") (token-32LHEX lexeme)]
   [(:: #\" (:* (:or (:~ #\") "\\\"")) #\")
    (token-QUOTED-STRING (substring lexeme 1 (- (string-length lexeme) 1)))]
   [(:+ token-char) (token-TOKEN lexeme)]))

(define digest-parser
  (parser (start credentials)
          (tokens regular keywords)
          (grammar (credentials [(DIGEST digest-response) $2])
                   (digest-response [(dr-part COMMA digest-response) (cons $1 $3)]
                                    [(dr-part) (list $1)])
                   (dr-part [(username) $1] [(realm) $1] [(nonce) $1]
                            [(digest-uri) $1] [(response) $1] [(algorithm) $1]
                            [(cnonce) $1] [(opaque) $1] [(message-qop) $1]
                            [(nonce-count) $1] [(auth-param) $1])
                   (auth-param [(TOKEN EQUALS auth-param-value) (cons (string->symbol $1) $3)])
                   (auth-param-value [(TOKEN) $1] [(QUOTED-STRING) $1])
                   (username [(USERNAME EQUALS QUOTED-STRING) (cons 'username $3)])
                   (realm [(REALM EQUALS QUOTED-STRING) (cons 'realm $3)])
                   (nonce [(NONCE EQUALS QUOTED-STRING) (cons 'nonce $3)])
                   (algorithm [(ALGORITHM EQUALS algorithm-value) (cons 'algorithm $3)])
                   (algorithm-value [(MD5) "md5"] [(MD5-SESS) "md5-sess"] [(TOKEN) $1])
                   (digest-uri [(URI EQUALS QUOTED-STRING) (cons 'uri $3)])
                   (opaque [(OPAQUE EQUALS QUOTED-STRING) (cons 'opaque $3)])
                   (message-qop [(QOP EQUALS qop-value) (cons 'qop $3)])
                   (qop-value [(AUTH) "auth"] [(AUTH-INT) "auth-int"] [(TOKEN) $1] [(QUOTED-STRING) $1])
                   (cnonce [(CNONCE EQUALS QUOTED-STRING) (cons 'cnonce $3)])
                   (nonce-count [(NC EQUALS 8LHEX) (cons 'nc $3)])
                   (response [(RESPONSE EQUALS QUOTED-STRING) (cons 'response $3)]))
          (end EOF)
          (error (lambda (a b c) (error 'digest-parser "Malformed digest: ~v ~v ~v" a b c)))))

(define (do-digest-parse str)
  (with-handlers ([exn:fail? (lambda _ #f)])
    (with-input-from-string 
     str 
     (lambda ()
       (digest-parser (λ () (digest-lexer (current-input-port))))))))

(define (request->digest-credentials req)
  (define headers (request-headers/raw req))
  (match (headers-assq* #"Authorization" headers)
    [#f #f]
    [(struct header (_ auth-bytes))
     (do-digest-parse (bytes->string/utf-8 auth-bytes))]))

(define username*realm->password/c
  (string? string? . -> . string?))
(define (password->digest-HA1 username*realm->password)
  (lambda (username realm)
    (define password
      (username*realm->password username realm))
    (define A1 
      (string->bytes/utf-8
       (format "~a:~a:~a" username realm password)))
    (define HA1 (md5 A1))
    HA1))

(define username*realm->digest-HA1/c
  (string? string? . -> . bytes?))
(define (make-check-digest-credentials username*realm->HA1)
  (lambda (method alist)
    (define (get-binding s l)
      (define c (assq s l))
      (if c (cdr c)
          (error 'make-check-digest-credentials "Missing digest field: ~a" s)))
    (define username (get-binding 'username alist))
    (define realm (get-binding 'realm alist))
    (define digest-uri (get-binding 'uri alist))
    (define nonce (get-binding 'nonce alist))
    (define nonce-count (get-binding 'nc alist))
    (define cnonce (get-binding 'cnonce alist))
    (define qop (get-binding 'qop alist))
    (define response (get-binding 'response alist))
    (define HA1 (username*realm->HA1 username realm))    
    (define A2
      (string->bytes/utf-8
       (format "~a:~a" method digest-uri)))
    (define HA2 (md5 A2))
    (define RESPONSE 
      (md5 
       (string->bytes/utf-8
        (format "~a:~a:~a:~a:~a:~a"
                HA1 nonce nonce-count cnonce qop HA2))))
    (bytes=? RESPONSE
             (string->bytes/utf-8 response))))

(provide/contract
 [make-digest-auth-header (string? string? string? . -> . header?)]
 [request->digest-credentials (request? . -> . (or/c false/c (listof (cons/c symbol? string?))))]
 [username*realm->password/c contract?]
 [username*realm->digest-HA1/c contract?]
 [password->digest-HA1 (username*realm->password/c . -> . username*realm->digest-HA1/c)]
 [make-check-digest-credentials (username*realm->digest-HA1/c . -> . (string? (listof (cons/c symbol? string?)) . -> . boolean?))])
