;;;
;;; <mime.rkt> ---- MIME support
;;;
;;; Copyright (C) 2002 by PLT.
;;; Copyright (C) 2001 by Wish Computing.
;;;
;;; This file is part of mime

;;; mime is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; mime is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with mime; see the file COPYING.  If not, write to the Free
;;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;;; MA 02110-1301 USA.

;;; Author: Francisco Solsona <solsona@acm.org>
;;
;; Commentary: MIME support for PLT Scheme: an implementation of
;; rfc2045, rfc2046, rfc2047, rfc2048, and rfc2049.

#lang racket/base

(require racket/port "mime-util.rkt" "qp.rkt" "base64.rkt" "head.rkt")

(provide
 ;; -- exceptions raised --
 (struct-out mime-error)
 (struct-out unexpected-termination)
 (struct-out missing-multipart-boundary-parameter) ; this is the only one actually raised
 (struct-out malformed-multipart-entity)
 (struct-out empty-mechanism)
 (struct-out empty-type)
 (struct-out empty-subtype)
 (struct-out empty-disposition-type)

 ;; -- basic mime structures --
 (struct-out message)
 (struct-out entity)
 (struct-out disposition)

 ;; -- mime methods --
 mime-analyze)

;; Constants:
(define discrete-alist
  '(("text" . text)
    ("image" . image)
    ("audio" . audio)
    ("video" . video)
    ("application" . application)))

(define disposition-alist
  '(("inline" . inline)
    ("attachment" . attachment)
    ("file" . attachment) ;; This is used (don't know why) by
                          ;; multipart/form-data
    ("messagetext" . inline)
    ("form-data" . form-data)))

(define composite-alist
  '(("message" . message)
    ("multipart" . multipart)))

(define mechanism-alist
  '(("7bit" . 7bit)
    ("8bit" . 8bit)
    ("binary" . binary)
    ("quoted-printable" . quoted-printable)
    ("base64" . base64)))

(define ietf-extensions '())

;; We don't try to keep up with IANA substypes
#;
(define iana-extensions
  '(;; text
    ("plain" . plain)
    ("html" . html)
    ("enriched"  . enriched) ; added 5/2005 - probably not iana
    ("richtext"  . richtext)
    ("tab-separated-values" . tab-separated-values)
    ;; Multipart
    ("mixed" . mixed)
    ("alternative" . alternative)
    ("digest" . digest)
    ("parallel" . parallel)
    ("appledouble" . appledouble)
    ("header-set" . header-set)
    ("form-data" . form-data)
    ;; Message
    ("rfc822" . rfc822)
    ("partial" . partial)
    ("external-body" . external-body)
    ("news" . news)
    ;; Application
    ("octet-stream" . octet-stream)
    ("postscript" . postscript)
    ("oda" . oda)
    ("atomicmail" . atomicmail)
    ("andrew-inset" . andrew-inset)
    ("slate" . slate)
    ("wita" . wita)
    ("dec-dx" . dec-dx)
    ("dca-rf" . dca-rf)
    ("activemessage" . activemessage)
    ("rtf" . rtf)
    ("applefile" . applefile)
    ("mac-binhex40" . mac-binhex40)
    ("news-message-id" . news-message-id)
    ("news-transmissio" . news-transmissio)
    ("wordperfect5.1" . wordperfect5.1)
    ("pdf" . pdf)
    ("zip" . zip)
    ("macwritei" . macwritei)
    ;; "image"
    ("jpeg" . jpeg)
    ("gif" . gif)
    ("ief" . ief)
    ("tiff" . tiff)
    ;; "audio"
    ("basic" . basic)
    ;; "video" .
    ("mpeg" . mpeg)
    ("quicktime" . quicktime)))

;; Basic structures
(define-struct message (version entity fields)
  #:mutable)
(define-struct entity
  (type subtype charset encoding disposition params id description other
   fields parts body)
  #:mutable)
(define-struct disposition
  (type filename creation modification read size params)
  #:mutable)

;; Exceptions
(define-struct (mime-error exn:fail) ())
(define-struct (unexpected-termination mime-error) (msg))
(define-struct (missing-multipart-boundary-parameter mime-error) ())

(define-struct (malformed-multipart-entity mime-error) (msg))
(define-struct (empty-mechanism mime-error) ())
(define-struct (empty-type mime-error) ())
(define-struct (empty-subtype mime-error) ())
(define-struct (empty-disposition-type mime-error) ())

;; *************************************
;; Practical stuff, aka MIME in action:
;; *************************************
(define CRLF (format "~a~a" #\return #\newline))
(define CRLF-binary "=0D=0A") ;; quoted printable representation

;; get-headers : input-port -> string
;; returns the header part of a message/part conforming to rfc822, and
;; rfc2045.
(define (get-headers in)
  (let loop ([headers ""] [ln (read-line in 'any)])
    (cond [(eof-object? ln)
           ;; (raise (make-unexpected-termination "eof reached! while parsing headers"))
           (warning "premature eof while parsing headers")
           headers]
          [(string=? ln "") headers]
          [else
           ;; Quoting rfc822:
           ;; " Headers occur before the message body and are
           ;; terminated by a null line (i.e., two contiguous
           ;; CRLFs)."
           ;; That is: Two empty lines.  But most MUAs seem to count
           ;; the CRLF ending the last field (header) as the first
           ;; CRLF of the null line.
           (loop (string-append headers ln CRLF)
                 (read-line in 'any))])))

(define (make-default-disposition)
  (make-disposition
   'inline ;; type
   "" ;; filename
   #f ;; creation
   #f ;; modification
   #f ;; read
   #f ;; size
   null ;; params
   ))

(define (make-default-entity)
  (make-entity
   'text  ;; type
   'plain ;; subtype
   'us-ascii ;; charset
   '7bit  ;; encoding
   (make-default-disposition) ;; disposition
   null   ;; params
   ""     ;; id
   ""     ;; description
   null   ;; other MIME fields (MIME-extension-fields)
   null   ;; fields
   null   ;; parts
   null   ;; body
   ))

(define (make-default-message)
  (make-message 1.0 (make-default-entity) null))

(define (mime-decode entity input)
  (set-entity-body!
   entity
   (case (entity-encoding entity)
     [(quoted-printable)
      (lambda (output)
        (qp-decode-stream input output))]
     [(base64)
      (lambda (output)
        (base64-decode-stream input output))]
     [else ;; 7bit, 8bit, binary
      (lambda (output)
        (copy-port input output))])))

(define (mime-analyze input [part #f])
  (let* ([iport (if (bytes? input)
                  (open-input-bytes input)
                  input)]
         [headers (get-headers iport)]
         [msg (if part
                (MIME-part-headers headers)
                (MIME-message-headers headers))]
         [entity (message-entity msg)])
    ;; OK we have in msg a MIME-message structure, lets see what we have:
    (case (entity-type entity)
      [(text image audio video application)
       ;; decode part, and save port and thunk
       (mime-decode entity iport)]
      [(message multipart)
       (let ([boundary (entity-boundary entity)])
         (when (not boundary)
           (when (eq? 'multipart (entity-type entity))
             (raise (make-missing-multipart-boundary-parameter
                     "missing multipart \"boundary\" parameter"
                     (current-continuation-marks)))))
         (set-entity-parts! entity
                            (map (lambda (part)
                                   (mime-analyze part #t))
                                 (if boundary
                                   (multipart-body iport boundary)
                                   (list iport)))))]
      [else
       ;; Unrecognized type, you're on your own! (sorry)
       (mime-decode entity iport)])
    ;; return mime structure
    msg))

(define (entity-boundary entity)
  (let* ([params (entity-params entity)]
         [ans (assoc "boundary" params)])
    (and ans (cdr ans))))

;; *************************************************
;; MIME Specific: rfc2045-2049, and rfc0822, rfc2183
;; *************************************************

;;multipart-body := [preamble CRLF]
;;                  dash-boundary transport-padding CRLF
;;                  body-part *encapsulation
;;                  close-delimiter transport-padding
;;                  [CRLF epilogue]
;; Returns a list of input ports, each one containing the correspongind part.
(define (multipart-body input boundary)
  (let* ([make-re (lambda (prefix)
                    (regexp (string-append prefix "--" (regexp-quote boundary) "(--)?\r\n")))]
         [re (make-re "\r\n")])
    (letrec ([eat-part (lambda ()
                         (let-values ([(pin pout) (make-pipe)])
                           (let ([m (regexp-match re input 0 #f pout)])
                             (cond
                               [(not m)
                                (close-output-port pout)
                                (values pin ;; part
                                        #f  ;; close-delimiter?
                                        #t  ;; eof reached?
                                        )]
                               [(cadr m)
                                (close-output-port pout)
                                (values pin #t #f)]
                               [else
                                (close-output-port pout)
                                (values pin #f #f)]))))])
      ;; pre-amble is allowed to be completely empty:
      (if (regexp-match-peek (make-re "^") input)
        ;; No \r\f before first separator:
        (read-line input)
        ;; non-empty preamble:
        (eat-part))
      (let loop ()
        (let-values ([(part close? eof?) (eat-part)])
          (cond [close? (list part)]
                [eof? (list part)]
                [else (cons part (loop))]))))))

;; MIME-message-headers := entity-headers
;;                        fields
;;                        version CRLF
;;                        ; The ordering of the header
;;                        ; fields implied by this BNF
;;                        ; definition should be ignored.
(define (MIME-message-headers headers)
  (let ([message (make-default-message)])
    (entity-headers headers message #t)
    message))

;; MIME-part-headers := entity-headers
;;                     [ fields ]
;;                     ; Any field not beginning with
;;                     ; "content-" can have no defined
;;                     ; meaning and may be ignored.
;;                     ; The ordering of the header
;;                     ; fields implied by this BNF
;;                     ; definition should be ignored.
(define (MIME-part-headers headers)
  (let ([message (make-default-message)])
    (entity-headers headers message #f)
    message))

;; entity-headers := [ content CRLF ]
;;                  [ encoding CRLF ]
;;                  [ id CRLF ]
;;                  [ description CRLF ]
;;                  *( MIME-extension-field CRLF )
(define (entity-headers headers message version?)
  (let ([entity (message-entity message)])
    (let-values ([(mime non-mime) (get-fields headers)])
      (let loop ([fields mime])
        (unless (null? fields)
          ;; Process MIME field
          (let ([trimmed-h (trim-comments (car fields))])
            (or (and version? (version trimmed-h message))
                (content trimmed-h entity)
                (encoding trimmed-h entity)
                (dispositione trimmed-h entity)
                (id trimmed-h entity)
                (description trimmed-h entity)
                (MIME-extension-field trimmed-h entity))
            ;; keep going
            (loop (cdr fields)))))
      ;; NON-mime headers (or semantically incorrect). In order to make
      ;; this implementation of rfc2045 robuts, we will save the header in
      ;; the fields field of the message struct:
      (set-message-fields! message non-mime)
      ;; Return message
      message)))

(define (get-fields headers)
  (let ([mime null] [non-mime null])
    (letrec ([store-field
              (lambda (f)
                (unless (string=? f "")
                  (if (mime-header? f)
                    (set! mime (append mime (list (trim-spaces f))))
                    (set! non-mime (append non-mime (list (trim-spaces f)))))))])
      (let ([fields (extract-all-fields headers)])
        (for-each (lambda (p)
                    (store-field (format "~a: ~a" (car p) (cdr p))))
                  fields))
      (values mime non-mime))))

(define re:content #rx"^(?i:content-)")
(define re:mime #rx"^(?i:mime-version):")

(define (mime-header? h)
  (or (regexp-match? re:content h)
      (regexp-match? re:mime h)))

;;; Headers
;;; Content-type follows this BNF syntax:
;; content := "Content-Type" ":" type "/" subtype
;;           *(";" parameter)
;;           ; Matching of media type and subtype
;;           ; is ALWAYS case-insensitive.
(define re:content-type #rx"^(?i:content-type):([^/]+)/([^/]+)$")
(define (content header entity)
  (let* ([params (string-tokenizer #\; header)]
         [one re:content-type]
         [h (trim-all-spaces (car params))]
         [target (regexp-match one h)]
         [old-param (entity-params entity)])
    (and target
         (set-entity-type! entity
                           (type (regexp-replace one h "\\1")))   ;; type
         (set-entity-subtype! entity
                              (subtype (regexp-replace one h "\\2"))) ;; subtype
         (set-entity-params!
          entity
          (append old-param
                  (let loop ([p (cdr params)] ;; parameters
                             [ans null])
                    (cond [(null? p) ans]
                          [else
                           (let ([par-pair (parameter (trim-all-spaces (car p)))])
                             (cond [par-pair
                                    (when (string=? (car par-pair) "charset")
                                      (set-entity-charset! entity (cdr par-pair)))
                                    (loop (cdr p) (append ans (list par-pair)))]
                                   [else
                                    (warning "Invalid parameter for Content-Type: `~a'" (car p))
                                    ;; go on...
                                    (loop (cdr p) ans)]))])))))))

;; From rfc2183 Content-Disposition
;; disposition := "Content-Disposition" ":"
;;                  disposition-type
;;                  *(";" disposition-parm)
(define re:content-disposition #rx"^(?i:content-disposition):(.+)$")
(define (dispositione header entity)
  (let* ([params (string-tokenizer #\; header)]
         [reg re:content-disposition]
         [h (trim-all-spaces (car params))]
         [target (regexp-match reg h)]
         [disp-struct (entity-disposition entity)])
    (and target
         (set-disposition-type!
          disp-struct
          (disp-type (regexp-replace reg h "\\1")))
         (disp-params (cdr params) disp-struct))))

;; version := "MIME-Version" ":" 1*DIGIT "." 1*DIGIT
(define re:mime-version #rx"^(?i:MIME-Version):([0-9]+)\\.([0-9]+)$")
(define (version header message)
  (let* ([reg re:mime-version]
         [h (trim-all-spaces header)]
         [target (regexp-match reg h)])
    (and target
         (set-message-version!
          message
          (string->number (regexp-replace reg h "\\1.\\2"))))))

;;   description := "Content-Description" ":" *text
(define re:content-description #rx"^(?i:content-description):[ \t\r\n]*(.*)$")
(define (description header entity)
  (let* ([reg re:content-description]
         [target (regexp-match reg header)])
    (and target
         (set-entity-description!
          entity
          (trim-spaces (regexp-replace reg header "\\1"))))))

;;   encoding := "Content-Transfer-Encoding" ":" mechanism
(define re:content-transfer-encoding #rx"^(?i:content-transfer-encoding):(.+)$")
(define (encoding header entity)
  (let* ([reg re:content-transfer-encoding]
         [h (trim-all-spaces header)]
         [target (regexp-match reg h)])
    (and target
         (set-entity-encoding!
          entity
          (mechanism (regexp-replace reg h "\\1"))))))

;;   id := "Content-ID" ":" msg-id
(define re:content-id #rx"^(?i:content-id):(.+)$")
(define (id header entity)
  (let* ([reg re:content-id]
         [h (trim-all-spaces header)]
         [target (regexp-match reg h)])
    (and target
         (set-entity-id!
          entity
          (msg-id (regexp-replace reg h "\\1"))))))

;; From rfc822:
;; msg-id      =  "<" addr-spec ">"            ; Unique message id
;; addr-spec   =  local-part "@" domain        ; global address
;; local-part  =  word *("." word)             ; uninterpreted
;;                                             ; case-preserved
;; domain      =  sub-domain *("." sub-domain)
;; sub-domain  =  domain-ref / domain-literal
;; domain-literal =  "[" *(dtext / quoted-pair) "]"
;; domain-ref  =  atom                         ; symbolic reference
(define (msg-id str)
  (let* ([r (regexp "^<[^@>]+@[^.]+(\\.[^.]+)*>$")]
         [ans (regexp-match r str)])
    (if ans
      str
      (begin (warning "Invalid msg-id: ~a" str) str))))

;;  mechanism := "7bit" / "8bit" / "binary" /
;;                 "quoted-printable" / "base64" /
;;                 ietf-token / x-token
(define (mechanism mech)
  (if (not mech)
    (raise (make-empty-mechanism))
    (let ([val (assoc (lowercase mech) mechanism-alist)])
      (or (and val (cdr val))
          (ietf-token mech)
          (x-token mech)))))

;;  MIME-extension-field := <Any RFC 822 header field which
;;                              begins with the string
;;                              "Content-">
;;
(define (MIME-extension-field header entity)
  (let* ([reg (regexp "^[Cc]ontent-(.+):[ \t]*(.+)$")]
         [target (regexp-match reg header)])
    (and target
         (set-entity-other!
          entity
          (append (entity-other entity)
                  (list (cons (regexp-replace reg header "\\1")
                              (trim-spaces (regexp-replace reg header "\\2")))))))))

;; type := discrete-type / composite-type
(define (type value)
  (if (not value)
    (raise (make-empty-type))
    (or (discrete-type value)
        (composite-type value))))

;; disposition-type := "inline" / "attachment"  / extension-token
(define (disp-type value)
  (if (not value)
    (raise (make-empty-disposition-type))
    (let ([val (assoc (lowercase (trim-spaces value)) disposition-alist)])
      (if val (cdr val) (extension-token value)))))

;; discrete-type := "text" / "image" / "audio" / "video" /
;;                  "application" / extension-token
(define (discrete-type value)
  (let ([val (assoc (lowercase (trim-spaces value)) discrete-alist)])
    (if val (cdr val) (extension-token value))))

;; composite-type := "message" / "multipart" / extension-token
(define (composite-type value)
  (let ([val (assoc (lowercase (trim-spaces value)) composite-alist)])
    (if val (cdr val) (extension-token value))))

;; extension-token := ietf-token / x-token
(define (extension-token value)
  (or (ietf-token value)
      (x-token value)))

;; ietf-token := <An extension token defined by a
;;               standards-track RFC and registered
;;               with IANA.>
(define (ietf-token value)
  (let ([ans (assoc (lowercase (trim-spaces value)) ietf-extensions)])
    (and ans (cdr ans))))

;;  Directly from RFC 1700:
;; Type            Subtype         Description                 Reference
;; ----            -------         -----------                 ---------
;; text            plain                                   [RFC1521,NSB]
;;                 richtext                                [RFC1521,NSB]
;;                 tab-separated-values                   [Paul Lindner]
;;
;; multipart       mixed                                   [RFC1521,NSB]
;;                 alternative                             [RFC1521,NSB]
;;                 digest                                  [RFC1521,NSB]
;;                 parallel                                [RFC1521,NSB]
;;                 appledouble                [MacMime,Patrik Faltstrom]
;;                 header-set                             [Dave Crocker]
;;
;; message         rfc822                                  [RFC1521,NSB]
;;                 partial                                 [RFC1521,NSB]
;;                 external-body                           [RFC1521,NSB]
;;                 news                        [RFC 1036, Henry Spencer]
;;
;; application     octet-stream                            [RFC1521,NSB]
;;                   postscript                              [RFC1521,NSB]
;;                   oda                                     [RFC1521,NSB]
;;                   atomicmail                           [atomicmail,NSB]
;;                   andrew-inset                       [andrew-inset,NSB]
;;                   slate                           [slate,terry crowley]
;;                   wita              [Wang Info Transfer,Larry Campbell]
;;                   dec-dx            [Digital Doc Trans, Larry Campbell]
;;                   dca-rft        [IBM Doc Content Arch, Larry Campbell]
;;                   activemessage                          [Ehud Shapiro]
;;                   rtf                                    [Paul Lindner]
;;                   applefile                  [MacMime,Patrik Faltstrom]
;;                   mac-binhex40               [MacMime,Patrik Faltstrom]
;;                   news-message-id              [RFC1036, Henry Spencer]
;;                   news-transmission            [RFC1036, Henry Spencer]
;;                   wordperfect5.1                         [Paul Lindner]
;;                   pdf                                    [Paul Lindner]
;;                   zip                                    [Paul Lindner]
;;                   macwriteii                             [Paul Lindner]
;;                   msword                                 [Paul Lindner]
;;                   remote-printing                         [RFC1486,MTR]
;;
;; image           jpeg                                    [RFC1521,NSB]
;;                 gif                                     [RFC1521,NSB]
;;                 ief             Image Exchange Format       [RFC1314]
;;                 tiff            Tag Image File Format           [MTR]
;;
;; audio           basic                                   [RFC1521,NSB]
;;
;; video           mpeg                                    [RFC1521,NSB]
;;                 quicktime                              [Paul Lindner]

;; x-token := <The two characters "X-" or "x-" followed, with
;;            no intervening white space, by any token>
(define (x-token value)
  (let* ([r  #rx"^[xX]-(.*)"]
         [h (trim-spaces value)]
         [ans (regexp-match r h)])
    (and ans
         (token (regexp-replace r h "\\1"))
         h)))

;; subtype := extension-token / iana-token
(define (subtype value)
  (if (not value)
    (raise (make-empty-subtype))
    (or (extension-token value)
        (iana-token value))))

;; iana-token := <A publicly-defined extension token. Tokens
;;               of this form must be registered with IANA
;;               as specified in RFC 2048.>
;;              Instead of trying to list all registered types
;;              here, we just convert to a symbol.
(define (iana-token value)
  (string->symbol (lowercase (trim-spaces value))))

;; parameter := attribute "=" value
(define re:parameter (regexp "([^=]+)=(.+)"))
(define (parameter par)
  (let* ([r re:parameter]
         [att (attribute (regexp-replace r par "\\1"))]
         [val (value (regexp-replace r par "\\2"))])
    (if (regexp-match r par)
      (cons (if att (lowercase att) "???") val)
      (cons "???" par))))

;; value := token / quoted-string
(define (value val)
  (or (token val)
      (quoted-string val)
      val))

;; token := 1*<any (US-ASCII) CHAR except SPACE, CTLs,
;;            or tspecials>
;; tspecials :=  "(" / ")" / "<" / ">" / "@" /
;;              "," / ";" / ":" / "\" / <">
;;              "/" / "[" / "]" / "?" / "="
;;              ; Must be in quoted-string,
;;              ; to use within parameter values
(define (token value)
  (let* ([tspecials (regexp "[^][()<>@,;:\\\"/?= ]+")]
         [ans (regexp-match tspecials value)])
    (and ans
         (string=? value (car ans))
         (car ans))))

;; attribute := token
;;             ; Matching of attributes
;;             ; is ALWAYS case-insensitive.
(define attribute token)

(define re:quotes (regexp "\"(.+)\""))
(define (quoted-string str)
  (let* ([quotes re:quotes]
         [ans (regexp-match quotes str)])
    (and ans (regexp-replace quotes str "\\1"))))

;; disposition-parm := filename-parm
;;                     / creation-date-parm
;;                     / modification-date-parm
;;                     / read-date-parm
;;                     / size-parm
;;                     / parameter
;;
;; filename-parm := "filename" "=" value
;;
;; creation-date-parm := "creation-date" "=" quoted-date-time
;;
;; modification-date-parm := "modification-date" "=" quoted-date-time
;;
;; read-date-parm := "read-date" "=" quoted-date-time
;;
;; size-parm := "size" "=" 1*DIGIT
(define (disp-params lst disp)
  (let loop ([lst lst])
    (unless (null? lst)
      (let* ([p (parameter (trim-all-spaces (car lst)))]
             [parm (car p)]
             [value (cdr p)])
        (cond [(string=? parm "filename")
               (set-disposition-filename! disp value)]
              [(string=? parm "creation-date")
               (set-disposition-creation!
                disp
                (disp-quoted-data-time value))]
              [(string=? parm "modification-date")
               (set-disposition-modification!
                disp
                (disp-quoted-data-time value))]
              [(string=? parm "read-date")
               (set-disposition-read!
                disp
                (disp-quoted-data-time value))]
              [(string=? parm "size")
               (set-disposition-size!
                disp
                (string->number value))]
              [else
               (set-disposition-params!
                disp
                (append (disposition-params disp) (list p)))])
        (loop (cdr lst))))))

;; date-time   =  [ day "," ] date time        ; dd mm yy
;;                                             ;  hh:mm:ss zzz
;;
;; day         =  "Mon"  / "Tue" /  "Wed"  / "Thu"
;;            /  "Fri"  / "Sat" /  "Sun"
;;
;; date        =  1*2DIGIT month 2DIGIT        ; day month year
;;                                   ;  e.g. 20 Jun 82
;;
;; month       =  "Jan"  /  "Feb" /  "Mar"  /  "Apr"
;;            /  "May"  /  "Jun" /  "Jul"  /  "Aug"
;;            /  "Sep"  /  "Oct" /  "Nov"  /  "Dec"
;;
;; time        =  hour zone                    ; ANSI and Military
;;
;; hour        =  2DIGIT ":" 2DIGIT [":" 2DIGIT]
;;                                   ; 00:00:00 - 23:59:59
;;
;; zone        =  "UT"  / "GMT"                ; Universal Time
;;                                   ; North American : UT
;;            /  "EST" / "EDT"                ;  Eastern:  - 5/ - 4
;;            /  "CST" / "CDT"                ;  Central:  - 6/ - 5
;;            /  "MST" / "MDT"                ;  Mountain: - 7/ - 6
;;            /  "PST" / "PDT"                ;  Pacific:  - 8/ - 7
;;            /  1ALPHA                       ; Military: Z = UT;
;;                                   ;  A:-1; (J not used)
;;                                   ;  M:-12; N:+1; Y:+12
;;            / ( ("+" / "-") 4DIGIT )        ; Local differential
;;                                   ;  hours+min. (HHMM)
(define date-time
  (lambda (str)
    ;; Fix Me: I have to return a date structure, or time in seconds.
    str))

;; quoted-date-time := quoted-string
;;                ; contents MUST be an RFC 822 `date-time'
;;                ; numeric timezones (+HHMM or -HHMM) MUST be used

(define disp-quoted-data-time date-time)
