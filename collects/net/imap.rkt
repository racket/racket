#lang racket/base

(require racket/contract/base 
         racket/tcp 
         openssl 
         "private/rbtree.rkt")

;; define the imap struct and its predicate here, for use in the contract, below
(define-struct imap (r w exists recent unseen uidnext uidvalidity
                     expunges fetches new?)
  #:mutable)
(define (imap-connection? v) (imap? v))

(provide/contract
 [imap-get-hierarchy-delimiter (imap-connection? . -> . bytes?)]
 [imap-list-child-mailboxes
  (case->
   (imap-connection? (or/c false/c bytes?)
                     . -> . (listof (list/c (listof symbol?) bytes?)))
   (imap-connection? (or/c false/c bytes?) (or/c false/c bytes?)
                     . -> .
                     (listof (list/c (listof symbol?) bytes?))))])

(provide
 imap-connection?
 imap-connect imap-connect*
 imap-disconnect
 imap-force-disconnect
 imap-reselect
 imap-examine
 imap-noop
 imap-poll
 imap-status

 imap-port-number ; a parameter

 imap-new?
 imap-messages
 imap-recent
 imap-uidnext
 imap-uidvalidity
 imap-unseen
 imap-reset-new!

 imap-get-expunges
 imap-pending-expunges?
 imap-get-updates
 imap-pending-updates?

 imap-get-messages
 imap-copy imap-append
 imap-store imap-flag->symbol symbol->imap-flag
 imap-expunge

 imap-mailbox-exists?
 imap-create-mailbox

 imap-mailbox-flags)

(define debug-via-stdio? #f)

(define eol (if debug-via-stdio? 'linefeed 'return-linefeed))

(define (tag-eq? a b)
  (or (eq? a b)
      (and (symbol? a)
           (symbol? b)
           (string-ci=? (symbol->string a) (symbol->string b)))))

(define field-names
  (list (list 'uid (string->symbol "UID"))
        (list 'header (string->symbol "RFC822.HEADER"))
        (list 'body (string->symbol "RFC822.TEXT"))
        (list 'size (string->symbol "RFC822.SIZE"))
        (list 'flags (string->symbol "FLAGS"))))

(define flag-names
  (list (list 'seen (string->symbol "\\Seen"))
        (list 'answered (string->symbol "\\Answered"))
        (list 'flagged (string->symbol "\\Flagged"))
        (list 'deleted (string->symbol "\\Deleted"))
        (list 'draft (string->symbol "\\Draft"))
        (list 'recent (string->symbol "\\Recent"))

        (list 'noinferiors (string->symbol "\\Noinferiors"))
        (list 'noselect (string->symbol "\\Noselect"))
        (list 'marked (string->symbol "\\Marked"))
        (list 'unmarked (string->symbol "\\Unmarked"))

        (list 'hasnochildren (string->symbol "\\HasNoChildren"))
        (list 'haschildren (string->symbol "\\HasChildren"))))

(define (imap-flag->symbol f)
  (or (ormap (lambda (a) (and (tag-eq? f (cadr a)) (car a))) flag-names)
      f))

(define (symbol->imap-flag s)
  (cond [(assoc s flag-names) => cadr] [else s]))

(define-logger imap)

(define make-msg-id
  (let ([id 0])
    (lambda ()
      (begin0 (string->bytes/latin-1 (format "a~a " id))
        (set! id (add1 id))))))

(define (starts-with? l n)
  (and (>= (bytes-length l) (bytes-length n))
       (bytes=? n (subbytes l 0 (bytes-length n)))))

(define (skip s n)
  (subbytes s (if (number? n) n (bytes-length n))))

(define (splice l sep)
  (if (null? l)
    ""
    (format "~a~a"
            (car l)
            (apply string-append
                   (map (lambda (n) (format "~a~a" sep n)) (cdr l))))))

(define (imap-read s r)
  (let loop ([s s]
             [r r]
             [accum null]
             [eol-k (lambda (accum) (reverse accum))]
             [eop-k (lambda (s accum) (error 'imap-read "unxpected close parenthesis"))])
    (cond
      [(bytes=? #"" s)
       (eol-k accum)]
      [(char-whitespace? (integer->char (bytes-ref s 0)))
       (loop (skip s 1) r accum eol-k eop-k)]
      [else
       (case (integer->char (bytes-ref s 0))
         [(#\")
          (let ([m (regexp-match #rx#"\"([^\"]*)\"(.*)" s)])
            (if m
              (loop (caddr m) r (cons (cadr m) accum) eol-k eop-k)
              (error 'imap-read "didn't find end of quoted string in: ~a" s)))]
         [(#\))
          (eop-k (skip s 1) accum)]
         [(#\() (letrec ([next-line
                          (lambda (accum)
                            (loop (read-bytes-line r eol) r
                                  accum
                                  next-line
                                  finish-parens))]
                         [finish-parens
                          (lambda (s laccum)
                            (loop s r
                                  (cons (reverse laccum) accum)
                                  eol-k eop-k))])
                  (loop (skip s 1) r null next-line finish-parens))]
         [(#\{) (let ([m (regexp-match #rx#"{([0-9]+)}(.*)" s)])
                  (cond
                    [(not m) (error 'imap-read "couldn't read {} number: ~a" s)]
                    [(not (bytes=? (caddr m) #"")) (error 'imap-read "{} not at end-of-line: ~a" s)]
                    [else
                     (loop #"" r
                           (cons (read-bytes (string->number
                                              (bytes->string/latin-1 (cadr m)))
                                             r)
                                 accum)
                           eol-k eop-k)]))]
         [else (let ([m (regexp-match #rx#"([^ (){}]+)(.*)" s)])
                 (if m
                   (loop (caddr m) r
                         (cons (let ([v (cadr m)])
                                 (if (regexp-match #rx#"^[0-9]*$" v)
                                   (string->number (bytes->string/latin-1 v))
                                   (string->symbol (bytes->string/latin-1 v))))
                               accum)
                         eol-k eop-k)
                   (error 'imap-read "failure reading atom: ~a" s)))])])))

(define (get-response r id info-handler continuation-handler)
  (let loop ()
    (let ([l (read-bytes-line r eol)])
      (log-imap-debug "raw-reply: ~s" l)
      (cond [(eof-object? l)
             (error 'imap-send "unexpected end-of-file from server")]
            [(and id (starts-with? l id))
             (let ([reply (imap-read (skip l id) r)])
               (log-imap-debug "response: ~a" reply)
               reply)]
            [(starts-with? l #"* ")
             (let ([info (imap-read (skip l 2) r)])
               (log-imap-debug "info: ~s" info)
               (info-handler info))
             (when id (loop))]
            [(starts-with? l #"+ ")
             (if (null? continuation-handler)
               (error 'imap-send "unexpected continuation request: ~a" l)
               ((car continuation-handler) loop (imap-read (skip l 2) r)))]
            [else
             (log-imap-warning "warning: unexpected response for ~a: ~a" id l)
             (when id (loop))]))))

;; A cmd is
;;  * (box v) - send v literally via ~a
;;  * string or bytes - protect as necessary
;;  * (cons cmd null) - same as cmd
;;  * (cons cmd cmd) - send cmd, space, cmd

(define (imap-send imap cmd info-handler . continuation-handler)
  (let ([r (imap-r imap)]
        [w (imap-w imap)]
        [id (make-msg-id)])
    (log-imap-debug "sending ~a~a" id cmd)
    (fprintf w "~a" id)
    (let loop ([cmd cmd])
      (cond
        [(box? cmd) (fprintf w "~a" (unbox cmd))]
        [(string? cmd) (loop (string->bytes/utf-8 cmd))]
        [(bytes? cmd)
         (if (or (regexp-match #rx#"[ *\"\r\n]" cmd)
                 (equal? cmd #""))
           (if (regexp-match #rx#"[\"\r\n]" cmd)
             (begin
               ;; Have to send size, then continue if the
               ;;  server consents
               (fprintf w "{~a}\r\n" (bytes-length cmd))
               (flush-output w)
               (get-response r #f void (list (lambda (gloop data) (void))))
               ;; Continue by writing the data
               (write-bytes cmd w))
             (fprintf w "\"~a\"" cmd))
           (fprintf w "~a" cmd))]
        [(and (pair? cmd) (null? (cdr cmd))) (loop (car cmd))]
        [(pair? cmd) (begin (loop (car cmd))
                            (fprintf w " ")
                            (loop (cdr cmd)))]))
    (fprintf w "\r\n")
    (flush-output w)
    (get-response r id (wrap-info-handler imap info-handler)
                  continuation-handler)))

(define (check-ok reply)
  (unless (and (pair? reply) (tag-eq? (car reply) 'OK))
    (error 'check-ok "server error: ~s" reply)))

(define (ok-tag-eq? i t)
  (and (tag-eq? (car i) 'OK)
       ((length i) . >= . 3)
       (tag-eq? (cadr i) (string->symbol (format "[~a" t)))))

(define (ok-tag-val i)
  (let ([v (caddr i)])
    (and (symbol? v)
         (let ([v (symbol->string v)])
           (regexp-match #rx"[]]$" v)
           (string->number (substring v 0 (sub1 (string-length v))))))))

(define (wrap-info-handler imap info-handler)
  (lambda (i)
    (when (and (list? i) ((length i) . >= . 2))
      (cond
        [(tag-eq? (cadr i) 'EXISTS)
         (when (> (car i) (or (imap-exists imap) 0))
           (set-imap-new?! imap #t))
         (set-imap-exists! imap (car i))]
        [(tag-eq? (cadr i) 'RECENT)
         (set-imap-recent! imap (car i))]
        [(tag-eq? (cadr i) 'EXPUNGE)
         (let ([n (car i)])
           (log-imap-debug "Recording expunge: ~s" n)
           ;; add it to the tree of expunges
           (expunge-insert! (imap-expunges imap) n)
           ;; decrement exists count:
           (set-imap-exists! imap (sub1 (imap-exists imap)))
           ;; adjust ids for any remembered fetches:
           (fetch-shift! (imap-fetches imap) n))]
        [(tag-eq? (cadr i) 'FETCH)
         (fetch-insert!
          (imap-fetches imap)
          ;; Convert result to assoc list:
          (cons (car i)
                (let ([new
                       (let loop ([l (caddr i)])
                         (if (null? l)
                           null
                           (cons (cons (car l) (cadr l))
                                 (loop (cddr l)))))])
                  ;; Keep anything not overridden:
                  (let ([old (cdr (or (fetch-find (imap-fetches imap) (car i))
                                      '(0)))])
                    (let loop ([old old][new new])
                      (cond
                        [(null? old) new]
                        [(assq (caar old) new)
                         (loop (cdr old) new)]
                        [else (loop (cdr old) (cons (car old) new))]))))))]
        [(ok-tag-eq? i 'UIDNEXT)
         (set-imap-uidnext! imap (ok-tag-val i))]
        [(ok-tag-eq? i 'UIDVALIDITY)
         (set-imap-uidvalidity! imap (ok-tag-val i))]
        [(ok-tag-eq? i 'UNSEEN)
         (set-imap-uidvalidity! imap (ok-tag-val i))]))
    (info-handler i)))

(define imap-port-number
  (make-parameter 143
                  (lambda (v)
                    (unless (and (number? v)
                                 (exact? v)
                                 (integer? v)
                                 (<= 1 v 65535))
                      (raise-type-error 'imap-port-number
                                        "exact integer in [1,65535]"
                                        v))
                    v)))

(define (has-starttls? imap)
  (let ([has? #f])
    (check-ok (imap-send imap
                         "CAPABILITY"
                         (lambda (caps) 
                           (when (member 'STARTTLS caps)
                             (set! has? #t)))))
    has?))

(define (imap-login imap username password inbox)
  (let ([reply (imap-send imap (list "LOGIN" username password) void)])
    (if (and (pair? reply) (tag-eq? 'NO (car reply)))
      (error 'imap-connect
        "username or password rejected by server: ~s" reply)
      (check-ok reply)))
  (let-values ([(init-count init-recent) (imap-reselect imap inbox)])
    (values imap init-count init-recent)))

(define (ports->tls-ports r w)  
  (ports->ssl-ports r w #:close-original? #t #:encrypt 'tls))

(define (imap-connect* r w username password inbox
                       #:tls? [tls? #f]
                       #:try-tls? [try-tls? #t])
  (with-handlers ([void
                   (lambda (x)
                     (close-input-port r)
                     (close-output-port w)
                     (raise x))])
    (let-values ([(r w)
                  (if tls?
                      (ports->tls-ports r w)
                      (values r w))])
      (let ([imap (make-imap r w #f #f #f #f #f
                             (new-tree) (new-tree) #f)])
        (check-ok (imap-send imap "NOOP" void))
        (define imap-maybe-tls
          (if (and (not tls?) try-tls? (has-starttls? imap))
              (begin
                (check-ok (imap-send imap "STARTTLS" void))
                (let-values ([(ssl-in ssl-out) (ports->tls-ports r w)])
                  (make-imap ssl-in ssl-out #f #f #f #f #f (new-tree) (new-tree) #f)))
              imap))
        (imap-login imap-maybe-tls username password inbox)))))

(define (imap-connect server username password inbox 
                       #:tls? [tls? #f]
                       #:try-tls? [try-tls? #t])
  ;; => imap count-k recent-k
  (let-values ([(r w)
                (if debug-via-stdio?
                    (begin
                      (printf "stdin == ~a\n" server)
                      (values  (current-input-port) (current-output-port)))
                    (tcp-connect server (imap-port-number)))])
    (imap-connect* r w username password inbox #:tls? tls? #:try-tls? try-tls?)))

(define (imap-reselect imap inbox)
  (imap-selectish-command imap (list "SELECT" inbox) #t))

(define (imap-examine imap inbox)
  (imap-selectish-command imap (list "EXAMINE" inbox) #t))

;; Used to return (values #f #f) if no change since last check?
(define (imap-noop imap)
  (imap-selectish-command imap "NOOP" #f))

(define (imap-selectish-command imap cmd reset?)
  (let ([init-count #f]
        [init-recent #f])
    (check-ok (imap-send imap cmd void))
    (when reset?
      (set-imap-expunges! imap (new-tree))
      (set-imap-fetches! imap (new-tree))
      (set-imap-new?! imap #f))
    (values (imap-exists imap) (imap-recent imap))))

(define (imap-status imap inbox flags)
  (unless (and (list? flags)
               (andmap (lambda (s)
                         (memq s '(messages recent uidnext uidvalidity unseen)))
                       flags))
    (raise-type-error 'imap-status "list of status flag symbols" flags))
  (let ([results null])
    (check-ok (imap-send imap (list "STATUS" inbox (box (format "~a" flags)))
                         (lambda (i)
                           (when (and (list? i) (= 3 (length i))
                                      (tag-eq? (car i) 'STATUS))
                             (set! results (caddr i))))))
    (map (lambda (f)
           (let loop ([l results])
             (cond
               [(or (null? l) (null? (cdr l))) #f]
               [(tag-eq? f (car l)) (cadr l)]
               [else (loop (cdr l))])))
         flags)))

(define (imap-poll imap)
  (when (and ;; Check for async messages from the server
             (char-ready? (imap-r imap))
             ;; It has better start with "*"...
             (= (peek-byte (imap-r imap)) (char->integer #\*)))
    ;; May set fields in `imap':
    (get-response (imap-r imap) #f (wrap-info-handler imap void) null)
    (void)))

(define (imap-get-updates imap)
  (no-expunges 'imap-updates imap)
  (let ([l (fetch-tree->list (imap-fetches imap))])
    (set-imap-fetches! imap (new-tree))
    l))

(define (imap-pending-updates? imap)
  (not (tree-empty? (imap-fetches imap))))

(define (imap-get-expunges imap)
  (let ([l (expunge-tree->list (imap-expunges imap))])
    (set-imap-expunges! imap (new-tree))
    l))

(define (imap-pending-expunges? imap)
  (not (tree-empty? (imap-expunges imap))))

(define (imap-reset-new! imap)
  (set-imap-new?! imap #f))

(define (imap-messages imap)
  (imap-exists imap))

(define (imap-disconnect imap)
  (let ([r (imap-r imap)]
        [w (imap-w imap)])
    (check-ok (imap-send imap "LOGOUT" void))
    (close-input-port r)
    (close-output-port w)))

(define (imap-force-disconnect imap)
  (let ([r (imap-r imap)]
        [w (imap-w imap)])
    (close-input-port r)
    (close-output-port w)))

(define (no-expunges who imap)
  (unless (tree-empty? (imap-expunges imap))
    (raise-mismatch-error who "session has pending expunge reports: " imap)))

(define (msg-set msgs)
  (apply
   string-append
   (let loop ([prev #f][msgs msgs])
     (cond
       [(null? msgs) null]
       [(and prev
             (pair? (cdr msgs))
             (= (add1 prev) (car msgs)))
        (loop (car msgs) (cdr msgs))]
       [prev (cons (format ":~a," prev)
                   (loop #f msgs))]
       [(null? (cdr msgs)) (list (format "~a" (car msgs)))]
       [(= (add1 (car msgs)) (cadr msgs))
        (cons (format "~a" (car msgs))
              (loop (car msgs) (cdr msgs)))]
       [else (cons (format "~a," (car msgs))
                   (loop #f (cdr msgs)))]))))

(define (imap-get-messages imap msgs field-list)
  (no-expunges 'imap-get-messages imap)
  (when (or (not (list? msgs))
            (not (andmap integer? msgs)))
    (raise-type-error 'imap-get-messages "non-empty message list" msgs))
  (when (or (null? field-list)
            (not (list? field-list))
            (not (andmap (lambda (f) (assoc f field-names)) field-list)))
    (raise-type-error 'imap-get-messages "non-empty field list" field-list))

  (if (null? msgs)
    null
    (begin
      ;; FETCH request adds info to `(imap-fectches imap)':
      (imap-send imap
                 (list "FETCH"
                       (box (msg-set msgs))
                       (box
                        (format "(~a)"
                                (splice (map (lambda (f)
                                               (cadr (assoc f field-names)))
                                             field-list)
                                        " "))))
                 void)
      ;; Sort out the collected info:
      (let ([flds (map (lambda (f) (cadr (assoc f field-names)))
                       field-list)])
        (begin0
            ;; For each msg, try to get each field value:
            (map
             (lambda (msg)
               (let ([m (or (fetch-find (imap-fetches imap) msg)
                            (error 'imap-get-messages "no result for message ~a" msg))])
                 (let loop ([flds flds][m (cdr m)])
                   (cond
                     [(null? flds)
                      (if (null? m)
                        (fetch-delete! (imap-fetches imap) msg)
                        (fetch-insert! (imap-fetches imap) (cons msg m)))
                      null]
                     [else
                      (let ([a (assoc (car flds) m)])
                        (cons (and a (cdr a))
                              (loop (cdr flds) (if a (remq a m) m))))]))))
             msgs))))))

(define (imap-store imap mode msgs flags)
  (no-expunges 'imap-store imap)
  (check-ok
   (imap-send imap
              (list "STORE"
                    (box (msg-set msgs))
                    (case mode
                      [(+) "+FLAGS.SILENT"]
                      [(-) "-FLAGS.SILENT"]
                      [(!) "FLAGS.SILENT"]
                      [else (raise-type-error 'imap-store
                                              "mode: '!, '+, or '-" mode)])
                    (box (format "~a" flags)))
              void)))

(define (imap-copy imap msgs dest-mailbox)
  (no-expunges 'imap-copy imap)
  (check-ok
   (imap-send imap (list "COPY" (box (msg-set msgs)) dest-mailbox) void)))

(define (imap-append imap dest-mailbox msg)
  (no-expunges 'imap-append imap)
  (let ([msg (if (bytes? msg) msg (string->bytes/utf-8 msg))])
    (check-ok
     (imap-send imap (list "APPEND"
                           dest-mailbox
                           (box "(\\Seen)")
                           (box (format "{~a}" (bytes-length msg))))
                void
                (lambda (loop contin)
                  (fprintf (imap-w imap) "~a\r\n" msg)
                  (loop))))))

(define (imap-expunge imap)
  (check-ok (imap-send imap "EXPUNGE" void)))

(define (imap-mailbox-exists? imap mailbox)
  (let ([exists? #f])
    (check-ok (imap-send imap
                         (list "LIST" "" mailbox)
                         (lambda (i)
                           (when (and (pair? i) (tag-eq? (car i) 'LIST))
                             (set! exists? #t)))))
    exists?))

(define (imap-create-mailbox imap mailbox)
  (check-ok (imap-send imap (list "CREATE" mailbox) void)))

(define (imap-get-hierarchy-delimiter imap)
  (let ([result #f])
    (check-ok
     (imap-send imap (list "LIST" "" "")
                (lambda (i)
                  (when (and (pair? i) (tag-eq? (car i) 'LIST))
                    (set! result (caddr i))))))
    result))

(define imap-list-child-mailboxes
  (case-lambda
    [(imap mailbox)
     (imap-list-child-mailboxes imap mailbox #f)]
    [(imap mailbox raw-delimiter)
     (let* ([delimiter (or raw-delimiter (imap-get-hierarchy-delimiter imap))]
            [mailbox-name (and mailbox (bytes-append mailbox delimiter))]
            [pattern (if mailbox
                       (bytes-append mailbox-name #"%")
                       #"%")])
       (map (lambda (p)
              (list (car p)
                    (cond
                      [(symbol? (cadr p))
                       (string->bytes/utf-8 (symbol->string (cadr p)))]
                      [(string? (cadr p))
                       (string->bytes/utf-8 (symbol->string (cadr p)))]
                      [(bytes? (cadr p))
                       (cadr p)])))
            (imap-list-mailboxes imap pattern mailbox-name)))]))

(define (imap-mailbox-flags imap mailbox)
  (let ([r (imap-list-mailboxes imap mailbox #f)])
    (if (= (length r) 1)
      (caar r)
      (error 'imap-mailbox-flags "could not get flags for ~s (~a)"
             mailbox
             (if (null? r) "no matches" "multiple matches")))))

(define (imap-list-mailboxes imap pattern except)
  (let* ([sub-folders null])
    (check-ok
     (imap-send imap (list "LIST" "" pattern)
                (lambda (x)
                  (when (and (pair? x)
                             (tag-eq? (car x) 'LIST))
                    (let* ([flags (cadr x)]
                           [name (cadddr x)]
                           [bytes-name (if (symbol? name)
                                         (string->bytes/utf-8 (symbol->string name))
                                         name)])
                      (unless (and except
                                   (bytes=? bytes-name except))
                        (set! sub-folders
                              (cons (list flags name) sub-folders))))))))
    (reverse sub-folders)))
