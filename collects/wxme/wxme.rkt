(module wxme mzscheme
  (require mzlib/port
           mzlib/string
           mzlib/kw
           racket/class
           racket/contract
           mzlib/list
           scheme/gui/dynamic
           syntax/modread
           (only racket/snip/private/snip int->img-type)
           "image.rkt"
           "editor.rkt"
           "private/compat.rkt")

  (define (decode who port snip-filter close? skip-content?)
    (expect #rx#"^WXME" port who "does not start with \"WXME\"")
    (expect #rx#"^01" port who "unrecognized format (not \"01\")")
    (let ([vers (string->number
                 (bytes->string/latin-1
                  (expect #rx#"^0[1-8]" port who "unrecognized version")))])
      (unless (vers . < . 4)
        (expect #rx#"^ ##[ \r\n]" port who "missing \" ## \" tag in the expected place"))
      (let ([header (read-header who port vers snip-filter skip-content?)])
        (let ([port (port->decoded-port who port vers header close?)])
          (if skip-content?
              (extract-used header port)
              port)))))
  
  (define (expect rx port who msg)
    (let ([m (regexp-match rx port)])
      (unless m
        (error who "bad WXME stream; ~a" msg))
      (car m)))

  (define (extract-used header port)
    (let loop ()
      (unless (eof-object? (read-byte-or-special port))
        (loop)))
    (values
     (filter values (map (lambda (c)
                           (and (snip-class-used? c)
                                (bytes->string/latin-1 (snip-class-name c))))
                         (vector->list (header-classes header))))
     (filter values (map (lambda (c)
                           (and (data-class-used? c)
                                (bytes->string/latin-1 (data-class-name c))))
                         (vector->list (header-data-classes header))))))

  ;; ----------------------------------------
  
  (define-struct header (classes data-classes styles snip-filter skip-unknown? snips-to-go stream skip-content?))
  (define (header-plain-text? h)
    (not (header-snip-filter h)))

  (define (read-header who port vers snip-filter skip-content?)
    (let* ([classes (read-snip-class-list who port vers)]
           [data-classes (read-data-class-list who port vers)]
           [header (make-header classes 
                                data-classes
                                (make-hash-table) 
                                snip-filter 
                                (unknown-extensions-skip-enabled)
                                0
                                #f
                                skip-content?)])
      (set-header-stream! header (make-object stream% who port vers header))
      (let ([cnt (read-editor who port vers header)])
        (set-header-snips-to-go! header cnt))
      header))

  (define (read-editor who port vers header)
    (discard-headers/footers who port vers)
    (read-styles who port vers (header-styles header))
    (read-class-headers who port vers header)
    (read-integer who port vers "snip count"))

  (define (read-editor-footers who port vers header)
    (discard-headers/footers who port vers))

  (define (read-editor-snip who port vers header)
    (let ([cnt (read-editor who port vers header)])
      (let loop ([cnt cnt][accum null])
        (if (zero? cnt)
            (begin
              (read-editor-footers who port vers header)
              (if (header-plain-text? header)
                  (apply bytes-append (reverse accum))
                  (snip-results->port 'nested-editor
                                      (let ([results (reverse accum)])
                                        (lambda ()
                                          (and (pair? results)
                                               (begin0
                                                (car results)
                                                (set! results (cdr results))))))
                                      void)))
            (loop (sub1 cnt)
                  (cons (read-snip who port vers header)
                        accum))))))

  (define-struct snip-class (name version required? manager used?))

  (define (read-snip-class-list who port vers)
    (let ([cnt (read-integer who port vers "snip-class count")])
      (list->vector
       (let loop ([i 0])
         (if (= i cnt)
             null
             (cons
              (let ([name (read-a-string who port vers "snip-class name")])
                (make-snip-class name
                                 (read-integer who port vers "snip-class version")
                                 (begin (read-integer who port vers "snip-class required?")
                                        ;; required? value isn't actually used; only a few
                                        ;;  built-in classes are required
                                        (member name '(#"wxtext" #"wxtab" #"wxmedia")))
                                 #f
                                 #f))
              (loop (add1 i))))))))

  (define-struct data-class (name required? manager used?))

  (define (read-data-class-list who port vers)
    (let ([cnt (read-integer who port vers "data-class count")])
      (list->vector
       (let loop ([i 0])
         (if (= i cnt)
             null
             (cons
              (let ([name (read-a-string who port vers "data-class name")])
                (make-data-class name
                                 (equal? name #"wxloc")
                                 #f
                                 #f))
              (loop (add1 i))))))))

  (define (discard-headers/footers who port vers)
    (let ([cnt (read-fixed-integer who port vers "header/footer extension count")])
      (let loop ([i 0])
        (unless (= i cnt)
          (let ([len (read-fixed-integer who port vers "header/footer extension length")])
            (skip-data port vers len)
            (loop (add1 i)))))))

  (define (read-styles who port vers styles)
    (let ([id (read-integer who port vers "style-list id")])
      (hash-table-get styles id
                      (lambda ()
                        (let ([cnt (read-integer who port vers "style count")])
                          (let loop ([i 1])
                            (unless (= i cnt)
                              (unless ((read-integer who port vers "base-style id") . < . i)
                                (read-error who "integer less than current index" "base-style id" port))
                              (read-a-string who port vers "style name")
                              (if (zero? (read-integer who port vers "style is-join?"))
                                  (begin
                                    (read-integer who port vers "style family")
                                    (read-a-string who port vers "style face")
                                    (read-inexact who port vers "style size multiply")
                                    (read-integer who port vers "style size addition")
                                    (read-integer who port vers "style weight on")
                                    (read-integer who port vers "style weight off")
                                    (read-integer who port vers "style slant on")
                                    (read-integer who port vers "style slant off")
                                    (unless (vers . < . 5)
                                      (read-integer who port vers "style smoothing on")
                                      (read-integer who port vers "style smoothing off"))
                                    (read-integer who port vers "style underlined on")
                                    (read-integer who port vers "style underlined off")
                                    (unless (vers . < . 6)
                                      (read-integer who port vers "style size-in-pixels on")
                                      (read-integer who port vers "style size-in-pixels off"))
                                    (unless (vers . < . 3)
                                      (read-integer who port vers "style transparent on")
                                      (read-integer who port vers "style transparent off"))
                                    (read-inexact who port vers "style foreground multiply red")
                                    (read-inexact who port vers "style foreground multiply green")
                                    (read-inexact who port vers "style foreground multiply blue")
                                    (read-inexact who port vers "style background multiply red")
                                    (read-inexact who port vers "style background multiply green")
                                    (read-inexact who port vers "style background multiply blue")
                                    (read-integer who port vers "style foreground addition red")
                                    (read-integer who port vers "style foreground addition green")
                                    (read-integer who port vers "style foreground addition blue")
                                    (read-integer who port vers "style background addition red")
                                    (read-integer who port vers "style background addition green")
                                    (read-integer who port vers "style background addition blue")
                                    (read-integer who port vers "style alignment on")
                                    (read-integer who port vers "style alignment off"))
                                  (unless ((read-integer who port vers "shift-style id") . < . i)
                                    (read-error who "integer less than current index" "shift-style id" port)))
                              (loop (add1 i)))))
                        (hash-table-put! styles id id)))))

  (define (read-class-headers who port vers header)
    (let ([cnt (read-fixed-integer who port vers "class-header count")])
      (let loop ([i 0])
        (unless (= i cnt)
          (let ([pos (read-integer who port vers "class-header class index")]
                [len (read-fixed-integer who port vers "class-header length")])
            (let ([class (find-class pos header who port vers #f)])
              (if (and class
                       (object? (snip-class-manager class)))
                  (send (snip-class-manager class) read-header (snip-class-version class) (header-stream header))
                  (skip-data port vers len)))
            (loop (add1 i)))))))

  (define (read-snip who port vers header)
    (let ([pos (read-integer who port vers "snip class index")])
      (let ([class (find-class pos header who port vers #t)])
        (let ([len (and (or (not class)
                            (not (snip-class-required? class)))
                        (read-fixed-integer who port vers "snip length"))])
          (if (and class
                   (snip-class-manager class)
                   (not (and len
                             (header-skip-content? header))))
              (let ([style (read-integer who port vers "snip style index")])
                (read-snip/given-class class who port vers header #f))
              (begin
                (skip-data port vers len)
                #""))))))
  
  (define (read-snip/given-class class who port vers header skip-buffer-data?)
    (let ([cvers (snip-class-version class)])
      (define manager (snip-class-manager class))
      (define name (snip-class-name class))
      (let ([s (if (procedure? manager)
                   ;; Built-in snip class:
                   (manager who port vers cvers header)
                   ;; Extension snip class:
                   (let* ([text? (header-plain-text? header)]
                          [s (send manager read-snip
                                   text?
                                   cvers
                                   (header-stream header))])
                     (if (and text?
                              (not (bytes? s)))
                         (error 'read-snip 
                                "reader for ~a in text-only mode produced something other than bytes: ~e"
                                name
                                s)
                         s)))])
        (unless skip-buffer-data? (read-buffer-data who port vers header))
        (if (header-skip-content? header)
            #""
            (if (bytes? s)
                ;; Return bytes for the stream:
                s
                ;; Filter the non-bytes result, and then wrap it as
                ;;  a special stream result:
                (let ([s ((header-snip-filter header) s)])
                  (lambda (src line col pos)
                    (if (s . is-a? . readable<%>)
                        (send s read-special src line col pos)
                        s))))))))

  (define (read-buffer-data who port vers header)
    (let loop ()
      (let ([pos (read-integer who port vers "data-class index")])
        (unless (zero? pos)
          (let ([data-class (find-data-class pos header who port vers #t)])
            (let ([len (and (or (not data-class)
                                (not (data-class-required? data-class)))
                            (read-fixed-integer who port vers "data length"))])
              (if data-class
                  ((data-class-manager data-class) who port vers header)
                  (skip-data port vers len))))
          (loop)))))

  ;; ----------------------------------------

  (define (read-raw-string who port vers what)
    (let ([v (cond
              [(vers . >= . 8) (plain-read port)]
              [else (read-integer who port vers what)])])
      (unless (and (integer? v)
                   (exact? v)
                   (v . >= . 0))
        (read-error who "non-negative exact integer for string length" what port))
      (let ([s (cond
                [(vers . >= . 8) (plain-read port)]
                [else (read-bytes v port)])])
        (cond
         [(bytes? s) 
          (unless (= (bytes-length s) v)
            (read-error who "byte string whose length matches an integer count" what port))
          s]
         [(and (list? s)
               (andmap bytes? s))
          (let ([s (apply bytes-append s)])
            (unless (= (bytes-length s) v)
              (read-error who "list of byte strings whose total length matches an integer count" what port))
            s)]
         [else
          (read-error who "byte string or byte-string list" what port)]))))

  (define (read-a-string who port vers what)
    (let ([s (read-raw-string who port vers what)])
      (let ([len (bytes-length s)])
        (when (zero? len)
          (read-error who "non-empty raw string" what port))
        (subbytes s 0 (sub1 len)))))

  (define (read-integer who port vers what)
    (cond
     [(vers . >= . 8)
      (let ([v (plain-read port)])
        (unless (and (integer? v)
                     (exact? v)
                     (<= (- (expt 2 31)) v (expt 2 31)))
          (read-error who "exact integer between [-2^31,2^31]" what port))
        v)]
     [else
      (let ([b (read-byte port)])
        (cond
         [(not (zero? (bitwise-and b #x80)))
          (cond
           [(not (zero? (bitwise-and b #x40)))
            (cond
             [(bitwise-and b #x01)
              (let ([b (read-byte port)])
                ;; convert to signed:
                (if (b . > . 127)
                    (- b 256)
                    b))]
             [(not (zero? (bitwise-and b #x02)))
              (integer-bytes->integer (read-bytes 2 port) #t #t)]
             [else
              (integer-bytes->integer (read-bytes 4 port) #t #t)])]
           [else
            (bitwise-ior (arithmetic-shift (bitwise-and #x3F b) 8)
                         (read-byte port))])]
         [else b]))]))

  (define (read-fixed-integer who port vers what)
    (cond
     [(vers . >= . 8)
      (read-integer who port vers what)]
     [else
      (integer-bytes->integer (read-bytes 4 port)
                              #t
                              (if (vers . > . 1)
                                  #t
                                  (broken-wxme-big-endian?)))]))
      
  (define (read-inexact who port vers what)
    (cond
     [(vers . >= . 8)
      (let ([v (plain-read port)])
        (unless (and (number? v)
                     (real? v))
          (read-error who "real number" what port))
        v)]
     [else
      (floating-point-bytes->real (read-bytes 8 port)
                                  (if (vers . > . 1)
                                      #t
                                      (broken-wxme-big-endian?)))]))
      

  (define (read-error who expected what port)
    (error who "WXME format problem while reading for ~a (expected ~a) from port: ~e around position: ~a"
           what expected port
           (file-position port)))

  (define (skip-data port vers len)
    (if (vers . >= . 8)
        (let loop ([len len])
          (unless (zero? len)
            (plain-read port)
            (loop (sub1 len))))
        (read-bytes len port)))

  ;; ----------------------------------------

  (define snip-reader<%>
    (interface ()
      read-header
      read-snip))
  
  (define readable<%>
    (interface ()
      read-special))
  
  (define (find-class/name name header who port vers used?)
    (define classes-vec (header-classes header))
    (define pos 
      (let loop ([i 0])
        (cond
          [(< i (vector-length classes-vec))
           (if (equal? (bytes->string/latin-1 (snip-class-name (vector-ref classes-vec i)))
                       name)
               i
               (loop (+ i 1)))]
          [else #f])))
    (unless pos
      (read-error who (format "class index for ~s" name) "known class name" port))
    (find-class pos header who port vers used?))
  
  (define (find-class pos header who port vers used?)
    (define classes (header-classes header))
    (unless (< -1 pos (vector-length classes))
      (read-error who "integer less than class-list length" "class index" port))
    (let ([class (vector-ref classes pos)])
      (when used?
        (set-snip-class-used?! class #t))
      (unless (snip-class-manager class)
        (set-snip-class-manager!
         class
         (let ([name (snip-class-name class)])
           (cond
            [(member name '(#"wxtext" #"wxtab"))
             (lambda (who port vers cvers header)
               (read-integer who port vers "string-snip flags")
               (let ([s (read-raw-string who port vers "string-snip content")])
                 (cond
                  [(= cvers 1) (string->bytes/utf-8 (bytes->string/latin-1 s))]
                  [(= cvers 2)
                   ;; UTF-32!
                   (unless (zero? (remainder (bytes-length s) 4))
                     (read-error who "size of read byte string is not a multiple of 4" "string-snip content" port))
                   (let loop ([pos 0][accum null])
                     (if (= pos (bytes-length s))
                         (begin
                           (string->bytes/utf-8 (apply string (reverse accum))))
                         (loop (+ pos 4)
                               (cons (integer->char
                                      (let ([v (integer-bytes->integer (subbytes s pos (+ pos 4)) #f
                                                                       (broken-wxme-big-endian?))])
                                        (unless (or (<= 0 v #xD7FF)
                                                    (<= #xE000 v #x10FFFF))
                                          (read-error who "UTF-32 character; probably an endian order mismatch" 
                                                      "string-snip content" port))
                                        v))
                                     accum))))]
                  [(cvers . > . 2) s])))]
            [(equal? name #"wxmedia")
             (lambda (who port vers cvers header)
               (read-integer who port vers "nested-editor type")
               (read-integer who port vers "nested-editor border")
               (read-integer who port vers "nested-editor left margin")
               (read-integer who port vers "nested-editor top margin")
               (read-integer who port vers "nested-editor right margin")
               (read-integer who port vers "nested-editor bottom margin")
               (read-integer who port vers "nested-editor left inset")
               (read-integer who port vers "nested-editor top inset")
               (read-integer who port vers "nested-editor right inset")
               (read-integer who port vers "nested-editor bottom inset")
               (read-inexact who port vers "nested-editor min width")
               (read-inexact who port vers "nested-editor max width")
               (read-inexact who port vers "nested-editor min height")
               (read-inexact who port vers "nested-editor max height")
               (when (cvers . > . 1)
                 (read-integer who port vers "nested-editor tight-fit?"))
               (when (cvers . > . 2)
                 (read-integer who port vers "nested-editor alignment"))
               (when (cvers . > . 3)
                 (read-integer who port vers "use background color"))
               (let ([n (read-editor-snip who port vers header)])
                 (if (header-plain-text? header)
                     n
                     (make-object editor% n))))]
            [(equal? name #"wximage")
             (lambda (who port vers cvers header)
               (let ([filename (read-a-string who port vers "image-snip filename")]
                     [type (read-integer who port vers "image-snip type")]
                     [w (read-inexact who port vers "image-snip width")]
                     [h (read-inexact who port vers "image-snip height")]
                     [dx (read-inexact who port vers "image-snip x-offset")]
                     [dy (read-inexact who port vers "image-snip y-offset")]
                     [relative (read-integer who port vers "image-snip relative?")])
                 (let ([data
                        (and (and (equal? filename #"")
                                  (cvers . > . 1)
                                  (not (zero? type)))
                             ;; inlined image
                             (apply
                              bytes-append
                              (let ([len (read-fixed-integer who port vers "image-snip image length")])
                                (let loop ([i 0])
                                  (if (= i len)
                                      null
                                      (cons
                                       (read-raw-string who port vers "image-snip image content")
                                       (loop (add1 i))))))))])
                   (if (header-plain-text? header)
                       #"."
                       (make-object image%
                         (if data #f filename)
                         data w h dx dy
                         relative (int->img-type type))))))]
            [else
             (if (header-skip-content? header)
                 #f
                 ;; Load a manager for this snip class?
                 (let ([lib (string->lib-path (bytes->string/latin-1 name) #f)])
                   (if lib
                       (let ([mgr (dynamic-require lib 'reader)])
                         (unless (mgr . is-a? . snip-reader<%>)
                           (error who "reader provided by ~s is not an instance of snip-reader<%>" lib))
                         mgr)
                       (if (header-skip-unknown? header)
                           #f
                           (error who "cannot load snip-class reader: ~s" name)))))]))))
      class))

  (define (find-data-class pos header who port vers used?)
    (define data-classes (header-data-classes header))
    (unless (< -1 pos (vector-length data-classes))
      (read-error who "integer less than data-class-list length" "data-class index" port))
    (let ([data-class (vector-ref data-classes pos)])
      (when used?
        (set-data-class-used?! data-class #t))
      (unless (data-class-manager data-class)
        (set-data-class-manager!
         data-class
         (case (data-class-name data-class)
           [(#"wxloc") 
            (lambda (who port vers header)
              (read-inexact who port vers "location x")
              (read-inexact who port vers "location y"))]
           [else
            (if (header-skip-content? header)
                #f
                ;; Load a manager for this data class?
                (error who "cannot load data-class managers, yet"))])))
      data-class))

  (define-local-member-name get-vers get-header get-port)
  (define stream%
    (class object%
      (init-field who port vers header)

      (define/public (get-vers) vers)
      (define/public (get-header) header)
      (define/public (get-port) port)
      
      (public [rfi read-fixed-integer])
      (define (rfi what)
        (read-fixed-integer who port vers what))

      (public [ri read-integer])
      (define (ri what)
        (read-integer who port vers what))
      
      (public [rix read-inexact])
      (define (rix what)
        (read-inexact who port vers what))

      (define/public (read-raw-bytes what)
        (read-raw-string who port vers what))
      (define/public (read-bytes what)
        (read-a-string who port vers what))

      (public [rne read-editor-snip])
      (define (rne what)
        (read-editor-snip who port vers header))

      (super-new)))

  (define stream<%> (class->interface stream%))

  ;; ----------------------------------------
  
  (define lib-mapping (make-hash-table 'equal))

  (define (ok-string-element? m)
    (and (string? m)
         (regexp-match? #rx"^[-a-zA-Z0-9_. ]+$" m)
         (not (string=? m ".."))
         (not (string=? m "."))))
  
  (define (ok-lib-path? m)
    (and (pair? m)
         (eq? 'lib (car m))
         (pair? (cdr m))
         (list? m)
         (andmap ok-string-element? (cdr m))))

  (define (register-lib-mapping! str target)
    (unless (ok-lib-path? target)
      (error 'register-lib-mapping! "given target is not a valid marshalable lib path: ~s" target))
    (hash-table-put! lib-mapping str target))

  (define (string->lib-path str gui?)
    (or (let ([m (and (regexp-match #rx"^[(].*[)]$" str)
                      (let* ([p (open-input-string str)]
                             [m (read p)])
                        (and (eof-object? (read p))
                             m)))])
          (or (and (and (list? m)
                        (= (length m) 2)
                        (ok-lib-path? (car m))
                        (ok-lib-path? (cadr m)))
                   (if gui?
                       (car m)
                       (cadr m)))
              (and (and (ok-lib-path? m)
                        gui?)
                   m)))
        (and (not gui?)
             (hash-table-get lib-mapping str #f))))

  (register-compatibility-mappings! register-lib-mapping!)

  ;; ----------------------------------------

  (define plain-params
    (with-module-reading-parameterization
     (lambda ()
      (current-parameterization))))

  (define (plain-read port)
    (call-with-parameterization 
     plain-params 
     (lambda () 
       (with-handlers ([exn:fail:read? (lambda (x) 'no-good)])
         (read port)))))

  ;; ----------------------------------------

  (define (port->decoded-port who port vers header close?)
    (snip-results->port
     (object-name port)
     (lambda ()
       (let ([snips-to-go (header-snips-to-go header)])
         (cond
          [(zero? snips-to-go) 
           (read-editor-footers who port vers header)
           #f]
          [else 
           (set-header-snips-to-go! header (sub1 snips-to-go))
           (read-snip who port vers header)])))
     (if close?
         (lambda () (close-input-port port))
         void)))

  (define (snip-results->port name next-item! close)
    (define-values (r w) (make-pipe))
    (define (read-proc buffer)
      (if (char-ready? r)
          (read-bytes-avail! buffer r)
          (let ([s (next-item!)])
            (cond
             [(not s)
              (close-output-port w)
              eof]
             [(bytes? s)
              (write-bytes s w)
              (read-proc buffer)]
             [else s]))))
    (make-input-port/read-to-peek
     name
     read-proc
     #f
     close))

  ;; ----------------------------------------

  (define (skip-reader port)
    (regexp-match/fail-without-reading #rx#"^#reader[(]lib\"read.ss\"\"wxme\"[)]" port))

  (define (wxme-convert-port port close? snip-filter)
    ;; read optional #reader header:
    (skip-reader port)
    ;; decode:
    (decode 'read-bytes port snip-filter close? #f))

  ;; ----------------------------------------

  (define unknown-extensions-skip-enabled (make-parameter #f))

  (define broken-wxme-big-endian? (make-parameter (system-big-endian?)))

  (define (is-wxme-stream? p)
    (and (regexp-match-peek #rx#"^(?:#reader[(]lib\"read[.]ss\"\"wxme\"[)])?WXME01[0-9][0-9] ##[ \r\n]" p)
         #t))

  (define/kw (wxme-port->port port #:optional [close? #t] [snip-filter (lambda (x) x)])
    (wxme-convert-port port close? snip-filter))

  (define/kw (wxme-port->text-port port #:optional [close? #t])
    (wxme-convert-port port close? #f))

  (define (do-read orig-port who read)
    (let ([port (if (gui-available?)
                    ;; GUI mode, since GRacket is available:
                    (let ([text% (dynamic-require 'mred 'text%)]
                          [open-input-text-editor (dynamic-require 'mred 'open-input-text-editor)])
                      (let ([t (new text%)])
                        (send t insert-port orig-port 'standard)
                        (open-input-text-editor t 0 'end values (object-name orig-port) #t)))
                    ;; Non-GUI mode:
                    (decode who orig-port (lambda (x) x) #f #f))])
      ;; Turn on line counting if it was on before:
      (let-values ([(line col pos) (port-next-location orig-port)])
        (when line (port-count-lines! port)))
      ;; Read:
      (let ([v (read port)])
        (let ([v2 (let loop ()
                    (let ([v2 (read port)])
                      (if (special-comment? v2)
                          (loop)
                          v2)))])
          (if (eof-object? v2)
              v
              `(begin
                 ,@(list* v v2 (let loop ([accum null])
                                 (let ([v (read port)])
                                   (cond
                                    [(eof-object? v) (reverse accum)]
                                    [(special-comment? v) (loop accum)]
                                    [else (loop (cons v accum))]))))))))))

  (define (wxme-read port)
    (do-read port 'read read))

  (define (wxme-read-syntax source-name-v port)
    (datum->syntax-object
     #f
     (do-read port 'read-syntax
              (lambda (port)
                (read-syntax source-name-v port)))))

  (define (extract-used-classes port)
    (if (is-wxme-stream? port)
        (begin
          (skip-reader port)
          (decode 'extract-used-classes port (lambda (x) x) #f #t))
        (values null null)))

  (define (read-snip-from-port name who stream)
    (define vers (send stream get-vers))
    (define header (send stream get-header))
    (define port (send stream get-port))
    (define class (find-class/name name header who port vers #t))
    (read-snip/given-class class who port vers header #t))
  
  (provide/contract [is-wxme-stream? (input-port? . -> . any)]
                    [wxme-port->text-port (->* (input-port?) (any/c) input-port?)]
                    [wxme-port->port (->* (input-port?) (any/c (any/c . -> . any)) input-port?)]
                    [register-lib-mapping! (string? string? . -> . void?)]
                    [string->lib-path (string? any/c . -> . any)]
                    [extract-used-classes (input-port? . -> . any)]
                    [read-snip-from-port (-> string? any/c (is-a?/c stream<%>) any)])

  (provide unknown-extensions-skip-enabled
           broken-wxme-big-endian?
           snip-reader<%>
           readable<%>
           stream<%>
           wxme-read
           wxme-read-syntax))
