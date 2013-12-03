(module seqcontract mzscheme
  (require mzlib/class)
  (provide (protect es-contract-mixin lock-contract-mixin))
  
  (require-for-syntax syntax/stx
                      syntax/boundmap)

  (define-syntax (sequence-contract-mixin stx)
    (syntax-case stx (state-machine)
      [(_ (state-machine
           [name exp (method-name aritiess states ...) ...] ...)
          clauses ...)
       (and (andmap identifier? (syntax->list (syntax (name ...))))
            (andmap (lambda (x) (andmap identifier? (syntax->list x)))
                    (syntax->list (syntax ((method-name ...) ...))))
            (andmap (lambda (xs) 
                      (andmap (lambda (x) (andmap identifier? (syntax->list x)))
                              (syntax->list xs)))
                    (syntax->list (syntax (((states ...) ...) ...)))))
       (let ()
         (define state-names (syntax->list (syntax (name ...))))
         (define predicate-names (generate-temporaries (syntax (name ...))))
         
         (define state-name->predicate-name
           (let ([mapping (make-bound-identifier-mapping)])
             (for-each (lambda (state-name predicate-name)
                         (bound-identifier-mapping-put! mapping state-name predicate-name))
                       state-names
                       predicate-names)
             (lambda (state-name)
               (bound-identifier-mapping-get mapping state-name))))
         
         (define-struct state-desc (method-name arities predicate-name state-name result-predicates) (make-inspector))
         
         ;; -> mapping[state-name-symbol -o> state-desc]
         (define (build-table)
           (let ([mapping (new-mapping)])
             (for-each 
              (lambda (state-name-stx predicate-name-stx method-names aritiess state-namess)
                (for-each
                 (lambda (method-name arities state-names-stx)
                   (extend-mapping/at-end
                    mapping
                    method-name
                    (make-state-desc method-name
                                     arities
                                     predicate-name-stx
                                     state-name-stx
                                     (syntax->list state-names-stx))))
                 (syntax->list method-names)
                 (syntax->list aritiess)
                 (syntax->list state-namess)))
              (syntax->list (syntax (name ...)))
              predicate-names
              (syntax->list (syntax ((method-name ...) ...)))
              (syntax->list (syntax ((aritiess ...) ...)))
              (syntax->list (syntax (((states ...) ...) ...))))
             mapping))
         
         (define (build-overriding-method mapping state-descs)
           (with-syntax ([method-name (state-desc-method-name (car state-descs))]
                         [super-method-name (build-super-name (state-desc-method-name (car state-descs)))]
                         [(predicate-name ...) (map state-desc-predicate-name state-descs)]
                         [(predicate-result-name ...)
                          (generate-temporaries
                           (map state-desc-predicate-name state-descs))]
                         [(state-name ...) (map state-desc-state-name state-descs)]
                         [((result-predicate-state ...) ...)
                          (map state-desc-result-predicates state-descs)]
                         [((result-predicate-name ...) ...)
                          (map 
                           (lambda (state-desc)
                             (map state-name->predicate-name
                                  (state-desc-result-predicates state-desc)))
                           state-descs)])
             (with-syntax ([(cases ...)
                            (map (lambda (arity)
                                   (with-syntax ([formals arity])
                                     (with-syntax ([call (if (identifier? arity)
                                                             (syntax (super-method-name . formals))
                                                             (with-syntax ([(x ...) arity])
                                                               (syntax (super-method-name x ...))))]
                                                   [args-as-list
                                                    (if (identifier? arity)
                                                        arity
                                                        (with-syntax ([(x ...) arity])
                                                          (syntax (list x ...))))])
                                       (syntax
                                        [formals
                                         (let ([predicate-result-name (predicate-name)] ...)
                                           (cond
                                             [predicate-result-name
                                              call
                                              
                                              ;; Doesn't do post-condition checking,
                                              ;; since it isn't thread safe
                                              #;
                                              (begin0
                                                call
                                                (unless (or (result-predicate-name) ...)
                                                  (sequence-contract-violation 
                                                   'positive 
                                                   "expected one of states ~s after calling ~s in state ~s"
                                                   '(result-predicate-state ...)
                                                   'method-name
                                                   'state-name)))
                                              ]
                                             ...
                                             [else
                                              (sequence-contract-violation 
                                               'negative
                                               "method ~s cannot be called, except in states ~s~a"
                                               'method-name
                                               '(state-name ...)
                                               (format-args args-as-list))]))]))))
                                 (syntax->list (state-desc-arities (car state-descs))))])
               (syntax
                (begin
                  (rename-super [super-method-name method-name])
                  (define/override method-name
                    (case-lambda cases ...)))))))
         
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ;;
         ;; finite mapping code
         ;;
         
         (define (new-mapping) (make-hash-table))
         (define (set-mapping mapping key-stx val)
           (hash-table-put! mapping (syntax-e key-stx) val))
         (define get-mapping
           (case-lambda
             [(mapping key-stx) (get-mapping mapping key-stx (lambda () (error 'get-mapping "ack!")))]
             [(mapping key-stx fail)
              (hash-table-get mapping (syntax-e key-stx) fail)]))
         (define (extend-mapping/at-end mapping key-stx ele)
           (set-mapping mapping key-stx 
                        (append
                         (get-mapping mapping key-stx (lambda () null))
                         (list ele))))
         (define (mapping-map f mapping)
           (hash-table-map mapping f))
         
         ;;
         ;;
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         
         (define (build-super-name name)
           (datum->syntax-object
            name
            (string->symbol
             (format 
              "super-~a"
              (syntax-object->datum name)))))
         
         (define table (build-table))
         (with-syntax ([(predicate-names ...) predicate-names]
                       [(overriding-method ...) (mapping-map
                                                 (lambda (k vs) (build-overriding-method table vs))
                                                 table)])
           
           (syntax
            (lambda (%)
              (class* % ()
                (define/private predicate-names (lambda () exp)) ...
                overriding-method ... 
                clauses ...)))))]))
  
  (define (format-args l)
    (cond
      [(null? l) ""]
      [else
       (string-append
        ", args "
        (let loop ([fst (car l)]
                   [rst (cdr l)])
          (cond
            [(null? rst) (format "~e" fst)]
            [else (string-append
                   (format "~e" fst)
                   " "
                   (loop (car rst) (cdr rst)))])))]))

  (define (sequence-contract-violation dir fmt . args)
    (apply error
           'sequence-contract-violation 
           (string-append (format "~a: " dir) fmt)
           args))

  (define es-contract-mixin 
    (sequence-contract-mixin
     (state-machine
      [in-edit-sequence
       (in-edit-sequence?)
       (begin-edit-sequence [() (x) (x y)] in-edit-sequence)
       (end-edit-sequence [()] in-edit-sequence out-of-edit-sequence)]
      [out-of-edit-sequence
       (not (in-edit-sequence?))
       (begin-edit-sequence [() (x) (x y)] in-edit-sequence)])
     
     (inherit in-edit-sequence?)
     (super-new)))

#|

  (define (test t)
    (send t begin-edit-sequence)
    (send t end-edit-sequence)
    (send t end-edit-sequence))
  
  (test (new text%))
  (test (new (es-contract-mixin text%)))
  
Matthew writes:

> Underscore tends to mean "internal". Many variants of
> Insert(), for example, call the main _Insert() method.

So, double check the methods to make sure that a flag check
in an underscore method means the flag is checked in the
non-underscore methods.

At Sun, 29 Jun 2003 09:26:02 -0500, Robby Findler wrote:
> Is there some kind of invariant or ordering on these
> flags? That is, if a method only checks the flowLocked flag,
> is that effectively the same as checking the flowLocked flag
> or the writeLocked flag or something like that?

Yes: readLocked => flowLocked, and flowLocked => writeLocked.

Matthew

  |#
  
  ;; need to figure out
  ;; line-start-position and friends 
  ;; (line-start-position not valid in readlock)
  
  (define lock-contract-mixin
    (sequence-contract-mixin
     (state-machine
      [unlocked
       (and (not (locked-for-write?))
            (not (locked-for-flow?))
            (not (locked-for-read?)))
       (set-position [(x) (x y) (x y z) (x y z p) (x y z p q) (x y z p q r)] unlocked)
       (set-autowrap-bitmap [(bitmap)] unlocked)
       (print-to-dc [(dc) (dc page)] unlocked)
       (move-position [(code?) (code? extend) (code? extend kind)] unlocked)
       (split-snip [(pos)] unlocked)
       (set-line-spacing [(space)] unlocked)
       (set-max-width [(width)] unlocked)
       (set-min-width [(width)] unlocked)
       (set-min-height [(width)] unlocked)
       (set-max-height [(width)] unlocked)
       (set-tabs [(tabs) (tabs tab-width) (tabs tab-width units?)] unlocked)
       (print [()
               (interactive?)
               (interactive? fit-on-page?)
               (interactive? fit-on-page? output-mode)
               (interactive? fit-on-page? output-mode parent)
               (interactive? fit-on-page? output-mode parent force-ps-page-bbox?)
               (interactive? fit-on-page? output-mode parent force-ps-page-bbox? as-eps?)]
              unlocked)
       
       (get-text [() (x) (x y) (x y z) (x y z p)] unlocked)
       (get-flattened-text [()] unlocked)
       (get-character [(start)] unlocked)
       (find-wordbreak [(start end reason)] unlocked)
       (save-file [() (filename) (filename format) (filename format show-errors?)] unlocked)
       (write-to-file [(stream) (stream start) (stream start end)]  unlocked)
       (find-position [(x y) (x y at-eol?) (x y at-eol? on-it?) (x y at-eol? on-it? edge-close?)] unlocked)
       (scroll-line-location [(pos)] unlocked)
       (num-scroll-lines [()] unlocked)
       (find-scroll-line [(location)] unlocked)
       (style-has-changed [(style)] unlocked)

       (set-paragraph-margins [(para fl l r)] unlocked)
       (set-paragraph-alignment [(para align)] unlocked)

       (change-style [(x) (x y) (x y z) (x y z w)] unlocked)
       (insert [(x) (x y) (x y z) (x y z p) (x y z p q)] unlocked)
       (delete [() (start) (start end) (start end scroll-ok?)] unlocked)
       (insert-port [(port) (port format) (port format show-errors?)] unlocked)
       (read-from-file [(x) (x y) (x y z)] unlocked)
       (set-style-list [(style-list)] unlocked)]
      
      [write-lock
       (and (locked-for-write?)
            (not (locked-for-flow?))
            (not (locked-for-read?)))

       (set-position [(x) (x y) (x y z) (x y z p) (x y z p q)] write-lock)
       (set-autowrap-bitmap [(bitmap)] write-lock)
       (print-to-dc [(dc)] write-lock)
       (move-position [(code?) (code? extend) (code? extend kind)] write-lock)
       (split-snip [(pos)] write-lock)
       (set-line-spacing [(space)] write-lock)
       (set-max-width [(width)] write-lock)
       (set-min-width [(width)] write-lock)
       (set-min-height [(width)] write-lock)
       (set-max-height [(width)] write-lock)
       (set-tabs [(tabs) (tabs tab-width) (tabs tab-width units?)] write-lock)
       (print [()
               (interactive?)
               (interactive? fit-on-page?)
               (interactive? fit-on-page? output-mode)
               (interactive? fit-on-page? output-mode parent)
               (interactive? fit-on-page? output-mode parent force-ps-page-bbox?)]
              write-lock)
       
       (get-text [() (x) (x y) (x y z) (x y z p)] write-lock)
       (get-flattened-text [()] write-lock)
       (get-character [(start)] write-lock)
       (find-wordbreak [(start end reason)] write-lock)
       (save-file [() (filename) (filename format) (filename format show-errors?)] write-lock)
       (write-to-file [(stream) (stream start end)]  write-lock)
       (find-position [(x y) (x y at-eol? on-it? edge-close?)] write-lock)
       (scroll-line-location [(pos)] write-lock)
       (num-scroll-lines [()] write-lock)
       (find-scroll-line [(location)] write-lock)
       (style-has-changed [(style)] write-lock)]
      
      [flow-lock
       (and (locked-for-flow?)
            (not (locked-for-read?)))

       (get-text [() (x) (x y) (x y z) (x y z p)] flow-lock)
       (get-flattened-text [()] flow-lock)
       (get-character [(start)] flow-lock)
       (find-wordbreak [(start end reason)] flow-lock)
       (save-file [() (filename) (filename format) (filename format show-errors?)] flow-lock)
       (write-to-file [(stream) (stream start end)]  flow-lock)
       (find-position [(x y) (x y at-eol? on-it? edge-close?)] flow-lock)
       (scroll-line-location [(pos)] flow-lock)
       (num-scroll-lines [()] flow-lock)
       (find-scroll-line [(location)] flow-lock)
       (style-has-changed [(style)] flow-lock)]

      [read-lock
       (locked-for-read?)])

     (inherit locked-for-flow?
              locked-for-write?
              locked-for-read?)
     (super-new))))

  #|
     ;; flowLocked in wx_mpriv
     set-position ; _SetPosition
     CheckRecalc  (only if graphicMaybeInvalid aka locations-computed?)
     set-autowrap-bitmap ; SetAutowrapBitmap
     Redraw 
     BeginPrint
     EndPrint
     HasPrintPage
     print-to-dc ; PrintToDC
     
     ;; flowlocked in wx_media.cxx
     move-position ; MovePosition
     split-snip ; SplitSnip
     set-line-spacing ; SetLineSpacing
     set-max-width ; SetMaxWidth
     set-min-width ; SetMinWidth
     set-min-height ; SetMinHeight
     set-max-height ; SetMaxHeight
     set-tabs ; SetTabs
     resized ; Resized ;; uses the flag, but not to abort
     
     ;; methods that consider
     ;; the readLocked variable,
     ;; at the C level; they just
     ;; return if it is set.
     get-text ; GetText
     get-character ; GetCharacter
     find-wordbreak ; FindWorkbreak
     save-file ; SaveFile
     write-to-file ; WriteToFile
     _FindPositionInSnip
     find-position ; FindPosition
     scroll-line-location ; ScrollLineLocation
     num-scroll-lines ; NumScrollLines
     find-scroll-line ; FindScrollLine
     style-has-changed ; StyleHasChanged ;; maybe need to expand this to include style lists?

     FindFirstVisiblePosition ;; LineStartPosition?
     FindLastVisiblePosition
     CheckRecalc
     
     ;; methods that consider the writeLocked variable,
     ;; at the C level
     _ChangeStyle
     _Insert
     _Delete
     insert-port ; InsertPort
     read-from-file ; ReadFromFile
     set-style-list ; SetStyleList
     ; Recounted
     ReallyCanEdit -- only when op != wxEDIT_COPY
     
     ;; in wx_mpbrd.cxx
     insert ; Insert
     delete ; Delete
     erase ; Erase
     delete ; Delete ;; -- with arg
     remove ; Remove
     move-to ; MoveTo
     move ; Move, also with arg
     change-style ; _ChangeStyle
     set-before ;SetBefore
     set-after ;SetAfter
     ;ReallyCanEdit -- only when op != wxEDIT_COPY
     ;Refresh has weird code checking writeLocked -- what does < 0 mean?
     do-paste ; DoPaste
     paste ; Paste
     insert-port ; InsertPort
     insert-file ; InsertFile
     read-from-file ; ReadFromFile
     ; BeginEditSequence ;; -- weird flag check
     ; EndEditSequence ;; -- weird flag check, like BeginEditSequence
  
  |#
