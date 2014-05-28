#lang racket/base

#|

profile todo:
  - use origin fields
  - sort out various ways of clearing out the profiling information

|#

(require errortrace/errortrace-key
         racket/unit
         racket/contract
         errortrace/stacktrace
         racket/class
         racket/path
         racket/gui/base
         string-constants
         framework
         framework/private/bday
         "embedded-snip-utils.rkt"
         drracket/private/drsig
         "bindings-browser.rkt"
         "stack-checkpoint.rkt"
         net/sendurl
         net/url
         racket/match
         mrlib/include-bitmap
         images/compile-time
         pkg/lib
         pkg/gui
         (for-syntax images/icons/misc images/icons/style images/icons/control images/logos)
         (for-syntax racket/base)
         (submod "frame.rkt" install-pkg))

(define orig (current-output-port))
(define (oprintf . args) (apply fprintf orig args))

(define base-phase
  (variable-reference->module-base-phase (#%variable-reference)))

(provide debug@)
(define-unit debug@
  (import [prefix drracket:rep: drracket:rep^]
          [prefix drracket:frame: drracket:frame^]
          [prefix drracket:unit: drracket:unit/int^]
          [prefix drracket:language: drracket:language^]
          [prefix drracket:language-configuration: drracket:language-configuration/internal^]
          [prefix drracket:init: drracket:init^]
          [prefix drracket: drracket:interface^])
  (export drracket:debug^)
  
  ;                                                          
  ;                                                          
  ;   ;                                                      
  ;   ;                             ;                        
  ;   ;                                                      
  ;   ; ;;   ;   ;   ;; ;         ;;;     ;;;;   ;;;   ; ;;  
  ;   ;;  ;  ;   ;  ;  ;;           ;    ;      ;   ;  ;;  ; 
  ;   ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
  ;   ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
  ;   ;   ;  ;   ;  ;   ;           ;    ;      ;   ;  ;   ; 
  ;   ;;  ;  ;  ;;  ;  ;;           ;    ;      ;   ;  ;   ; 
  ;   ; ;;    ;; ;   ;; ;           ;     ;;;;   ;;;   ;   ; 
  ;                     ;                                    
  ;                  ;;;                                     
  ;                                                          
  
  ;; type debug-source = (union symbol (instanceof editor<%>))
  
  ;; original-output-port : output-port
  ;; for debugging -- be sure to print to here, not the current output port
  (define original-output-port (current-output-port))
  
  ;; cms->srclocs : continuation-marks -> (listof srcloc)
  (define (cms->srclocs cms)
    (map 
     (λ (x) (make-srcloc (list-ref x 1)
                         (list-ref x 2)
                         (list-ref x 3)
                         (list-ref x 4)
                         (list-ref x 5)))
     (continuation-mark-set->list cms errortrace-key)))
  
  ;; error-delta : (instanceof style-delta%)
  (define error-delta (make-object style-delta% 'change-style 'italic))
  (send error-delta set-delta-foreground (make-object color% 255 0 0))
  
  ;; get-error-color : -> (instanceof color%)
  (define get-error-color
    (let ([w-o-b (make-object color% 63 0 0)]
          [b-o-w (make-object color% "PINK")])
      (λ ()
        (if (preferences:get 'framework:white-on-black?)
            w-o-b
            b-o-w))))
  
  (define arrow-cursor (make-object cursor% 'arrow))
  (define (clickable-snip-mixin snip%)
    (class snip%
      (init-rest args)
      (inherit get-flags set-flags get-admin get-extent)
      
      (define callback void)
      (define/public (set-callback cb) (set! callback cb))
      (define/public (get-callback) callback)
      
      (define grabbed? #f)
      (define in-bounds? #f)
      
      (define (set-clicked new-grabbed? new-in-bounds? dc)
        (define needs-invalidate?
          (or (not (equal? grabbed? new-grabbed?))
              (not (equal? new-in-bounds? in-bounds?))))
        (set! grabbed? new-grabbed?)
        (set! in-bounds? new-in-bounds?)
        (when needs-invalidate?
          (invalidate dc)))
      
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (super draw dc x y left top right bottom dx dy draw-caret)
        (when (and in-bounds? grabbed?)
          (let ([brush (send dc get-brush)]
                [pen (send dc get-pen)])
            (let-values ([(w h) (get-w/h dc)])
              (send dc set-brush (send the-brush-list find-or-create-brush "black" 'hilite))
              (send dc set-pen (send the-pen-list find-or-create-pen "white" 1 'transparent))
              (send dc draw-rectangle x y w h)
              (send dc set-pen pen)
              (send dc set-brush brush)))))
      
      (define/override (on-event dc x y editorx editory evt)
        (define-values (w h) (get-w/h dc))
        (define in-bounds? (and (<= (- (send evt get-x) x) w)
                                (<= (- (send evt get-y) y) h)))
        (cond
          [(send evt button-down? 'left)
           (set-clicked #t in-bounds? dc)]
          [(send evt button-up? 'left)
           (let ([admin (send this get-admin)])
             (when admin
               (send (send admin get-editor) set-caret-owner #f 'global)))
           (when (and grabbed? in-bounds?)
             (callback this))
           (set-clicked #f in-bounds? dc)]
          [else
           (set-clicked grabbed? in-bounds? dc)]))
      
      (define/private (invalidate dc)
        (let ([admin (get-admin)])
          (when admin
            (let-values ([(w h) (get-w/h dc)])
              (send admin needs-update this 0 0 w h)))))
      
      (define/private (get-w/h dc)
        (let ([wb (box 0)]
              [hb (box 0)])
          ;; know that the snip is the same size everywhere, 
          ;; so just use (0,0) for its position
          (get-extent dc 0 0 wb hb #f #f #f #f)
          (values (unbox wb)
                  (unbox hb))))
      
      (define/override (adjust-cursor dc x y editorx editory event)
        arrow-cursor)
      
      (apply super-make-object args)
      (set-flags (cons 'handles-events (get-flags)))))
  
  (define clickable-image-snip%  (clickable-snip-mixin image-snip%))
  (define clickable-string-snip%
    (class (clickable-snip-mixin snip%)
      (define/override (get-extent dc x y wb hb db sb lb rb)
        (define-values (w h d a) (send dc get-text-extent str))
        (set-box/f wb w)
        (set-box/f hb h)
        (set-box/f db d)
        (set-box/f sb a)
        (set-box/f lb 0)
        (set-box/f rb 0))
      (define/override (draw dc x y left top right bottom dx dy draw-caret)
        (define font (send dc get-font))
        (send dc set-font (send the-font-list find-or-create-font
                                (send font get-point-size)
                                (send font get-face)
                                (send font get-family)
                                (send font get-style)
                                (send font get-weight)
                                #t
                                (send font get-smoothing)
                                #f
                                (send font get-hinting)))
        (send dc draw-text str x y)
        (send dc set-font font))
      
      (inherit get-callback set-callback)
      (init-field str)
      (define/override (copy)
        (let ([n (new clickable-string-snip% [str str])])
          (send n set-callback (get-callback))
          n))
      (define/override (write f)
        (define bts (string->bytes/utf-8 str))
        (send f put (bytes-length bts) bts))
      (super-new)
      (inherit set-snipclass)
      (set-snipclass clickable-string-snipclass)))
  (define (set-box/f b v) (when (box? b) (set-box! b v)))
  (define clickable-string-snipclass
    (new (class snip-class%
           (define/override (read f)
             (define str (bytes->string/utf-8 (or (send f get-unterminated-bytes) #"")))
             (new clickable-string-snip% [str str]))
           (super-new))))
  (send clickable-string-snipclass set-classname "drclickable-string-snipclass")
  (send clickable-string-snipclass set-version 0)
  (send (get-the-snip-class-list) add clickable-string-snipclass)
  
  ;; make-note% : string -> (union class #f)
  (define (make-note% filename bitmap)
    (and (send bitmap ok?)
         (letrec ([note%
                   (class clickable-image-snip%
                     (inherit get-callback)
                     (define/public (get-image-name) filename)
                     (define stack1 #f)
                     (define stack2 #f)
                     (define/public (set-stacks s1 s2) (set! stack1 s1) (set! stack2 s2))
                     (define/public (get-stacks) (values stack1 stack2))
                     (define/override (copy) 
                       (let ([n (new note%)])
                         (send n set-callback (get-callback))
                         (send n set-stacks stack1 stack2)
                         n))
                     (super-make-object bitmap))])
           note%)))
  
  (define file-note%
    (make-note% "stop-22x22.png" (compiled-bitmap (stop-sign-icon #:color halt-icon-color))))
  (define bug-note%
    (make-note% "stop-multi.png" (compiled-bitmap (stop-signs-icon #:color halt-icon-color))))
  
  (define mf-note% (make-note% "mf.gif" (include-bitmap (lib "icons/mf.gif") 'gif)))
  (define small-planet-bitmap (compiled-bitmap (planet-logo #:height (default-icon-height))))
  (define planet-note% (make-note% "small-planet.png" small-planet-bitmap))
  (define install-note% 
    (class clickable-image-snip%
      (inherit get-callback)
      (define/override (copy) 
        (let ([n (new install-note%)])
          (send n set-callback (get-callback))
          n))
      (super-new)))
  
  ;; display-stats : (syntax -> syntax)
  ;; count the number of syntax expressions & number of with-continuation-marks in an 
  ;; expanded expression ... except that it counts keywords, too.
  ;; returns its argument.
  ;(define (display-stats stx)
  ;  (let ([exps 0]
  ;        [wcms 0])
  ;    (let loop ([stx stx])
  ;      (kernel-syntax-case stx ()
  ;        [(#%with-continuation-mark key mark body)
  ;         (set! wcms (+ wcms 1))
  ;         (loop #`body)]
  ;        [(subexps ...)
  ;         (set! exps (+ exps 1))
  ;         (for-each loop (syntax->list stx))]
  ;        [exp
  ;         (set! exps (+ exps 1))]))
  ;    (eprintf "exps: ~v\nwcms: ~v\n" exps wcms))
  ;  stx)
  
  ;; make-debug-eval-handler : (sexp -> value) -> sexp -> value
  ;; adds debugging information to `sexp' and calls `oe'
  (define (make-debug-eval-handler oe)
    (let ([debug-tool-eval-handler
           (λ (orig-exp)
             (if (compiled-expression? (if (syntax? orig-exp)  
                                           (syntax-e orig-exp)  
                                           orig-exp))
                 (oe orig-exp)
                 (let loop ([exp (if (syntax? orig-exp)
                                     orig-exp
                                     (namespace-syntax-introduce
                                      (datum->syntax #f orig-exp)))])
                   (let ([top-e (expand-syntax-to-top-form exp)]) 
                     (syntax-case top-e (begin) 
                       [(begin expr ...)
                        ;; Found a `begin', so expand/eval each contained 
                        ;; expression one at a time 
                        (let i-loop ([exprs (syntax->list #'(expr ...))]
                                     [last-one (list (void))])
                          (cond
                            [(null? exprs) 
                             (apply values last-one)]
                            [else 
                             (i-loop (cdr exprs)
                                     (call-with-values 
                                      (λ () 
                                        (call-with-continuation-prompt
                                         (λ () (loop (car exprs)))
                                         (default-continuation-prompt-tag)
                                         (λ args
                                           (apply
                                            abort-current-continuation 
                                            (default-continuation-prompt-tag)
                                            args))))
                                      list))]))]
                       [_else 
                        ;; Not `begin', so proceed with normal expand and eval 
                        (let* ([annotated (annotate-top (expand-syntax top-e)
                                                        (namespace-base-phase))])
                          (oe annotated))])))))])
      debug-tool-eval-handler))
  
  ;; make-debug-error-display-handler : (string (union TST exn) -> void) -> string (union TST exn) -> void
  ;; adds in the bug icon, if there are contexts to display
  (define (make-debug-error-display-handler orig-error-display-handler)
    (define (debug-error-display-handler msg exn)
      (let ([rep (drracket:rep:current-rep)])
        (cond
          [rep
           (error-display-handler/stacktrace 
            msg
            exn 
            (and (exn? exn) 
                 (continuation-mark-set? (exn-continuation-marks exn))
                 (cms->srclocs (exn-continuation-marks exn))))]
          [else 
           (orig-error-display-handler msg exn)])))
    debug-error-display-handler)
  
  ;; error-display-handler/stacktrace : string any (listof srcloc) -> void
  ;; =User=
  (define (error-display-handler/stacktrace 
           msg exn 
           [pre-stack #f]
           #:interactions-text [ints (drracket:rep:current-rep)]
           #:definitions-text [defs (let ([rep (drracket:rep:current-rep)])
                                      (and rep
                                           (send rep get-definitions-text)))])
    (let* ([stack1 (or pre-stack '())]
           [stack2 (if (exn? exn)
                       (map cdr (filter cdr (cut-stack-at-checkpoint exn)))
                       '())]
           [port-name-matches-cache (make-hasheq)]
           [stack1-editions (map (λ (x) (srcloc->edition/pair defs ints x port-name-matches-cache)) stack1)]
           [stack2-editions (map (λ (x) (srcloc->edition/pair defs ints x port-name-matches-cache)) stack2)]
           [src-locs (cond
                       [(exn:srclocs? exn)
                        ((exn:srclocs-accessor exn) exn)]
                       [(pick-first-defs port-name-matches-cache defs stack1) => list]
                       [(pick-first-defs port-name-matches-cache defs stack2) => list]
                       [(pair? stack1)
                        (list (car stack1))]
                       [(pair? stack2)
                        (list (car stack2))]
                       [else '()])]
           [src-locs-edition (and (pair? src-locs)
                                  (srcloc->edition/pair defs ints (car src-locs) port-name-matches-cache))])

      (print-planet-icon-to-stderr exn)
      (unless (exn:fail:user? exn)
        (unless (exn:fail:syntax? exn)
          (unless (and (null? stack1) (null? stack2))
            (print-bug-to-stderr msg stack1 stack1-editions stack2 stack2-editions defs ints)))
        (display-srclocs-in-error src-locs src-locs-edition))
      (display msg (current-error-port))
      (when (exn:fail:syntax? exn)
        (unless (error-print-source-location)
          (show-syntax-error-context (current-error-port) exn)))
      (print-pkg-icon-to-stderr exn)
      (newline (current-error-port))
      (flush-output (current-error-port))
      (when (and ints
                 (eq? (current-error-port) 
                      (send ints get-err-port)))
        (parameterize ([current-eventspace drracket:init:system-eventspace])
          (queue-callback
           (λ ()
             ;; need to make sure that the user's eventspace is still the same
             ;; and still running here?
             (send ints highlight-errors src-locs (if (null? stack1)
                                                      stack2
                                                      stack1))))))))
  
  (define (srcloc->edition/pair defs ints srcloc [port-name-matches-cache #f])
    (let ([src (srcloc-source srcloc)])
      (cond
        [(and (or (symbol? src)
                  (path? src))
              ints
              (port-name-matches?/use-cache ints src port-name-matches-cache))
         (cons (make-weak-box ints) (send ints get-edition-number))]
        [(and (or (symbol? src)
                  (path? src))
              defs
              (port-name-matches?/use-cache defs src port-name-matches-cache))
         (cons (make-weak-box defs) (send defs get-edition-number))]
        [(path? src)
         (let ([frame (send (group:get-the-frame-group) locate-file src)])
           (and frame
                (is-a? frame drracket:unit:frame<%>)
                (cons (make-weak-box (send frame get-definitions-text))
                      (send (send frame get-definitions-text) get-edition-number))))]
        [else #f])))
  
  (define (pick-first-defs port-name-matches-cache defs stack)
    (for/or ([srcloc (in-list stack)])
      (and (srcloc? srcloc)
           (port-name-matches?/use-cache defs (srcloc-source srcloc) port-name-matches-cache)
           srcloc)))
  
  (define (port-name-matches?/use-cache txt src port-name-matches-cache)
    (if port-name-matches-cache
        (hash-ref! port-name-matches-cache (cons txt src) (λ () (send txt port-name-matches? src)))
        (send txt port-name-matches? src)))
  
  ;; =User=
  (define (print-planet-icon-to-stderr exn)
    (when (exn:fail:contract:blame? exn)
      (let ([table (parse-gp exn
                             (blame-positive
                              (exn:fail:contract:blame-object exn)))])
        (when table
          (let ([gp-url (bug-info->ticket-url table)])
            (when planet-note%
              (when (port-writes-special? (current-error-port))
                (let ([note (new planet-note%)])
                  (send note set-callback (λ (snp) 
                                            ;; =Kernel= =Handler=
                                            (drracket:unit:forget-saved-bug-report table)
                                            (send-url (url->string gp-url))))
                  (parameterize ([current-eventspace drracket:init:system-eventspace])
                    (queue-callback
                     (λ ()
                       (drracket:unit:record-saved-bug-report table))))
                  (write-special note (current-error-port))
                  (display #\space (current-error-port))))))))))
  
  
  ;; =Kernel= =User=
  (define (bug-info->ticket-url table)
    (make-url 
     "http"
     #f
     "planet.racket-lang.org"
     #f
     #t
     (list (make-path/param "trac" '())
           (make-path/param "newticket" '()))
     table
     #f))
  
  ;; =User=
  (define (parse-gp exn gp)
    (match gp
      [`(planet ,fn (,user ,package ,planet-version ...))
       (list (cons 'component (format "~a/~a" user package))
             (cons 'keywords "contract violation")
             (cons 'pltversion (version))
             (cons 'planetversion
                   (cond
                     [(null? planet-version) ""]
                     [(null? (cdr planet-version))
                      (format "~s" `(,(car planet-version) ?))]
                     [else
                      (format "~s" `(,(car planet-version) ,(cadr planet-version)))]))
             (cons 'description (exn->trace exn)))]
      [else #f]))
  
  ;; =User=
  (define (print-pkg-icon-to-stderr exn)
    (when (exn:missing-module? exn)
      (define mod ((exn:missing-module-accessor exn) exn))
      (define pkgs (pkg-catalog-suggestions-for-module mod))
      (define update-pkgs-node (new clickable-string-snip% [str "[update catalog]"]))
      (define (get-tlw snp)
        (define admin (send snp get-admin))
        (define canvas (and admin (send (send admin get-editor) get-canvas)))
        (and canvas (send canvas get-top-level-window)))
      (send update-pkgs-node set-callback 
            (λ (snp)
              (pkg-catalog-update-local/simple-status-dialog
               #:parent (get-tlw snp))))
      (cond
        [(null? pkgs)
         (when (port-writes-special? (current-error-port))
           (display "\n  no packages suggestions are available " (current-error-port))
           (write-special update-pkgs-node (current-error-port)))]
        [else
         (display "\n  packages that provide the missing module:" (current-error-port))
         (when (port-writes-special? (current-error-port))
           (display " " (current-error-port))
           (write-special update-pkgs-node (current-error-port)))
         (for ([pkg (in-list pkgs)])
           (eprintf "\n    ~a" pkg)
           (when (port-writes-special? (current-error-port))
             (define note (new clickable-string-snip% [str "[install]"]))
             (send note set-callback 
                   (λ (snp) 
                     ;; =Kernel= =Handler=
                     (define tlw (get-tlw snp))
                     (install-pkg
                      tlw
                      (lambda (thunk)
                        (parameterize ([error-display-handler drracket:init:original-error-display-handler])
                          (thunk)))
                      #:package-to-offer pkg)))
             (eprintf "  ")
             (write-special note (current-error-port))))])))
  
  ;; =User=
  (define (exn->trace exn)
    (let ([sp (open-output-string)])
      (parameterize ([current-error-port sp])
        (drracket:init:original-error-display-handler (exn-message exn) exn))
      (get-output-string sp)))
  
  ;; =User=
  (define (print-bug-to-stderr msg cms1 editions1 cms2 editions2 defs ints)
    (when (port-writes-special? (current-error-port))
      (let ([note% (if (mf-bday?) mf-note% bug-note%)])
        (when note%
          (let ([note (new note%)])
            (send note set-stacks cms1 cms2)
            (send note set-callback (λ (snp) (show-backtrace-window/edition-pairs/two msg cms1 editions1 cms2 editions2 defs ints)))
            (write-special note (current-error-port))
            (display #\space (current-error-port)))))))
  
  ;; display-srclocs-in-error : (listof src-loc) -> void
  ;; prints out the src location information for src-to-display
  ;; as it would appear in an error message
  (define (display-srclocs-in-error srcs-to-display edition-pair)
    (unless (null? srcs-to-display)
      (let ([src-to-display (car srcs-to-display)])
        (let* ([src (srcloc-source src-to-display)]
               [line (srcloc-line src-to-display)]
               [col (srcloc-column src-to-display)]
               [pos (srcloc-position src-to-display)]
               [do-icon
                (λ ()
                  (when file-note%
                    (when (port-writes-special? (current-error-port))
                      (let ([note (new file-note%)])
                        (send note set-callback 
                              (λ (snp) (open-and-highlight-in-file srcs-to-display edition-pair)))
                        (write-special note (current-error-port))
                        (display #\space (current-error-port))))))]
               [do-src
                (λ ()
                  (cond
                    [(path? src)
                     (define-values (n-cd n-src)
                       (with-handlers ([exn:fail? (λ (x) (values (current-directory) src))])
                         (values (normalize-path (current-directory)) (normalize-path src))))
                     (display (path->string (find-relative-path n-cd n-src))
                              (current-error-port))]
                    [else
                     (display "<unsaved editor>" (current-error-port))]))]
               [do-line/col (λ () (eprintf ":~a:~a" line col))]
               [do-pos (λ () (eprintf "::~a" pos))]
               [src-loc-in-defs/ints?
                (let ([rep (drracket:rep:current-rep)])
                  (and rep
                       (is-a? rep drracket:rep:text<%>)
                       (let ([defs (send rep get-definitions-text)])
                         (or (send rep port-name-matches? src)
                             (eq? rep src)
                             (send defs port-name-matches? src)
                             (eq? defs src)))))])
          (cond
            [(and src line col)
             (do-icon)
             (unless src-loc-in-defs/ints?
               (do-src)
               (do-line/col)
               (display ": " (current-error-port)))]
            [(and src pos)
             (do-icon)
             (unless src-loc-in-defs/ints?
               (do-src)
               (do-pos)
               (display ": " (current-error-port)))])))))
  
  ;; find-src-to-display : exn (union #f (listof srcloc))
  ;;                    -> (listof srclocs)
  ;; finds the source location to display, choosing between
  ;; the stack trace and the exception record.
  (define (find-src-to-display exn cms)
    (let ([has-info?
           (λ (srcloc)
             (ormap (λ (f) (f srcloc))
                    (list srcloc-column
                          srcloc-line
                          srcloc-position
                          srcloc-source
                          #;srcloc-span)))])  ;; don't consider span alone to count as `info'
      (cond
        [(and (exn:srclocs? exn)
              (ormap has-info? ((exn:srclocs-accessor exn) exn)))
         ((exn:srclocs-accessor exn) exn)]
        [(pair? cms) (list (car cms))]
        [else '()])))
  
  ;; show-syntax-error-context : 
  ;; display the source information associated with a syntax error (if present)
  (define (show-syntax-error-context port exn)
    (let ([error-text-style-delta (make-object style-delta%)]
          [send-out
           (λ (msg f) 
             (if (port-writes-special? (current-error-port))
                 (let ([snp (make-object string-snip% msg)])
                   (f snp)
                   (write-special snp (current-error-port)))
                 (display msg (current-error-port))))])
      (send error-text-style-delta set-delta-foreground (make-object color% 200 0 0))
      (define (show-one expr)
        (display " " (current-error-port))
        (send-out (format "~s" (syntax->datum expr))
                  (λ (snp)
                    (send snp set-style
                          (send (editor:get-standard-style-list) find-or-create-style
                                (send (editor:get-standard-style-list) find-named-style "Standard")
                                error-text-style-delta)))))
      (define exprs (exn:fail:syntax-exprs exn))
      (define (show-in)
        (send-out " in:"
                  (λ (snp)
                    (send snp set-style
                          (send (editor:get-standard-style-list) find-named-style "Standard")))))
      (cond
        [(null? exprs) (void)]
        [(null? (cdr exprs))
         (show-in)
         (show-one (car exprs))]
        [else
         (show-in)
         (for-each (λ (expr)
                     (display "\n " (current-error-port))
                     (show-one expr))
                   exprs)])))
  
  
  ;; insert/clickback : (instanceof text%) (union string (instanceof snip%)) (-> void)
  ;; inserts `note' and a space at the end of `rep'
  ;; also sets a clickback on the inserted `note' (but not the space).
  (define (insert/clickback rep note clickback)
    (let ([before (send rep last-position)])
      (send rep insert (if (string? note)
                           note
                           (send note copy))
            before before)
      (let ([after (send rep last-position)])
        (send rep insert #\space after after)
        (send rep set-clickback before after
              (λ (txt start end)
                (clickback))))))
  
  ;; with-mark : mark-stx syntax (any? -> syntax) -> syntax
  ;; a member of stacktrace-imports^
  ;; This uses the following format for continuation marks:
  ;;    (cons Dummy srcloc-list)
  ;; where Dummy is always ignored.
  
  ;; Note that this is not necessarily the same format used by `make-st-mark`
  ;; which is unspecified.
  (define (with-mark src-stx expr phase)
    (let ([source (cond
                    [(path? (syntax-source src-stx))
                     (syntax-source src-stx)]
                    [(is-a? (syntax-source src-stx) editor<%>)
                     (syntax-source src-stx)]
                    [else 
                     (let* ([rep (drracket:rep:current-rep)])
                       (and
                        rep
                        (let ([defs (send rep get-definitions-text)])
                          (cond
                            [(send rep port-name-matches? (syntax-source src-stx))
                             rep]
                            [(send defs port-name-matches? (syntax-source src-stx))
                             defs]
                            [else #f]))))])]
          [position (or (syntax-position src-stx) 0)]
          [span (or (syntax-span src-stx) 0)]
          [line (or (syntax-line src-stx) 0)]
          [column (or (syntax-column src-stx) 0)])
      (if source
          (with-syntax ([expr expr]
                        [mark (list 'dummy-thing source line column position span)]
                        [wcm (syntax-shift-phase-level #'with-continuation-mark phase)]
                        [errortrace-key errortrace-key] ; a symbol
                        [qte (syntax-shift-phase-level #'quote phase)])
            (syntax
             (wcm (qte errortrace-key)
                  (qte mark)
                  expr)))
          expr)))
  
  ;; current-backtrace-window : (union #f (instanceof frame:basic<%>))
  ;; the currently visible backtrace window, or #f, if none
  (define current-backtrace-window #f)
  
  ;; reset-backtrace-window : -> void
  ;; effect: updates current-backtrace-window
  ;; closes the current backtrace window and creates a new (unshown) one
  (define (reset-backtrace-window)
    (when current-backtrace-window
      (send current-backtrace-window close)
      (set! current-backtrace-window #f))
    
    (set! current-backtrace-window 
          (make-object backtrace-frame%
            (string-constant backtrace-window-title)
            #f
            (preferences:get 'drracket:backtrace-window-width)
            (preferences:get 'drracket:backtrace-window-height)
            (preferences:get 'drracket:backtrace-window-x)
            (preferences:get 'drracket:backtrace-window-y))))
  
  ;; hide-backtrace-window : -> void
  (define (hide-backtrace-window)
    (when current-backtrace-window
      (send current-backtrace-window close)
      (set! current-backtrace-window #f)))
  
  ;; backtrace-frame% : (extends frame:basic<%>)
  (define backtrace-frame%
    (class (drracket:frame:basics-mixin (frame:standard-menus-mixin frame:basic%))
      (define/override (on-size x y)
        (preferences:set 'drracket:backtrace-window-width x)
        (preferences:set 'drracket:backtrace-window-height y)
        (super on-size x y))
      (define/override (on-move x y)
        (preferences:set 'drracket:backtrace-window-x x)
        (preferences:set 'drracket:backtrace-window-y y)
        (super on-move x y))
      (define/override (edit-menu:between-find-and-preferences edit-menu) (void))
      (define/override (edit-menu:between-select-all-and-find edit-menu) (void))
      (define/override (file-menu:between-save-as-and-print file-menu) (void))
      (define/augment (on-close) 
        (set! current-backtrace-window #f)
        (inner (void) on-close))
      (super-new)))
  
  ;; show-backtrace-window : string
  ;;                         (listof srcloc?)
  ;;                         -> 
  ;;                         void
  (define (show-backtrace-window error-text dis/exn [rep #f] [defs #f])
    (let ([dis (if (exn? dis/exn)
                   (cms->srclocs (exn-continuation-marks dis/exn))
                   dis/exn)])
      (show-backtrace-window/edition-pairs error-text dis (map (λ (x) #f) dis) defs rep)))
  
  (define (show-backtrace-window/edition-pairs error-text dis editions defs ints)
    (show-backtrace-window/edition-pairs/two error-text dis editions '() '() defs ints))
  
  (define (show-backtrace-window/edition-pairs/two error-text dis1 editions1 dis2 editions2 defs ints)
    (reset-backtrace-window)
    (when (mf-bday?)
      (new message%
           [label (string-constant happy-birthday-matthias)]
           [parent (send current-backtrace-window get-area-container)]))
    (define tab-panel 
      (if (and (pair? dis1) (pair? dis2))
          (new tab-panel% 
               [choices (list "Errortrace" "Builtin")]
               [parent (send current-backtrace-window get-area-container)]
               [callback
                (λ (a b) 
                  (send tab-panel change-children
                        (λ (l) (if (zero? (send tab-panel get-selection))
                                   (list ec1)
                                   (list ec2)))))])
          (new vertical-panel% [parent (send current-backtrace-window get-area-container)])))
    (define ec1 (add-ec/text dis1 editions1 defs ints tab-panel error-text))
    (define ec2 (add-ec/text dis2 editions2 defs ints tab-panel error-text))
    (when (and (pair? dis1) (pair? dis2))
      (send tab-panel change-children (λ (l) (list ec1)))))
  
  (define (add-ec/text dis1 editions1 defs ints tab-panel error-text)
    (cond
      [(pair? dis1)
       (define text1 (new (text:wide-snip-mixin text:hide-caret/selection%)))
       (define ec1 (new (canvas:color-mixin canvas:wide-snip%)
                        [parent tab-panel]
                        [editor text1]))
       (add-one-set-to-frame text1 ec1 error-text dis1 editions1 defs ints)
       ec1]
      [else #f]))
  
  (define (add-one-set-to-frame text ec error-text dups-dis dups-editions defs ints)
    (define-values (dis editions skips) (remove-adjacent-duplicates dups-dis dups-editions))
    (letrec ([di-vec (list->vector dis)]
             [editions-vec (list->vector editions)]
             [skip-counts (list->vector skips)]
             [index 0]
             [how-many-at-once 15]
             [show-next-dis
              (λ ()
                (let ([start-pos (send text get-start-position)]
                      [end-pos (send text get-end-position)])
                  (send text begin-edit-sequence)
                  (send text set-position (send text last-position))
                  (let loop ([n index])
                    (cond
                      [(and (< n (vector-length di-vec))
                            (< n (+ index how-many-at-once)))
                       (show-frame ec text (vector-ref di-vec n) (vector-ref editions-vec n) (vector-ref skip-counts n) defs ints)
                       (loop (+ n 1))]
                      [else
                       (set! index n)]))
                  
                  ;; add 'more frames' link
                  (when (< index (vector-length di-vec))
                    (let ([end-of-current (send text last-position)])
                      (send text insert #\newline)
                      (let ([hyper-start (send text last-position)])
                        (send text insert 
                              (let* ([num-left
                                      (- (vector-length di-vec)
                                         index)]
                                     [num-to-show
                                      (min how-many-at-once
                                           num-left)])
                                (if (= num-left 1)
                                    (string-constant last-stack-frame)
                                    (format (if (num-left . <= . num-to-show)
                                                (string-constant last-stack-frames)
                                                (string-constant next-stack-frames))
                                            num-to-show))))
                        (let ([hyper-end (send text last-position)])
                          (send text change-style (gui-utils:get-clickback-delta
                                                   (preferences:get 'framework:white-on-black?))
                                hyper-start hyper-end)
                          (send text set-clickback
                                hyper-start hyper-end
                                (λ x
                                  (send text begin-edit-sequence)
                                  (send text lock #f)
                                  (send text delete end-of-current (send text last-position))
                                  (show-next-dis)
                                  (send text set-position 
                                        (send text last-position)
                                        (send text last-position))
                                  (send text lock #t)
                                  (send text end-edit-sequence)))
                          
                          (send text insert #\newline)
                          (send text set-paragraph-alignment (send text last-paragraph) 'center)))))
                  
                  (send text set-position start-pos end-pos)
                  (send text end-edit-sequence)))])
      (send current-backtrace-window set-alignment 'center 'center)
      (send current-backtrace-window reflow-container)
      (send text auto-wrap #t)
      (send text set-autowrap-bitmap #f)
      (send text insert error-text)
      (send text insert "\n\n")
      (send text change-style error-delta 0 (- (send text last-position) 1))
      (show-next-dis)
      (send text set-position 0 0)
      (send text lock #t)
      (send text hide-caret #t)
      (send current-backtrace-window show #t)))
  
  (define (remove-adjacent-duplicates dis editions)
    (cond
      [(null? dis) (values '() '() '())]
      [else
       (let loop ([di (car dis)]
                  [edition (car editions)]
                  [dis (cdr dis)]
                  [editions (cdr editions)])
         (cond
           [(null? dis) (values (list di) (list edition) (list 0))]
           [else
            (define di2 (car dis))
            (define edition2 (car editions))
            (define-values (res-dis res-editions skip-counts) (loop di2 edition2 (cdr dis) (cdr editions)))
            (if (equal? di di2)
                (values res-dis res-editions (cons (+ (car skip-counts) 1) (cdr skip-counts)))
                (values (cons di res-dis)
                        (cons edition res-editions)
                        (cons 0 skip-counts)))]))]))
  
  (let ()
    (define (check dis-in editions-in dis-expected editions-expected skip-expected)
      (define-values (dis-got editions-got skip-got) (remove-adjacent-duplicates dis-in editions-in))
      (unless (and (equal? dis-got dis-expected)
                   (equal? editions-got editions-expected)
                   (equal? skip-got skip-expected))
        (eprintf "~s =\n  ~s, but expected\n  ~s\n\n"
                 `(remove-adjacent-duplicates ',dis-in ',editions-in)
                 `(values ',dis-got ',editions-got ',skip-got)
                 `(values ',dis-expected ',editions-expected ',skip-expected))))
    (check '() '() '() '() '())
    (check '(1) '(2) '(1) '(2) '(0))
    (check '(1 2 3) '(4 5 6) '(1 2 3) '(4 5 6) '(0 0 0))
    (check '(1 1) '(2 3) '(1) '(3) '(1))
    (check '(1 2) '(3 3) '(1 2) '(3 3) '(0 0))
    (check '(1 2 2 3 4 4 3) '(a b c d e f g)
           '(1 2 3 4 3) '(a c d f g) '(0 1 0 1 0))
    (check '(1 2 2 2 2 2 3) '(a b c d e f g)
           '(1 2 3) '(a f g) '(0 4 0)))
    
                 
  
  ;; show-frame : (instanceof editor-canvas%)
  ;;              (instanceof text%) 
  ;;              st-mark   // see format description at `with-mark`
  ;;              def ints  // definitions and interactions texts
  ;;              -> 
  ;;              void 
  ;; shows one frame of the continuation
  (define (show-frame editor-canvas text di edition skip-count defs ints)
    (let* ([debug-source (srcloc-source di)]
           [fn (get-filename debug-source)]
           [line (srcloc-line di)]
           [column (srcloc-column di)]
           [start (srcloc-position di)]
           [span (srcloc-span di)]
           [start-pos (send text last-position)])
      
      ;; make hyper link to the file
      (send text insert (format "~a: ~a:~a" fn line column))
      (let ([end-pos (send text last-position)])
        (send text insert " ")
        (send text change-style 
              (gui-utils:get-clickback-delta (preferences:get 'framework:white-on-black?))
              start-pos 
              end-pos)
        (send text set-clickback
              start-pos end-pos
              (λ (ed start end)
                (open-and-highlight-in-file (list di) edition))))
      
      (unless (zero? skip-count)
        (send text insert " skipped ")
        (send text insert (number->string skip-count))
        (send text insert " duplicate frame")
        (unless (= skip-count 1)
          (send text insert "s"))
        (send text insert #\newline))
      
      (when (and start span)
        (insert-context editor-canvas text debug-source start span defs ints)
        (send text insert #\newline))))
  
  ;; insert-context : (instanceof editor-canvas%)
  ;;                  (instanceof text%)
  ;;                  debug-info
  ;;                  number
  ;;                  defs ints // definitions and interactions texts
  ;;                  -> 
  ;;                  void
  (define (insert-context editor-canvas text file start span defs ints)
    (let-values ([(from-text close-text)
                  (cond
                    [(and ints (send ints port-name-matches? file))
                     (values ints void)]
                    [(and defs (send defs port-name-matches? file))
                     (values defs void)]
                    [(path? file)
                     (let ([file (with-handlers ((exn:fail? (λ (x) #f)))
                                   (normal-case-path (normalize-path file)))])
                       (if file
                           (cond
                             [(send (group:get-the-frame-group)
                                    locate-file
                                    file)
                              =>
                              (λ (frame)
                                (cond
                                  [(is-a? frame drracket:unit:frame%)
                                   (let loop ([tabs (send frame get-tabs)])
                                     (cond
                                       [(null? tabs) (values #f void)]
                                       [else
                                        (let* ([tab (car tabs)]
                                               [defs (send tab get-defs)])
                                          (if (with-handlers ((exn:fail? (λ (x) #f)))
                                                (equal? (normalize-path (normal-case-path (send defs get-filename)))
                                                        file))
                                              (values defs void)
                                              (loop (cdr tabs))))]))]
                                  [(is-a? frame frame:editor<%>)
                                   (values (send frame get-editor) void)]
                                  [else (values #f void)]))]
                             [(path? file)
                              (let ([text (new text:basic%)])
                                (if (send text load-file file)
                                    (values text 
                                            (λ () (send text on-close)))
                                    (values #f (λ () (void)))))]
                             [else
                              (values #f void)])
                           (values #f void)))]
                    [(is-a? file editor<%>)
                     (values file void)]
                    [else (error 'insert-context "unknown file spec ~e" file)])])
      (when from-text
        (let* ([finish (+ start span -1)]
               [context-text (copy/highlight-text from-text start finish)])
          (send context-text lock #t)
          (send context-text hide-caret #t)
          (send text insert "  ")
          (let ([snip (make-object editor-snip% context-text)])
            (send snip use-style-background #t)
            (send editor-canvas add-wide-snip snip)
            (let ([p (send text last-position)])
              (send text insert snip p p)
              (send text insert #\newline)
              (when (preferences:get 'framework:white-on-black?)
                (send text change-style white-on-black-style p (+ p 1))))))
        (close-text))))
  
  (define white-on-black-style (make-object style-delta%))
  (define stupid-internal-define-syntax1 (send white-on-black-style set-delta-foreground "white"))
  
  ;; copy/highlight-text : text number number -> text
  ;; copies the range from `start' to `finish', including the entire paragraph at
  ;; each end and highlights the characters corresponding the original range,
  ;; in the resulting text
  (define (copy/highlight-text from-text start finish)
    (let* ([to-text (new text:standard-style-list%)]
           [para-start-pos (send from-text paragraph-start-position 
                                 (send from-text position-paragraph start))]
           [para-end-pos (send from-text paragraph-end-position
                               (send from-text position-paragraph 
                                     finish))]
           [from-start (- start para-start-pos)]
           [from-end (+ from-start (- finish start))])
      (send from-text split-snip para-start-pos)
      (send from-text split-snip para-end-pos)
      (let loop ([snip (send from-text find-snip para-start-pos 'after-or-none)])
        (when (and snip
                   (< (send from-text get-snip-position snip) para-end-pos))
          (send to-text insert (send snip copy))
          (loop (send snip next))))
      (send to-text highlight-range (max 0 (- from-start 1)) from-end (get-error-color) #f 'high)
      to-text))
  
  ;; get-filename : debug-source -> string
  (define (get-filename file)
    (cond
      [(symbol? file) (symbol->string file)]
      [(path? file) (path->string file)]
      [(is-a? file editor<%>)
       (get-filename-from-editor file)]))
  
  ;; get-filename-from-editor : (is-a?/c editor<%>) -> string
  (define (get-filename-from-editor editor)
    (let* ([untitled (string-constant unknown-debug-frame)]
           [canvas (send editor get-canvas)]
           [frame (and canvas (send canvas get-top-level-window))])
      (if (is-a? frame drracket:unit:frame%)
          (let ([filename (send (send frame get-definitions-text) get-filename)])
            (cond
              [(and filename (eq? editor (send frame get-interactions-text)))
               (format (string-constant files-interactions) filename)]
              [(eq? editor (send frame get-interactions-text))
               (string-constant current-interactions)]
              [filename filename]
              [else (string-constant current-definitions)]))
          (or (send editor get-filename) 
              untitled))))
  
  ;; open-and-highlight-in-file : (or/c srcloc (listof srcloc)) -> void
  (define (open-and-highlight-in-file raw-srcloc [edition-pair #f])
    (let* ([srclocs (if (srcloc? raw-srcloc) (list raw-srcloc) raw-srcloc)]
           [sources (filter values (map srcloc-source srclocs))])
      (unless (null? sources)
        (let* ([debug-source (car sources)]
               [same-src-srclocs
                (filter (λ (x) (eq? debug-source (srcloc-source x)))
                        srclocs)]
               [frame (cond
                        [(path? debug-source) (handler:edit-file debug-source)]
                        [(and (symbol? debug-source)
                              (text:lookup-port-name debug-source))
                         =>
                         (lambda (editor)
                           (get-enclosing-editor-frame editor))]
                        [else #f])]
               [editor (cond
                         [(path? debug-source)
                          (cond
                            [(and frame (is-a? frame drracket:unit:frame%))
                             (send frame get-definitions-text)]
                            [(and frame (is-a? frame frame:editor<%>))
                             (send frame get-editor)]
                            [else #f])]
                         [(and (symbol? debug-source)
                               (text:lookup-port-name debug-source))
                          =>
                          values]
                         [else #f])]
               [rep (and (is-a? frame drracket:unit:frame%)
                         (send frame get-interactions-text))])
          (when frame
            (send frame show #t))
          (when (and edition-pair
                     (let ([wbv (weak-box-value (car edition-pair))])
                       (and wbv (eq? editor wbv))))
            (unless (= (cdr edition-pair) (send editor get-edition-number))
              (message-box (string-constant drscheme)
                           (string-constant editor-changed-since-srcloc-recorded)
                           frame
                           '(ok caution)
                           #:dialog-mixin frame:focus-table-mixin)))
          (when (and rep editor)
            (when (is-a? editor text:basic<%>)
              (send rep highlight-errors same-src-srclocs '())
              (send editor set-caret-owner #f 'global)))))))
  
  
  
  
  ;                                                                                      
  ;                                                                                      
  ;                                                                                      
  ;                                                                                      
  ;                                                                                      
  ;   ;                  ;                                                               
  ;  ;;;;   ;;;    ;;;  ;;;;       ;;;    ;;;   ;     ;  ;;;   ; ;  ;;;     ;; ;    ;;;  
  ;   ;    ;   ;  ;      ;        ;   ;  ;   ;   ;   ;  ;   ;  ;;  ;   ;   ;  ;;   ;   ; 
  ;   ;   ;    ;  ;;     ;       ;      ;     ;  ;   ; ;    ;  ;       ;  ;    ;  ;    ; 
  ;   ;   ;;;;;;   ;;    ;       ;      ;     ;   ; ;  ;;;;;;  ;    ;;;;  ;    ;  ;;;;;; 
  ;   ;   ;          ;   ;       ;      ;     ;   ; ;  ;       ;   ;   ;  ;    ;  ;      
  ;   ;    ;         ;   ;        ;   ;  ;   ;     ;    ;      ;   ;   ;   ;  ;;   ;     
  ;    ;;   ;;;;  ;;;     ;;       ;;;    ;;;      ;     ;;;;  ;    ;;;;;   ;; ;    ;;;; 
  ;                                                                            ;         
  ;                                                                       ;    ;         
  ;                                                                        ;;;;          
  
  
  (define test-coverage-enabled (make-parameter #f))
  
  (define current-test-coverage-info (make-thread-cell #f))
  
  (define (initialize-test-coverage-point expr)
    (unless (hash? (thread-cell-ref current-test-coverage-info))
      (let ([rep (drracket:rep:current-rep)])
        (when rep
          (let ([ut (eventspace-handler-thread (send rep get-user-eventspace))])
            (when (eq? ut (current-thread))
              (let ([ht (make-hasheq)])
                (thread-cell-set! current-test-coverage-info ht)
                (send rep set-test-coverage-info ht)))))))
    (let ([ht (thread-cell-ref current-test-coverage-info)])
      (when (hash? ht)
        ;; if rep isn't around, we don't do test coverage...
        ;; this can happen when check syntax expands, for example
        (hash-set! ht expr #;(box #f) (mcons #f #f)))))
  
  (define (test-covered expr)
    (let ([ht (thread-cell-ref current-test-coverage-info)])
      (and (hash? ht) ;; as in the `when' test in `initialize-test-coverage-point'
           (let ([v (hash-ref ht expr #f)])
             ;; (and v (λ () (set-box! v #t)))
             (and v (with-syntax ([v v]) #'(#%plain-app set-mcar! v #t)))))))
  
  (define test-coverage-interactions-text<%>
    (interface ()
      set-test-coverage-info
      get-test-coverage-info))
  
  (define test-coverage-tab<%>
    (interface ()
      show-test-coverage-annotations ;; hash-table (union #f style) (union #f style) boolean -> void
      get-test-coverage-info-visible?
      ask-about-clearing-test-coverage?))
  
  (define test-coverage-interactions-text-mixin
    (mixin (drracket:rep:text<%> text:basic<%>) (test-coverage-interactions-text<%>)
      (inherit get-context)
      (field [test-coverage-info #f]
             [test-coverage-on-style #f]
             [test-coverage-off-style #f]
             [ask-about-reset? #f])
      (define/public set-test-coverage-info
        (λ (ht [on-style #f] [off-style #f] [ask? #t])
          (set! test-coverage-info ht)
          (set! test-coverage-on-style on-style)
          (set! test-coverage-off-style off-style)
          (set! ask-about-reset? ask?)))
      (define/public (get-test-coverage-info) 
        test-coverage-info)
      
      (inherit get-top-level-window)
      (define/augment (after-many-evals)
        (when test-coverage-info
          (send (get-context) show-test-coverage-annotations
                test-coverage-info
                test-coverage-on-style
                test-coverage-off-style
                ask-about-reset?))
        (inner (void) after-many-evals))
      
      (super-new)))
  
  (define test-coverage-definitions-text-mixin
    (mixin ((class->interface text%) drracket:unit:definitions-text<%>) ()
      (inherit get-canvas get-tab)
      
      (define/private (clear-test-coverage?)
        (if (preferences:get 'drracket:test-coverage-ask-about-clearing?)
            (let ([msg-box-result
                   (message-box/custom
                    (string-constant drscheme)
                    (string-constant test-coverage-clear?)
                    (string-constant yes)
                    (string-constant no)
                    (string-constant test-coverage-clear-and-do-not-ask-again)
                    (send (get-canvas) get-top-level-window)
                    '(default=1)
                    2
                    #:dialog-mixin frame:focus-table-mixin)])
              (case msg-box-result
                [(1) #t]
                [(2) #f]
                [(3)
                 (preferences:set 'drracket:test-coverage-ask-about-clearing? #f)
                 #t]))
            #t))
      
      (define/public (clear-test-coverage)
        (let ([tab (get-tab)])
          (when (send tab get-test-coverage-info-visible?)
            (send tab clear-test-coverage-display)
            (let ([it (send tab get-ints)])
              (when (is-a? it test-coverage-interactions-text<%>)
                (send it set-test-coverage-info #f))))))
      
      (define/private (can-clear-coverage?)
        (let ([tab (get-tab)])
          (or (not tab)
              (not (send tab get-test-coverage-info-visible?))
              (not (send tab ask-about-clearing-test-coverage?))
              (clear-test-coverage?))))
      
      (define/augment (can-insert? x y)
        (and (inner #t can-insert? x y)
             (can-clear-coverage?)))
      
      (define/augment (can-delete? x y)
        (and (inner #t can-delete? x y)
             (can-clear-coverage?)))
      
      (define/augment (after-insert x y)
        (inner (void) after-insert x y)
        (clear-test-coverage))
      
      (define/augment (after-delete x y)
        (inner (void) after-delete x y)
        (clear-test-coverage))
      
      (super-new)))
  
  (define test-coverage-on-style-name "plt:module-language:test-coverage-on")
  (define test-coverage-off-style-name "plt:module-language:test-coverage-off")
  
  (define erase-test-coverage-style-delta (make-object style-delta% 'change-normal-color))
  (send erase-test-coverage-style-delta set-transparent-text-backing-on #t)
  
  (define test-coverage-tab-mixin
    (mixin (drracket:rep:context<%> drracket:unit:tab<%>) (test-coverage-tab<%>)
      
      (field [internal-clear-test-coverage-display #f])
      
      (define/public (clear-test-coverage-display)
        (when internal-clear-test-coverage-display
          (internal-clear-test-coverage-display)
          (set! internal-clear-test-coverage-display #f)))
      
      (field [ask-about-reset? #t])
      (define/public (ask-about-clearing-test-coverage?) ask-about-reset?)
      
      (define/public (get-test-coverage-info-visible?)
        (not (not internal-clear-test-coverage-display)))
      
      (define/public (show-test-coverage-annotations ht on-style off-style ask?)
        (set! ask-about-reset? ask?)
        (let* ([edit-sequence-ht (make-hasheq)]
               [locked-ht (make-hasheq)]
               [already-frozen-ht (make-hasheq)]
               [actions-ht (make-hash)]
               
               [port-name-cache (make-hasheq)]
               
               ;; can-annotate : (listof (list boolean srcloc))
               ;; boolean is #t => code was run
               ;;            #f => code was not run
               ;; remove those that cannot be annotated
               [can-annotate
                (filter values
                        (hash-map ht
                          (λ (stx covered?)
                            (and (syntax? stx)
                                 (let ([src (syntax-source stx)]
                                       [pos (syntax-position stx)]
                                       [span (syntax-span stx)])
                                   (and pos
                                        span
                                        (hash-ref! port-name-cache src (λ () (send (get-defs) port-name-matches? src)))
                                        (list (mcar covered?)
                                              (make-srcloc (get-defs) #f #f pos span))))))))]
               
               ;; filtered : (listof (list boolean srcloc))
               ;; remove redundant expressions
               [filtered
                (let (;; actions-ht : (list src number number) -> (list boolean syntax)
                      [actions-ht (make-hash)])
                  (for-each
                   (λ (pr)
                     (let* ([on? (list-ref pr 0)]
                            [key (list-ref pr 1)]
                            [old (hash-ref actions-ht key 'nothing)])
                       (cond
                         [(eq? old 'nothing) (hash-set! actions-ht key on?)]
                         [old ;; recorded as executed
                          (void)]
                         [(not old) ;; recorded as unexected
                          (when on?
                            (hash-set! actions-ht key #t))])))
                   can-annotate)
                  (hash-map actions-ht (λ (k v) (list v k))))])
          
          ;; if everything is covered *and* no coloring has been done, do no coloring.
          (unless (and (andmap car filtered)
                       (not (get-test-coverage-info-visible?)))
            
            (let (;; sorted : (listof (list boolean srcloc))
                  ;; sorting predicate:
                  ;;  x < y if
                  ;;    x's span is bigger than y's (ie, do larger expressions first)
                  ;;    unless x and y are the same source location.
                  ;;    in that case, color red first and then green
                  [sorted
                   (sort
                    filtered
                    (λ (x y)
                      (let* ([x-on (list-ref x 0)]
                             [y-on (list-ref y 0)]
                             [x-srcloc (list-ref x 1)]
                             [y-srcloc (list-ref y 1)]
                             [x-pos (srcloc-position x-srcloc)]
                             [y-pos (srcloc-position y-srcloc)]
                             [x-span (srcloc-span x-srcloc)]
                             [y-span (srcloc-span y-srcloc)])
                        (cond
                          [(and (= x-pos y-pos)
                                (= x-span x-span))
                           (or y-on
                               (not x-on))]
                          [else (>= x-span y-span)]))))])
              
              ;; turn on edit-sequences in all editors to be touched by new annotations
              ;; also fill in the edit-sequence-ht
              (for-each
               (λ (pr)
                 (let ([src (srcloc-source (list-ref pr 1))])
                   (hash-ref 
                    edit-sequence-ht
                    src
                    (λ ()
                      (hash-set! edit-sequence-ht src #f)
                      (send src begin-edit-sequence #f #f)
                      (when (send src is-locked?)
                        (hash-set! locked-ht src #t)
                        (send src lock #f))))))
               sorted)
              
              ;; clear out old annotations (and thaw colorers)
              (when internal-clear-test-coverage-display
                (internal-clear-test-coverage-display)
                (set! internal-clear-test-coverage-display #f))
              
              ;; freeze the colorers, but avoid a second freeze (so we can avoid a second thaw)
              (hash-for-each
               edit-sequence-ht
               (λ (src _)
                 (if (send src is-frozen?)
                     (hash-set! already-frozen-ht src #t)
                     (send src freeze-colorer))))
              
              ;; set new annotations
              (for-each
               (λ (pr)
                 (let ([on? (list-ref pr 0)]
                       [srcloc (list-ref pr 1)])
                   (let* ([src (srcloc-source srcloc)]
                          [pos (srcloc-position srcloc)]
                          [span (srcloc-span srcloc)])
                     (send src change-style
                           (if on?
                               (or on-style 
                                   (send (editor:get-standard-style-list)
                                         find-named-style 
                                         test-coverage-on-style-name))
                               (or off-style 
                                   (send (editor:get-standard-style-list)
                                         find-named-style 
                                         test-coverage-off-style-name)))
                           (- pos 1)
                           (+ (- pos 1) span)
                           #f))))
               sorted)
              
              ;; relock editors
              (hash-for-each 
               locked-ht
               (λ (txt _) (send txt lock #t)))
              
              ;; end edit sequences
              (hash-for-each 
               edit-sequence-ht
               (λ (txt _) (send txt end-edit-sequence)))
              
              ;; save thunk to reset these new annotations
              (set! internal-clear-test-coverage-display
                    (λ ()
                      (hash-for-each
                       edit-sequence-ht
                       (λ (txt _) 
                         (send txt begin-edit-sequence #f #f)))
                      (hash-for-each
                       edit-sequence-ht
                       (λ (txt _) 
                         (let ([locked? (send txt is-locked?)])
                           (when locked? (send txt lock #f))
                           (send txt change-style 
                                 erase-test-coverage-style-delta
                                 0
                                 (send txt last-position)
                                 #f)
                           (when locked? (send txt lock #t)))))
                      (hash-for-each
                       edit-sequence-ht
                       (λ (txt _) 
                         (unless (hash-ref already-frozen-ht txt #f)
                           (let ([locked? (send txt is-locked?)])
                             (when locked? (send txt lock #f))
                             (send txt thaw-colorer)
                             (when locked? (send txt lock #t))))
                         (send txt end-edit-sequence)))))))))
      
      (inherit get-defs)
      (define/augment (clear-annotations)
        (inner (void) clear-annotations)
        (send (get-defs) clear-test-coverage))
      
      (super-new)))
  
  
  
  
  
  ;                                                                 
  ;                                                                 
  ;                           ;;;        ;;;                        
  ;                          ;      ;      ;      ;                 
  ;                          ;             ;                        
  ;   ; ;;   ; ;;;   ;;;   ;;;;;; ;;;      ;    ;;;    ; ;;    ;; ; 
  ;   ;;  ;  ;;  ;  ;   ;    ;      ;      ;      ;    ;;  ;  ;  ;; 
  ;   ;   ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;   ; 
  ;   ;   ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;   ; 
  ;   ;   ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;   ; 
  ;   ;;  ;  ;      ;   ;    ;      ;      ;      ;    ;   ;  ;  ;; 
  ;   ; ;;   ;       ;;;     ;      ;      ;      ;    ;   ;   ;; ; 
  ;   ;                                                           ; 
  ;   ;                                                        ;;;  
  ;                                                                 
  
  
  (define profile-key (gensym))
  
  ;; prof-info =
  ;; (make-prof-info
  ;;    boolean ;; protect against nested calls
  ;;    number[number of calls]
  ;;    number[time spent in all calls]
  ;;   (union #f symbol) 
  ;;   expression)
  (define-struct prof-info (nest num time name expr) #:mutable)
  
  ;; copy-prof-info : prof-info -> prof-info
  (define (copy-prof-info prof-info)
    (make-prof-info (prof-info-nest prof-info)
                    (prof-info-num prof-info)
                    (prof-info-time prof-info)
                    (prof-info-name prof-info)
                    (prof-info-expr prof-info)))
  
  ;; any-info? : prof-info -> boolean
  (define (any-info? prof-info)
    (or (not (zero? (prof-info-num prof-info)))
        (not (zero? (prof-info-time prof-info)))))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; profiling runtime support
  
  ;; parameter
  ;; imported into errortrace
  (define profiling-enabled (make-parameter #f))
  
  ;; holds a hash-table for the profiling information
  (define current-profile-info (make-thread-cell #f))
  
  
  ;; initialize-profile-point : sym syntax syntax -> void
  ;; called during compilation to register this point as
  ;; a profile point. 
  ;; =User=
  ;; imported into errortrace
  (define (initialize-profile-point key name expr)
    (unless (thread-cell-ref current-profile-info)
      (let ([rep (drracket:rep:current-rep)])
        (when rep
          (let ([ut (eventspace-handler-thread (send rep get-user-eventspace))])
            (when (eq? ut (current-thread))
              (let ([ht (make-hasheq)])
                (thread-cell-set! current-profile-info ht)
                (send (send rep get-context) add-profile-info ht)))))))
    (let ([profile-info (thread-cell-ref current-profile-info)])
      (when profile-info
        (hash-set! profile-info
                   key 
                   (make-prof-info #f 0 0 (and (syntax? name) (syntax-e name)) expr))))
    (void))
  
  ;; register-profile-start : sym -> (union #f number)
  ;; =User=
  ;; imported into errortrace
  (define (register-profile-start key)
    (let ([ht (thread-cell-ref current-profile-info)])
      (when ht
        (let ([info (hash-ref ht key)])
          (set-prof-info-num! info (+ (prof-info-num info) 1))
          (if (prof-info-nest info)
              #f
              (begin
                (set-prof-info-nest! info #t)
                (current-process-milliseconds)))))))
  
  ;; register-profile-done : sym (union #f number) -> void
  ;; =User=
  ;; imported into errortrace
  (define (register-profile-done key start)
    (when start
      (let ([ht (thread-cell-ref current-profile-info)])
        (when ht
          (let ([info (hash-ref ht key)])
            (set-prof-info-nest! info #f)
            (set-prof-info-time! info
                                 (+ (- (current-process-milliseconds) start)
                                    (prof-info-time info)))))))
    (void))
  
  (define (get-color-value/pref val max-val drracket:profile:low-color drracket:profile:high-color drracket:profile:scale)
    (let* ([adjust
            (case drracket:profile:scale
              [(sqrt) sqrt]
              [(square) (λ (x) (* x x))]
              [(linear) (λ (x) x)])]
           [factor (adjust (if (zero? max-val) 0 (/ val max-val)))]
           [get-rgb-value
            (λ (sel)
              (let ([small (sel drracket:profile:low-color)]
                    [big (sel drracket:profile:high-color)])
                (inexact->exact (floor (+ (* factor (- big small)) small)))))])
      (make-object color% 
        (get-rgb-value (λ (x) (send x red)))
        (get-rgb-value (λ (x) (send x green)))
        (get-rgb-value (λ (x) (send x blue))))))
  
  ;; get-color-value : number number -> (is-a?/c color%)
  ;; returns the profiling color
  ;; for `val' if `max-val' is the largest
  ;; of any profiling amount.
  (define (get-color-value val max-val)
    (get-color-value/pref val 
                          max-val
                          (preferences:get 'drracket:profile:low-color)
                          (preferences:get 'drracket:profile:high-color)
                          (preferences:get 'drracket:profile:scale)))
  
  ;; extract-maximum : (listof prof-info) -> number
  ;; gets the maximum value of the currently preferred profiling info.
  (define (extract-maximum infos)
    (let ([max-value 0]
          [sel (if (eq? (preferences:get 'drracket:profile-how-to-count) 'time)
                   prof-info-time
                   prof-info-num)])
      (for-each
       (λ (val)
         (set! max-value (max max-value (sel val))))
       infos)
      max-value))
  
  ;; profile-definitions-mixin : mixin
  (define profile-definitions-text-mixin
    (mixin ((class->interface text%) drracket:unit:definitions-text<%>) ()
      (inherit get-canvas get-tab)
      
      (define/augment (can-insert? x y)
        (and (inner #t can-insert? x y)
             (can-reset-profile?)))
      
      (define/augment (can-delete? x y)
        (and (inner #t can-delete? x y)
             (can-reset-profile?)))
      
      (define/augment (on-insert x y)
        (inner (void) on-insert x y)
        (do-reset-profile))
      
      (define/augment (on-delete x y)
        (inner (void) on-delete x y)
        (do-reset-profile))
      
      (define/private (can-reset-profile?)
        (let ([canvas (get-canvas)])
          (or (not canvas)
              (let ([frame (send canvas get-top-level-window)])
                (or (not (send frame get-profile-info-visible?))
                    (eq? (message-box (string-constant drscheme)
                                      (string-constant profiling-clear?)
                                      frame
                                      '(yes-no)
                                      #:dialog-mixin frame:focus-table-mixin)
                         'yes))))))
      
      (define/private (do-reset-profile)
        (send (get-tab) reset-profile))
      
      (super-new)))
  
  (define profile-interactions-tab<%>
    (interface ()
      add-profile-info))
  
  (define-local-member-name 
    
    ;; tab methods
    reset-profile  ;; erases profile display & information
    hide-profile  ;; hides profiling info, but it is still here to be shown again
    show-profile  ;; shows the profile info, if there is any to show
    refresh-profile ;; shows current info in profile window
    get-profile-info-text
    can-show-profile?
    get-sort-mode ;; indicates if the results are currently shown sorted by time, or not
    set-sort-mode ;; updates the sort mode flag (only called by the gui control callback)
    
    ;; frame methods
    hide-profile-gui
    show-profile-gui
    
    ;; frame and tab methods
    get-profile-info-visible?
    ; on frame, indicates if the gui stuff shows up currently
    ; on tab, indicates if the user has asked for the gui to show up.
    )
  
  (define profile-tab-mixin
    (mixin (drracket:unit:tab<%>) (profile-interactions-tab<%>)
      (define profile-info-visible? #f)
      (define/public (get-profile-info-visible?) profile-info-visible?)
      
      (define sort-mode (preferences:get 'drracket:profile-how-to-count))
      (define/public (get-sort-mode) sort-mode)
      (define/public (set-sort-mode mode) (set! sort-mode mode))
      
      (inherit get-frame is-current-tab? get-defs)
      ;; profile-info : (listof hashtable[symbol -o> prof-info])
      (define profile-info '())
      (define/public (add-profile-info ht) (set! profile-info (cons ht profile-info)))
      
      (define/public (show-profile)
        (unless profile-info-visible?
          (set! profile-info-visible? #t)
          (send (get-frame) show-profile-gui)))
      
      (define/public (hide-profile)
        (when profile-info-visible?
          (set! profile-info-visible? #f)
          (send profile-info-text clear-profile-display)
          (when (is-current-tab?)
            (send (get-frame) hide-profile-gui))))
      
      (define/public (reset-profile)
        (hide-profile)
        (set! profile-info '()))
      
      (define/public (refresh-profile) 
        (send profile-info-text refresh-profile profile-info (get-defs)))
      
      ;; can-show-profile? : -> boolean
      ;; indicates if there is any profiling information to be shown.
      (define/public (can-show-profile?)
        (let/ec esc-k
          (for-each
           (λ (ht)
             (hash-for-each
              ht
              (λ (key v)
                (when (any-info? v)
                  (esc-k #t)))))
           profile-info)
          #f))
      
      (define profile-info-text (new profile-text% (tab this)))
      (define/public (get-profile-info-text) profile-info-text)
      
      (define/augment (clear-annotations)
        (inner (void) clear-annotations)
        (reset-profile))
      
      (super-new)))
  
  ;; profile-unit-frame-mixin : mixin
  ;; adds profiling to the unit frame
  (define profile-unit-frame-mixin
    (mixin (drracket:unit:frame<%> drracket:frame:<%>) ()
      
      (inherit get-interactions-text get-current-tab set-show-menu-sort-key)
      
      ;; update-shown : -> void
      ;; updates the state of the profile item's show menu
      (define/override (update-shown)
        (super update-shown)
        (send show-profile-menu-item set-label
              (if profile-info-visible?
                  (string-constant profiling-hide-profile)
                  (string-constant profiling-show-profile))))
      
      ;; add-show-menu-items : menu -> void
      ;; adds the show profile menu item
      (define/override (add-show-menu-items show-menu)
        (super add-show-menu-items show-menu)
        (set! show-profile-menu-item 
              (instantiate menu:can-restore-menu-item% ()
                (label (string-constant profiling-hide-profile))
                (parent show-menu)
                (callback
                 (λ (x y)
                   (show-profile-menu-callback)))))
        (set-show-menu-sort-key show-profile-menu-item 207))
      
      (define show-profile-menu-item #f)
      (define profile-gui-constructed? #f)
      
      ;; get-profile-info-visible? : -> boolean
      ;; returns #t when the profiling information is visible in the frame.
      (define/public (get-profile-info-visible?) profile-info-visible?)
      
      (field [profile-info-outer-panel #f])
      (define/override (make-root-area-container % parent)
        (set! profile-info-outer-panel
              (super make-root-area-container
                     panel:vertical-dragable%
                     parent))
        (make-object % profile-info-outer-panel))
      
      (define/private (show-profile-menu-callback)
        (cond
          [profile-info-visible?
           (send (get-current-tab) hide-profile)]
          [(send (get-current-tab) can-show-profile?)
           (send (get-current-tab) show-profile)
           (send (get-current-tab) refresh-profile)]
          [else
           (message-box (string-constant drscheme)
                        (string-constant profiling-no-information-available)
                        #:dialog-mixin frame:focus-table-mixin)]))
      
      (define/public (hide-profile-gui)
        (when profile-gui-constructed?
          (when profile-info-visible?
            (send profile-info-outer-panel change-children
                  (λ (l)
                    (remq profile-info-panel l)))
            (set! profile-info-visible? #f)
            (update-shown))))
      
      (define/public (show-profile-gui)
        (unless profile-info-visible?
          (construct-profile-gui)
          (send profile-info-outer-panel change-children
                (λ (l)
                  (append (remq profile-info-panel l)
                          (list profile-info-panel))))
          (set! profile-info-visible? #t)
          (send profile-info-editor-canvas set-editor (send (get-current-tab) get-profile-info-text))
          (send (get-current-tab) refresh-profile)
          ;; set to a small percentage so it gets the minimum height
          (send profile-info-outer-panel set-percentages '(9/10 1/10))
          (update-shown)))
      
      (field (profile-info-visible? #f))
      
      (define/augment (on-tab-change from-tab to-tab)
        (inner (void) on-tab-change from-tab to-tab)
        (cond
          [(and (not profile-info-visible?)
                (send to-tab get-profile-info-visible?))
           (show-profile-gui)]
          [(and profile-info-visible?
                (not (send to-tab get-profile-info-visible?)))
           (hide-profile-gui)])
        (when profile-choice
          (send profile-choice set-selection
                (profile-mode->selection
                 (send to-tab get-sort-mode))))
        (when profile-info-editor-canvas
          (send profile-info-editor-canvas set-editor 
                (and (send to-tab can-show-profile?)
                     (send to-tab get-profile-info-text)))))
      
      (super-new)
      
      (define profile-info-panel #f)
      (define profile-info-editor-canvas #f)
      (define profile-choice #f)
      
      (inherit begin-container-sequence end-container-sequence)
      (define/private (construct-profile-gui)
        (unless profile-gui-constructed?
          (set! profile-gui-constructed? #t)
          (begin-container-sequence)
          (let ()
            (define _2
              (set! profile-info-panel (instantiate horizontal-panel% ()
                                         (parent profile-info-outer-panel)
                                         (stretchable-height #f))))
            (define profile-left-side (instantiate vertical-panel% (profile-info-panel)))
            (define _3
              (set! profile-info-editor-canvas (new canvas:basic% 
                                                    (parent profile-info-panel)
                                                    (editor (send (get-current-tab) get-profile-info-text)))))
            (define profile-message (instantiate message% ()
                                      (label (string-constant profiling))
                                      (parent profile-left-side)))
            (define _4
              (set! profile-choice (instantiate radio-box% ()
                                     (label #f)
                                     (parent profile-left-side)
                                     (callback
                                      (λ (x y)
                                        (let ([mode (profile-selection->mode (send profile-choice get-selection))])
                                          (preferences:set 'drracket:profile-how-to-count mode)
                                          (send (get-current-tab) set-sort-mode mode)
                                          (send (get-current-tab) refresh-profile))))
                                     (choices (list (string-constant profiling-time)
                                                    (string-constant profiling-number))))))
            (define _1
              (send profile-choice set-selection
                    (case (preferences:get 'drracket:profile-how-to-count)
                      [(time) 0]
                      [(count) 1])))
            (define update-profile-button
              (instantiate button% ()
                (label (string-constant profiling-update))
                (parent profile-left-side)
                (callback
                 (λ (x y)
                   (send (get-current-tab) refresh-profile)))))
            (define hide-profile-button 
              (instantiate button% ()
                (label (string-constant profiling-hide-profile))
                (parent profile-left-side)
                (callback
                 (λ (x y)
                   (send (get-current-tab) hide-profile)))))
            (send profile-choice set-selection 
                  (profile-mode->selection (preferences:get 'drracket:profile-how-to-count)))
            
            (send profile-left-side stretchable-width #f)
            
            (let ([wid (max (send update-profile-button get-width)
                            (send hide-profile-button get-width)
                            (send profile-choice get-width)
                            (send profile-message get-width))])
              (send update-profile-button min-width wid)
              (send hide-profile-button min-width wid)
              (send profile-choice min-width wid))
            (send profile-left-side set-alignment 'left 'center)
            
            ;; hide profiling info initially, but reflow the container
            ;; so that the invisible children get the right size.
            (send this reflow-container)
            (send profile-info-outer-panel change-children
                  (λ (l)
                    (remq profile-info-panel l))))
          (end-container-sequence)))))
  
  (define (profile-selection->mode sel)
    (case sel
      [(0) 'time]
      [(1) 'count]))
  
  (define (profile-mode->selection mode)
    (case mode
      [(time) 0]
      [(count) 1]))
  
  ;; profile-text% : extends text:basic%
  ;; this class keeps track of a single thread's
  ;; profiling information. these methods are not
  ;; to be called directly, but only by the frame class, since
  ;; they do not completely implement the abstraction for the
  ;; GUI. They only manage the profiling information reported
  ;; in the bottom window
  (define profile-text% 
    (class text:line-spacing%
      (init-field tab)
      
      ;; clear-profile-display : -> void
      ;; clears out the GUI showing the profile results
      (define/public (clear-profile-display)
        (begin-edit-sequence #t #f)
        (let ([locked? (is-locked?)])
          (lock #f)
          (clear-old-results)
          (erase)
          (lock locked?)
          (end-edit-sequence)))
      
      (inherit lock is-locked?
               get-canvas hide-caret get-snip-location
               begin-edit-sequence end-edit-sequence 
               erase insert)
      
      ;; clear-old-results : -> void
      ;; removes the profile highlighting
      (field [clear-old-results void])
      
      ;; refresh-profile : (listof hashtable[...]) text% -> void
      ;; does the work to erase any existing profile info
      ;; and make new profiling info.
      (define/public (refresh-profile profile-info definitions-text)
        (begin-edit-sequence #t #f)
        (lock #f)
        (erase)
        (clear-old-results)
        (let* (;; must copy them here in case the program is still running
               ;; and thus updating them.
               [infos '()]
               [_ (let loop ([profile-info profile-info])
                    (cond
                      [(null? profile-info) (void)]
                      [else 
                       (let ([ht (car profile-info)])
                         (hash-for-each
                          ht
                          (λ (key val)
                            (when (any-info? val)
                              (set! infos (cons (copy-prof-info val) infos))))))
                       (loop (cdr profile-info))]))]
               
               ;; each editor that gets some highlighting is put
               ;; into this table and an edit sequence is begun for it.
               ;; after all ranges are updated, the edit sequences are all closed.
               [in-edit-sequence (make-hasheq)]
               [clear-highlight void]
               [max-value (extract-maximum infos)]
               
               [port-name-matches-cache (make-hasheq)]
               [show-highlight
                (λ (info)
                  (let* ([expr (prof-info-expr info)]
                         [src (and (syntax-source expr)
                                   definitions-text
                                   (hash-ref! port-name-matches-cache
                                              (syntax-source expr)
                                              (λ () (send definitions-text port-name-matches? (syntax-source expr)))))]
                         [pos (syntax-position expr)]
                         [span (syntax-span expr)])
                    (when (and (is-a? src text:basic<%>)
                               (number? pos)
                               (number? span))
                      (unless (hash-ref in-edit-sequence src (λ () #f))
                        (hash-set! in-edit-sequence src #t)
                        (send src begin-edit-sequence #t #f))
                      (let* ([color (get-color-value 
                                     (if (eq? (preferences:get 'drracket:profile-how-to-count) 'time)
                                         (prof-info-time info)
                                         (prof-info-num info))
                                     max-value)]
                             [clr (send src highlight-range (- pos 1) (+ pos span -1) color)])
                        (let ([old-thnk clear-highlight])
                          (set! clear-highlight
                                (λ ()
                                  (clr)
                                  (old-thnk))))))))]
               [smaller-range?
                (λ (x y)
                  (let ([x-span (syntax-span (prof-info-expr x))]
                        [y-span (syntax-span (prof-info-expr y))])
                    (if (and x-span y-span)
                        (< x-span y-span)
                        #f)))]
               
               [show-line
                (λ (info newline? highlight-line?)
                  (let* ([expr (prof-info-expr info)]
                         [expr-src (syntax-source expr)]
                         [count (prof-info-num info)]
                         [time (prof-info-time info)]
                         [name (prof-info-name info)])
                    (when newline? (send src-loc-editor insert "\n"))
                    (when highlight-line? (small-blank-line src-loc-editor))
                    (let ([before (send src-loc-editor last-position)])
                      (insert-profile-src-loc src-loc-editor expr name)
                      (let ([after (send src-loc-editor last-position)])
                        (cond
                          [(string? expr-src)
                           (send src-loc-editor change-style (gui-utils:get-clickback-delta) before after)
                           (let ([after (send src-loc-editor last-position)])
                             (send src-loc-editor set-clickback 
                                   before after 
                                   (λ (text start end)
                                     (open-file-and-goto-position expr-src (syntax-position expr)))))]
                          [(is-a? expr-src editor:basic<%>)
                           (send src-loc-editor change-style (gui-utils:get-clickback-delta) before after)
                           (send src-loc-editor set-clickback
                                 before after
                                 (λ (text start end)
                                   (let ([window (send expr-src get-top-level-window)]
                                         [pos (syntax-position expr)])
                                     (when window (send window show #t))
                                     (when pos (send expr-src set-position (- pos 1)))
                                     (send expr-src set-caret-owner #f 'global))))]
                          [else (void)])))
                    
                    (when newline? (send time-editor insert "\n"))
                    (when highlight-line? (small-blank-line time-editor))
                    (send time-editor insert (format "~a" time))
                    (send time-editor set-paragraph-alignment (send time-editor last-paragraph) 'right)
                    
                    (when newline? (send count-editor insert "\n")) 
                    (when highlight-line? (small-blank-line count-editor))
                    (send count-editor insert (format "~a" count))
                    (send count-editor set-paragraph-alignment (send count-editor last-paragraph) 'right)))]
               
               [bigger-value?
                (λ (x y)
                  (let ([sel (if (eq? 'count (preferences:get 'drracket:profile-how-to-count))
                                 prof-info-num
                                 prof-info-time)])
                    (> (sel x) (sel y))))]
               
               [cleanup-editor
                (λ (ed)
                  (let* ([ed-admin (send ed get-admin)]
                         [snip (send ed-admin get-snip)]
                         [bl (box 0)]
                         [br (box 0)])
                    (get-snip-location snip bl #f #f)
                    (get-snip-location snip br #f #t)
                    (let ([w (+ (- (unbox br) (unbox bl)) 4)])
                      (send ed set-max-width w)
                      (send ed set-min-width w)))
                  (send ed hide-caret #t)
                  (send ed lock #t))]
               
               [top-infos (top 100 (sort infos bigger-value?))])
          (for-each show-highlight top-infos)
          (initialize-editors)
          (let loop ([infos top-infos]
                     [newline? #f]
                     [highlight-counter 0])
            (cond
              [(null? infos) (void)]
              [else 
               (show-line (car infos) newline? (and newline? (zero? highlight-counter)))
               (loop (cdr infos) #t (modulo (+ highlight-counter 1) 2))]))
          (cleanup-editor count-editor)
          (cleanup-editor time-editor)
          (cleanup-editor src-loc-editor)
          
          (hash-for-each
           in-edit-sequence
           (λ (key val)
             (send key end-edit-sequence)))
          (set! clear-old-results 
                (λ ()
                  (hash-for-each
                   in-edit-sequence
                   (λ (key val) (send key begin-edit-sequence #t #f)))
                  (clear-highlight)
                  (hash-for-each
                   in-edit-sequence
                   (λ (key val) (send key end-edit-sequence)))
                  (set! clear-old-results void))))
        (lock #t)
        (end-edit-sequence)
        (let ([canvas (get-canvas)])
          (when canvas
            (send canvas scroll-to 0 0 1 1 #t 'start))))
      
      
      ;; top : number (listof X) -> (listof X)
      ;; extracts the first `n' elements from a list.
      (define/private (top n lst)
        (let loop ([n n]
                   [lst lst])
          (cond
            [(null? lst) null]
            [(= 0 n) null]
            [else (cons (car lst) (loop (- n 1) (cdr lst)))])))
      
      (field (src-loc-editor #f)
             (time-editor #f)
             (count-editor #f))
      (define/private (clear-editors)
        (set! src-loc-editor #f)
        (set! time-editor #f)
        (set! count-editor #f))
      (define/private (initialize-editors)
        (set! src-loc-editor (instantiate text% ()))
        (set! time-editor (instantiate text% ()))
        (set! count-editor (instantiate text% ()))
        (send src-loc-editor set-styles-sticky #f)            
        (send time-editor set-styles-sticky #f)
        (send count-editor set-styles-sticky #f)
        (insert (instantiate editor-snip% (time-editor)))
        (insert (instantiate editor-snip% (count-editor)))
        (insert (instantiate editor-snip% (src-loc-editor)))
        (insert-title (string-constant profiling-col-function) src-loc-editor)
        (insert-title (string-constant profiling-col-time-in-msec) time-editor)
        (insert-title (string-constant profiling-col-calls) count-editor))
      
      (define/private (insert-title str txt)
        (send txt insert str)
        (send txt insert "\n")
        (send txt change-style bold-delta 0 (- (send txt last-position) 1))
        (send txt set-paragraph-alignment 0 'center))
      
      (super-new)
      (hide-caret #t)))
  
  ;; format-percentage : number[0 <= n <= 1] -> string
  ;; formats the number as a percentage string with trailing zeros,
  ;; to 3 decimal places.
  (define (format-percentage n)
    (let* ([number-of-places 3]
           [whole-part (floor (* n 100))]
           [decimal-part (- (* n 100) whole-part)]
           [truncated/moved-decimal-part (floor (* (expt 10 number-of-places) decimal-part))]
           [pad
            (λ (str)
              (if ((string-length str) . < . number-of-places)
                  (string-append (make-string (- number-of-places (string-length str)) #\0) 
                                 str)
                  str))])
      (string-append (format "~a" whole-part)
                     "."
                     (pad (format "~a" truncated/moved-decimal-part)))))
  
  (define (small-blank-line txt)
    (let ([before (send txt last-position)])
      (send txt insert "\n")
      (let ([after (send txt last-position)])
        (send txt change-style small-font-style before after))))
  
  (define small-font-style (make-object style-delta% 'change-size 6))
  
  ;; bold-delta : style-delta
  (define bold-delta (make-object style-delta% 'change-bold))
  
  ;; insert-profile-src-loc : syntax name -> string
  (define (insert-profile-src-loc editor stx name)
    (cond
      [name
       (let ([before (send editor last-position)])
         (send editor insert (format "~a" name)))]
      [else
       (let* ([src (syntax-source stx)]
              [filename 
               (cond
                 [(string? src) src]
                 [(is-a? src editor<%>) (get-filename-from-editor src)]
                 [else (string-constant profiling-unknown-src)])]
              [col (syntax-column stx)]
              [line (syntax-line stx)]
              [pos (syntax-position stx)]
              [span (syntax-span stx)]
              [src
               (cond
                 [(and col line)
                  (format "~a: ~a.~a" filename line col)]
                 [pos
                  (format "~a: ~a" filename pos)]
                 [else 
                  filename])])
         (send editor insert src))]))
  
  ;; open-file-and-goto-position : string (union #f number) -> void
  (define (open-file-and-goto-position filename pos)
    (let ([frame (handler:edit-file filename)])
      (when (and frame
                 pos
                 (is-a? frame drracket:unit:frame%))
        (let ([defs (send frame get-definitions-text)])
          (send defs set-position (- pos 1))))))
  
  ;; get-src-filename : tst -> (union #f string)
  (define (get-src-filename src)
    (cond
      [(string? src) src]
      [(is-a? src text%)
       (send src get-filename)]
      [else #f]))
  
  ;; get-src-loc : syntax -> string
  (define (get-src-loc expr)
    (cond
      [(and (number? (syntax-line expr))
            (number? (syntax-column expr))
            (number? (syntax-span expr)))
       (format " ~a.~a [~a]" 
               (syntax-line expr) 
               (syntax-column expr)
               (syntax-span expr))]
      [(and (number? (syntax-position expr))
            (number? (syntax-span expr)))
       (format " ~a-~a" 
               (syntax-position expr)
               (syntax-span expr))]
      [else ""]))
  
  (define (add-prefs-panel)
    (preferences:add-panel
     (string-constant profiling)
     (λ (s-parent)
       (letrec ([parent (make-object vertical-panel% s-parent)]
                [msg (make-object message% 
                       (string-constant profiling-color-config) 
                       parent)]
                [hp (make-object horizontal-pane% parent)]
                [low (make-object button% (string-constant profiling-low-color) hp 
                       (λ (x y) (color-callback #t)))]
                [color-bar (make-object color-bar% hp)]
                [high (make-object button% (string-constant profiling-high-color) hp
                        (λ (x y) (color-callback #f)))]
                
                [scale (instantiate radio-box% ()
                         (label (string-constant profiling-scale))
                         (parent parent)
                         (callback (λ (x y) (scale-callback)))
                         (choices
                          (list (string-constant profiling-sqrt)
                                (string-constant profiling-linear)
                                (string-constant profiling-square))))]
                
                [color-callback
                 (λ (low?)
                   (let ([color (get-color-from-user 
                                 (if low?
                                     (string-constant profiling-choose-low-color)
                                     (string-constant profiling-choose-high-color))
                                 #f
                                 (preferences:get
                                  (if low?
                                      'drracket:profile:low-color
                                      'drracket:profile:high-color)))])
                     (when color
                       (preferences:set 
                        (if low? 'drracket:profile:low-color 'drracket:profile:high-color)
                        color))))]
                [scale-callback
                 (λ ()
                   (preferences:set 
                    'drracket:profile:scale
                    (case (send scale get-selection)
                      [(0) 'sqrt]
                      [(1) 'linear]
                      [(2) 'square])))])
         (preferences:add-callback
          'drracket:profile:scale
          (λ (p v)
            (send scale set-selection
                  (case v
                    [(sqrt) 0]
                    [(linear) 1]
                    [(square) 2]))))
         (send parent set-alignment 'left 'center)
         (send hp stretchable-height #f)
         parent))))
  
  (define color-bar%
    (class canvas%
      (inherit get-client-size get-dc)
      (field [pen (make-object pen% "black" 1 'solid)]
             [in-on-paint? #f])
      (define/override (on-paint)
        (set! in-on-paint? #t)
        (let* ([dc (get-dc)]
               [dummy-pen (send dc get-pen)]
               [drracket:profile:low-color (preferences:get 'drracket:profile:low-color)]
               [drracket:profile:high-color (preferences:get 'drracket:profile:high-color)]
               [drracket:profile:scale (preferences:get 'drracket:profile:scale)])
          (let-values ([(w h) (get-client-size)])
            (let loop ([n 0])
              (when (n . <= . w)
                (send pen set-color 
                      (get-color-value/pref n w drracket:profile:low-color drracket:profile:high-color drracket:profile:scale))
                (send dc set-pen pen)
                (send dc draw-line n 0 n h)
                (send dc set-pen dummy-pen)
                (loop (+ n 1))))
            (let-values ([(tw th ta td) (send dc get-text-extent 
                                              (string-constant profiling-example-text))])
              (send dc draw-text
                    (string-constant profiling-example-text)
                    (floor (- (/ w 2) (/ tw 2)))
                    (floor (- (/ h 2) (/ th 2)))))))
        (set! in-on-paint? #f))
      
      ;; queue callbacks here so that the preferences
      ;; values are actually set by the time on-paint
      ;; is called.
      (preferences:add-callback
       'drracket:profile:scale
       (λ (p v)
         (unless in-on-paint?
           (queue-callback
            (λ ()
              (on-paint))))))
      (preferences:add-callback
       'drracket:profile:low-color
       (λ (p v)
         (unless in-on-paint?
           (queue-callback
            (λ ()
              (on-paint))))))
      (preferences:add-callback
       'drracket:profile:high-color
       (λ (p v)
         (unless in-on-paint?
           (queue-callback
            (λ ()
              (on-paint))))))
      
      (super-instantiate ())))


  (define-values/invoke-unit/infer stacktrace@))
