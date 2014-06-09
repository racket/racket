#lang racket/base

(require string-constants
         racket/class
         racket/match
         racket/list
         racket/set
         racket/unit
         mred/mred-sig
         "../preferences.rkt"
         unstable/2d/dir-chars
         mrlib/tex-table
         (only-in srfi/13 string-prefix? string-prefix-length)
         "sig.rkt")
(provide keymap@)

(define-unit keymap@
  (import mred^
          [prefix finder: framework:finder^]
          [prefix handler: framework:handler^]
          [prefix frame: framework:frame^]
          [prefix editor: framework:editor^]
          [prefix text: framework:text^])
  (export (rename framework:keymap^
                  [-get-file get-file]))
  (init-depend mred^)

  ;; if I put this in main.rkt with the others, it doesn't happen
  ;; early enough... ? JBC, 2011-07-12
  (preferences:set-default 'framework:automatic-parens #f boolean?)


  (define user-keybindings-files (make-hash))
  
  (define (add-user-keybindings-file spec)
    (hash-ref
     user-keybindings-files
     spec
     (λ ()
       (let* ([path (spec->path spec)]
              [sexp (and (file-exists? path)
                         (parameterize ([read-accept-reader #t])
                           (call-with-input-file path read)))])
         (match sexp
           [`(module ,name ,lang ,x ...)
	    (cond
	     [(valid-keybindings-lang? lang)
	      (let ([km (dynamic-require spec '#%keymap)])
		(hash-set! user-keybindings-files spec km)
		(send user-keymap chain-to-keymap km #t))]
	     [else
	      (error 'add-user-keybindings-file
		     (string-constant user-defined-keybinding-malformed-file/found-lang)
		     (path->string path)
		     lang)])]
           [else (error 'add-user-keybindings-file 
                        (string-constant user-defined-keybinding-malformed-file)
                        (path->string path))])))))
  
  (define (valid-keybindings-lang? x)
    (member x
            (list `(lib "keybinding-lang.ss" "framework")
                  `(lib "keybinding-lang.rkt" "framework")
                  `(lib "framework/keybinding-lang.ss")
                  `(lib "framework/keybinding-lang.rkt")
                  `framework/keybinding-lang)))
  
  (define (spec->path p)
    (cond
      [(path? p) p]
      [else
       (let* ([mod-name ((current-module-name-resolver) p #f #f #t)]
              [str (symbol->string mod-name)]
              [pth (substring str 1 (string-length str))])
         (let-values ([(base name _) (split-path pth)])
           (let ([filenames
                  (sort
                   (filter (λ (x) (substring? (path->string name) x))
                           (map path->string (directory-list base)))
                   (λ (x y) (> (string-length x) (string-length y))))])
             (when (null? filenames)
               (error 'spec->path "could not convert ~s, found no filenames for ~s" p mod-name))
             (build-path base (car filenames)))))]))
  
  (define (substring? s1 s2)
    (and (<= (string-length s1) 
             (string-length s2))
         (string=? s1 (substring s2 0 (string-length s1)))))
  
  (define (remove-user-keybindings-file spec)
    (let/ec k
      (let ([km (hash-ref user-keybindings-files spec (λ () (k (void))))])
        (send global remove-chained-keymap km)
        (hash-remove! user-keybindings-files spec))))
  
  (define (remove-chained-keymap ed keymap-to-remove)
    (let ([ed-keymap (send ed get-keymap)])
      (when (eq? keymap-to-remove ed-keymap)
        (error 'keymap:remove-keymap "cannot remove initial keymap from editor"))
      (let p-loop ([parent-keymap ed-keymap])
        (unless (is-a? parent-keymap aug-keymap<%>)
          (error 'keymap:remove-keymap
                 "found a keymap that is not a keymap:aug-keymap<%> ~e" 
                 parent-keymap))
        (let c-loop ([child-keymaps (send parent-keymap get-chained-keymaps)])
          (cond
            [(null? child-keymaps) 
             (void)]
            [else
             (let ([child-keymap (car child-keymaps)])
               (cond
                 [(eq? child-keymap keymap-to-remove)
                  (send parent-keymap remove-chained-keymap child-keymap)
                  (c-loop (cdr child-keymaps))]
                 [else 
                  (p-loop child-keymap)
                  (c-loop (cdr child-keymaps))]))])))))
  
  (define (set-chained-keymaps parent-keymap children-keymaps)
    (for-each (λ (orig-sub) (send parent-keymap remove-chained-keymap))
              (send parent-keymap get-chained-keymaps))
    (for-each (λ (new-sub) (send parent-keymap chain-to-keymap new-sub #f))
              children-keymaps))
  
  (define aug-keymap<%> (interface ((class->interface keymap%))
                          get-chained-keymaps
                          get-map-function-table
                          get-map-function-table/ht))
  
  (define aug-keymap-mixin
    (mixin ((class->interface keymap%)) (aug-keymap<%>)
      (define chained-keymaps null)
      (define/public (get-chained-keymaps) chained-keymaps)
      
      (define/override (chain-to-keymap keymap prefix?)
        (super chain-to-keymap keymap prefix?)
        (set! chained-keymaps
              (if prefix?
                  (cons keymap chained-keymaps)
                  (append chained-keymaps (list keymap)))))
      
      (define/override (remove-chained-keymap keymap)
        (super remove-chained-keymap keymap)
        (set! chained-keymaps (remq keymap chained-keymaps)))
      
      (define function-table (make-hasheq))
      (define/public (get-function-table) function-table)
      (define/override (map-function keyname fname)
        (super map-function (canonicalize-keybinding-string keyname) fname)
        (hash-set! function-table (string->symbol keyname) fname))
      
      (define/public (get-map-function-table)
        (get-map-function-table/ht (make-hasheq)))
      
      (define/public (get-map-function-table/ht table)
        (for ([(keyname fname) (in-hash function-table)])
          (define cs (canonicalize-keybinding-string (format "~a" keyname)))
          (define key (string->symbol cs))
          (unless (hash-ref table key #f)
            (when (on-this-platform? cs)
              (hash-set! table key fname))))
        (for ([chained-keymap (in-list chained-keymaps)])
          (when (is-a? chained-keymap aug-keymap<%>)
            (send chained-keymap get-map-function-table/ht table)))
        table)
      
      (define/private (on-this-platform? cs)
        (define splits
          (for/list ([x (in-list (split-out #\; (string->list cs)))])
            (all-but-last (split-out #\: x))))
        (define (has-key? k) (ormap (λ (x) (member (list k) x)) splits))
        (cond
          [(eq? (system-type) 'windows)
           (cond
             [(or (and (regexp-match? #rx"a:c" cs)
                       (not (regexp-match? #rx"~a:c" cs)))
                  (and (regexp-match? #rx"c:m" cs)
                       (not (regexp-match? #rx"~c:m" cs))))
              #f]
             [(or (has-key? #\a) (has-key? #\d))
              #f]
             [else #t])]
          [(eq? (system-type) 'macosx)
           (cond
             [(has-key? #\m)
              #f]
             [else #t])]
          [(eq? (system-type) 'unix)
           (cond
             [(or (has-key? #\a) (has-key? #\d))
              #f]
             [else #t])]
          [else 
           ;; just in case new platforms come along .... 
           #t]))
      
      (define/private (all-but-last l)
        (cond
          [(null? l) l]
          [(null? (cdr l)) l]
          [else (cons (car l) (all-but-last (cdr l)))]))
      
      (super-new)))
  
  (define aug-keymap% (aug-keymap-mixin keymap%))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;           canonicalize-keybinding-string            ;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  ;; canonicalize-keybinding-string : string -> string
  ;; The result can be used with string=? to determine
  ;; if two key bindings refer to the same key.
  ;; Assumes a well-formed keystring.
  (define (canonicalize-keybinding-string str)
    (define chars (map char-downcase (string->list str)))
    (define separated-keys
      (map
       canonicalize-single-keybinding-string
       (split-out #\; chars)))
    (join-strings ";" separated-keys))
  
  ;; join-strings : string (listof string) -> string
  ;; concatenates strs with sep between each of them
  (define (join-strings sep strs)
    (if (null? strs)
        ""
        (apply
         string-append
         (cons
          (car strs)
          (let loop ([sepd-strs (cdr strs)])
            (cond
              [(null? sepd-strs) null]
              [else (list*
                     sep
                     (car sepd-strs)
                     (loop (cdr sepd-strs)))]))))))
  
  ;; canonicalize-single-keybinding-string : (listof char) -> string
  (define (canonicalize-single-keybinding-string chars)
    (let* ([neg? (char=? (car chars) #\:)]
           [mods/key (split-out #\: (if neg? (cdr chars) chars))]
           [mods
            (let loop ([mods mods/key])
              (cond
                [(null? mods) null]
                [(null? (cdr mods)) null]
                [else (cons (car mods) (loop (cdr mods)))]))]
           [key (apply string (car (last-pair mods/key)))]
           [canon-key
            (cond
              [(string=? key "enter") "return"]
              [(string=? key "del") "delete"]
              [(string=? key "ins") "insert"]
              [else key])]
           [shift (if neg? #f 'd/c)]
           [control (if neg? #f 'd/c)]
           [alt (if neg? #f 'd/c)]
           [meta (if neg? #f 'd/c)]
           [command (if neg? #f 'd/c)]
           [lock 'd/c]
           [question-mark 'd/c]
           
           [do-key
            (λ (char val)
              (cond
                [(eq? val #t) (string char)]
                [(eq? val #f) (string #\~ char)]
                [(eq? val 'd/c) #f]))])
      
      (for-each (λ (mod)
                  (let ([val (not (char=? (car mod) #\~))])
                    (case (if (char=? (car mod) #\~)
                              (cadr mod)
                              (car mod))
                      [(#\s) (set! shift val)]
                      [(#\c) (set! control val)]
                      [(#\a) (set! alt val)]
                      [(#\d) (set! command val)]
                      [(#\m) (set! meta val)]
                      [(#\l) (set! lock val)]
                      [(#\?) (set! question-mark val)])))
                mods)
      
      (join-strings ":"
                    (filter
                     values
                     (list
                      (do-key #\? question-mark)
                      (do-key #\a alt)
                      (do-key #\c control)
                      (do-key #\d command)
                      (do-key #\m meta)
                      (do-key #\s shift)
                      (do-key #\l lock)
                      canon-key)))))
  
  ;; split-out : char (listof char) -> (listof (listof char))
  ;; splits a list of characters at its first argument
  ;; if the last character is the same as the first character,
  ;; it is not split into an empty list, but returned.
  (define (split-out split-char chars)
    (let loop ([chars chars]
               [this-split null]
               [all-split null])
      (cond
        [(null? chars)
         (reverse (cons (reverse this-split) all-split))]
        [else (let ([char (car chars)])
                (cond
                  [(char=? split-char char)
                   (if (null? (cdr chars))
                       (loop null
                             (cons char this-split)
                             all-split)
                       (loop (cdr chars)
                             null
                             (cons (reverse this-split) all-split)))]
                  [else
                   (loop (cdr chars)
                         (cons char this-split)
                         all-split)]))])))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;         end canonicalize-keybinding-string          ;;;;;;;;
  ;;;;;;;                                                     ;;;;;;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  (define (make-meta-prefix-list key [mask-control? #f])
    (list (if mask-control?
              (string-append "m:" key)
              (string-append "~c:m:" key))
          (string-append "ESC;" key)))
  
  (define (send-map-function-meta keymap key func [mask-control? #f] 
                                  #:alt-as-meta-keymap [alt-as-meta-keymap #f])
    (for ([key (in-list (make-meta-prefix-list key mask-control?))])
      (send keymap map-function key func))
    (when alt-as-meta-keymap
      (unless (send alt-as-meta-keymap is-function-added? func)
        (error 'send-map-function-meta
               "expected to find ~s mapped in alt-as-meta-keymap"
               func))
      (send alt-as-meta-keymap map-function (string-append "?:a:" key) func)))
  
  (define has-control-regexp #rx"(?:^|:)c:")
  
  (define add-to-right-button-menu (make-parameter void))
  (define add-to-right-button-menu/before (make-parameter void))
  
  (define setup-global
    ; Define some useful keyboard functions
    (let* ([ring-bell
            (λ (edit event)
              (bell))]
           
           [mouse-popup-menu 
            (λ (edit event)
              (when (send event button-up?)
                (let ([a (send edit get-admin)])
                  (when a
                    (let ([m (make-object popup-menu%)])
                      
                      ((add-to-right-button-menu/before) m edit event)
                      
                      (append-editor-operation-menu-items 
                       m #:popup-position 
                       (list edit
                             (send edit find-position (send event get-x) (send event get-y))))
                      (for-each
                       (λ (i)
                         (when (is-a? i selectable-menu-item<%>)
                           (send i set-shortcut #f)))
                       (send m get-items))
                      
                      ((add-to-right-button-menu) m edit event)
                      
                      (let-values ([(x y) (send edit
                                                dc-location-to-editor-location
                                                (send event get-x)
                                                (send event get-y))])
                        (send a popup-menu m (+ x 1) (+ y 1))))))))]
           
           [toggle-anchor
            (λ (edit event)
              (send edit set-anchor
                    (not (send edit get-anchor))))]
           [center-view-on-line
            (λ (edit event)
              (let ([new-mid-line (send edit position-line
                                        (send edit get-start-position))]
                    [bt (box 0)]
                    [bb (box 0)])
                (send edit get-visible-line-range bt bb #f)
                (let* ([half (sub1 (quotient (- (unbox bb) (unbox bt)) 2))]
                       [last-pos (send edit position-line (send edit last-position))]
                       [top-pos (send edit line-start-position 
                                      (max (min (- new-mid-line half) last-pos) 0))]
                       [bottom-pos (send edit line-start-position 
                                         (max 0
                                              (min (+ new-mid-line half)
                                                   last-pos)))])
                  (send edit scroll-to-position 
                        top-pos
                        #f
                        bottom-pos)))
              #t)]
           
                      
           [collapse-variable-space
            ;; As per emacs: collapse tabs & spaces around the point,
            ;; perhaps leaving a single space.
            ;; drscheme bonus: if at end-of-line, collapse into the next line.
            (λ (leave-one? edit event)
              (letrec ([last-pos  (send edit last-position)]
                       [sel-start (send edit get-start-position)]
                       [sel-end   (send edit get-end-position)]
                       [collapsible? (λ (c) (and (char-whitespace? c)
                                                 (not (char=? #\newline c))))]
                       [find-noncollapsible
                        ; Return index of next non-collapsible char,
                        ; starting at pos in direction dir.
                        ; NB returns -1 or last-pos, if examining
                        ; initial/final whitespace
                        ; (or, when initial pos is outside of [0,last-pos).)
                        (λ (pos dir)
                          (let loop ([pos pos])
                            (cond [(<  pos 0) -1]
                                  [(>= pos last-pos) last-pos]
                                  [(collapsible? (send edit get-character pos))
                                   (loop (+ pos dir))]
                                  [else pos])))])
                (when (= sel-start sel-end) ; Only when no selection:
                  (let* ([start (add1 (find-noncollapsible (sub1 sel-start) -1))]
                         [end-heeding-eol (find-noncollapsible sel-start +1)]
                         ; This is the end of the range, were we to always heed newlines.
                         
                         ; Special case: if we're sitting at EOL,
                         ; and we're not affecting much else,
                         ; then delete that EOL and collapse spaces
                         ; at the start of next line, too:
                         [end (if (and (<= (- end-heeding-eol start)
                                           (if leave-one? 1 0))
                                       (char=? #\newline (send edit get-character end-heeding-eol))
                                       ; If you wish to avoid deleting an newline at EOF, do so here.
                                       )
                                  (find-noncollapsible (add1 end-heeding-eol) +1)
                                  end-heeding-eol)]
                         [making-no-difference?
                          ; Don't introduce edits into undo-chain, if no effect.
                          (if leave-one?
                              (and (= (- end start) 1)
                                   (char=? #\space (send edit get-character start)))
                              (= (- end start) 0))])
                    (unless making-no-difference?
                      (send edit begin-edit-sequence)
                      (send edit set-position end) ; Even after delete, caret will be at "end".
                      (send edit delete start end)
                      (when leave-one? (send edit insert #\space start))
                      (send edit end-edit-sequence))))))]
           
           [collapse-space
            (λ (edit event)
              (collapse-variable-space #t edit event))]
           
           [remove-space
            (λ (edit event)
              (collapse-variable-space #f edit event))]
           
           [collapse-newline
            (λ (edit event)
              (define (find-nonwhite pos d offset)
                (define done (if (= offset -1) 0 (send edit last-position)))
                (let/ec escape
                  (let loop ([pos pos])
                    (cond
                      [(= pos done) (escape pos)]
                      [else
                       (define c (send edit get-character (+ pos offset)))
                       (cond
                         [(char=? #\newline c)
                          (loop (+ pos d))
                          (escape pos)]
                         [(char-whitespace? c)
                          (loop (+ pos d))]
                         [else pos])]))))
              (define sel-start (send edit get-start-position))
              (define sel-end (send edit get-end-position))
              (when (= sel-start sel-end)
                (define pos-para (send edit position-paragraph sel-start #f))
                (define pos-para-start (send edit paragraph-start-position pos-para))
                (define pos-para-end (send edit paragraph-end-position pos-para))
                       
                (define whitepara?
                  (let loop ([pos pos-para-start])
                    (if (>= pos pos-para-end)
                        #t
                        (and (char-whitespace? (send edit get-character pos))
                             (loop (add1 pos))))))
                       
                (define start (find-nonwhite pos-para-start -1 -1))
                (define end (find-nonwhite pos-para-end 1 0))
                
                (define start-para (send edit position-paragraph start #f))
                (define start-para-start (send edit paragraph-start-position start-para))
                (define end-para (send edit position-paragraph end #f))
                (define end-para-start (send edit paragraph-start-position (add1 end-para)))
                (cond
                  [(and whitepara?
                        (= start-para pos-para)
                        (= end-para pos-para))
                   ; Special case: just delete this para
                   (send edit delete pos-para-start (add1 pos-para-end))]
                  [(and whitepara? (< start-para pos-para))
                   ; Can delete before & after
                   (send* edit 
                     (begin-edit-sequence)
                     (delete (add1 pos-para-end) end-para-start)
                     (delete start-para-start pos-para-start)
                     (end-edit-sequence))]
                  [else
                   ; Only delete after
                   (send edit delete (add1 pos-para-end) end-para-start)])))]
           
           [open-line
            (λ (edit event)
              (let ([sel-start (send edit get-start-position)]
                    [sel-end (send edit get-end-position)])
                (when (= sel-start sel-end)
                    (send* edit 
                      (insert #\newline)
                      (set-position sel-start)))))]
           
           [transpose-chars
            (λ (edit event)
              (let ([sel-start (send edit get-start-position)]
                    [sel-end (send edit get-end-position)])
                (when (and (= sel-start sel-end)
                           (not (= sel-start 0)))
                  
                  (let ([sel-start
                         (if (= sel-start
                                (send edit line-end-position
                                      (send edit position-line sel-start)))
                             (sub1 sel-start)
                             sel-start)])
                    (let ([s (send edit get-text
                                   sel-start (add1 sel-start))])
                      (send* edit
                        (begin-edit-sequence)
                        (delete sel-start (add1 sel-start))
                        (insert s (- sel-start 1))
                        (set-position (add1 sel-start))
                        (end-edit-sequence)))))))]
           
           [transpose-words
            (λ (edit event)
              (let ([sel-start (send edit get-start-position)]
                    [sel-end (send edit get-end-position)])
                (when (= sel-start sel-end)
                  (let ([word-1-start (box sel-start)])
                    (send edit find-wordbreak word-1-start #f 'caret)
                    (let ([word-1-end (box (unbox word-1-start))])
                      (send edit find-wordbreak #f word-1-end 'caret)
                      (let ([word-2-end (box (unbox word-1-end))])
                        (send edit find-wordbreak #f word-2-end 'caret)
                        (let ([word-2-start (box (unbox word-2-end))])
                          (send edit find-wordbreak word-2-start #f 'caret)
                          (let ([text-1 (send edit get-text
                                              (unbox word-1-start)
                                              (unbox word-1-end))]
                                [text-2 (send edit get-text
                                              (unbox word-2-start)
                                              (unbox word-2-end))])
                            (send* edit
                              (begin-edit-sequence)
                              (insert text-1 
                                      (unbox word-2-start)
                                      (unbox word-2-end))
                              (insert text-2 
                                      (unbox word-1-start)
                                      (unbox word-1-end))
                              (set-position (unbox word-2-end))
                              (end-edit-sequence))))))))))]
           
           [capitalize-it
            (λ (edit char-case1 char-case2)
              (let ([sel-start (send edit get-start-position)]
                    [sel-end (send edit get-end-position)]
                    [real-end (send edit last-position)])
                (when (= sel-start sel-end)
                  (let ([word-end (let ([b (box sel-start)])
                                    (send edit find-wordbreak #f b 'caret)
                                    (min real-end (unbox b)))])
                    (send edit begin-edit-sequence)
                    (let loop ([pos sel-start]
                               [char-case char-case1])
                      (when (< pos word-end)
                        (let ([c (send edit get-character pos)])
                          (cond
                            [(char-alphabetic? c)
                             (send edit insert 
                                   (list->string
                                    (list (char-case c)))
                                   pos (add1 pos))
                             (loop (add1 pos) char-case2)]
                            [else 
                             (loop (add1 pos) char-case)]))))
                    (send* edit 
                      (end-edit-sequence)
                      (set-position word-end))))))]
           
           [capitalize-word
            (λ (edit event)
              (capitalize-it edit char-upcase char-downcase))]
           [upcase-word
            (λ (edit event)
              (capitalize-it edit char-upcase char-upcase))]
           [downcase-word
            (λ (edit event)
              (capitalize-it edit char-downcase char-downcase))]
           
           [kill-word
            (λ (edit event)
              (let ([sel-start (send edit get-start-position)]
                    [sel-end (send edit get-end-position)])
                (let ([end-box (box sel-end)])
                  (send edit find-wordbreak #f end-box 'caret)
                  (send edit kill 0 sel-start (unbox end-box)))))]
           
           [backward-kill-word
            (λ (edit event)
              (let ([sel-start (send edit get-start-position)]
                    [sel-end (send edit get-end-position)])
                (let ([start-box (box sel-start)])
                  (send edit find-wordbreak start-box #f 'caret)
                  (send edit kill 0 (unbox start-box) sel-end))))]
           [copy-click-region
            (λ (edit event)
              (region-click/internal edit event
                            (λ (click eol start end)
                              (send edit flash-on start end)
                              (send edit copy #f 0 start end))))]
           [cut-click-region
            (λ (edit event)
              (region-click/internal edit event
                            (λ (click eol start end)
                              (send edit cut #f 0 start end))))]
           [paste-click-region
            (λ (edit event)
              (region-click/internal edit event
                            (λ (click eol start end)
                              (send edit set-position click)
                              (send edit paste-x-selection 0 click))))]
           
           [mouse-copy-clipboard
            (λ (edit event)
              (send edit copy #f (send event get-time-stamp)))]
           
           [mouse-copy-clipboard/disable-anchor
            (λ (edit event)
              (send edit set-anchor #f)
              (send edit copy #f (send event get-time-stamp)))]
           
           [mouse-paste-clipboard
            (λ (edit event)
              (send edit paste (send event get-time-stamp)))]
           
           [mouse-cut-clipboard
            (λ (edit event)
              (send edit cut #f (send event get-time-stamp)))]
           
           [select-click-word
            (λ (edit event)
              (region-click edit event
                            (λ (click eol)
                              (let ([start-box (box click)]
                                    [end-box (box click)])
                                (send edit find-wordbreak 
                                      start-box
                                      end-box
                                      'selection)
                                (send edit set-position
                                      (unbox start-box)
                                      (unbox end-box))))))]
           [select-click-line
            (λ (edit event)
              (region-click edit event
                            (λ (click eol)
                              (let* ([line (send edit position-line 
                                                 click eol)]
                                     [start (send edit line-start-position
                                                  line #f)]
                                     [end (send edit line-end-position
                                                line #f)])
                                (send edit set-position start end)))))]
           
           [goto-line
            (λ (edit event)
              (let ([num-str
                     (call/text-keymap-initializer
                      (λ ()
                        (get-text-from-user
                         (string-constant goto-line)
                         (string-constant goto-line))))])
                (when (string? num-str)
                  (let* ([possible-num (string->number num-str)]
                         [line-num (and possible-num (inexact->exact possible-num))])
                    (cond
                      [(and (number? line-num)
                            (integer? line-num)
                            (<= 1 line-num (+ (send edit last-paragraph) 1)))
                       (let ([pos (send edit paragraph-start-position 
                                        (sub1 line-num))])
                         (send edit set-position pos))]
                      [else
                       (message-box
                        (string-constant goto-line)
                        (format 
                         (string-constant goto-line-invalid-number)
                         num-str
                         (+ (send edit last-line) 1)))]))))
              
              #t)]
           [repeater
            (λ (n edit)
              (let* ([km (send edit get-keymap)]
                     [done
                      (λ ()
                        (send km set-break-sequence-callback void)
                        (send km remove-grab-key-function))])
                (send km set-grab-key-function
                      (λ (name local-km edit event)
                        (if name
                            (begin
                              (done)
                              (dynamic-wind
                               (λ ()
                                 (send edit begin-edit-sequence))
                               (λ ()
                                 (let loop ([n n])
                                   (unless (zero? n)
                                     (send local-km call-function name edit event)
                                     (loop (sub1 n)))))
                               (λ ()
                                 (send edit end-edit-sequence))))
                            (let ([k (send event get-key-code)])
                              (if (and (char? k) (char<=? #\0 k #\9))
                                  (set! n (+ (* n 10) (- (char->integer k)
                                                         (char->integer #\0))))
                                  (begin
                                    (done)
                                    (dynamic-wind
                                     (λ ()
                                       (send edit begin-edit-sequence))
                                     (λ ()
                                       (let loop ([n n])
                                         (unless (zero? n)
                                           (send edit on-char event)
                                           (loop (sub1 n)))))
                                     (λ ()
                                       (send edit end-edit-sequence)))))))
                        #t))
                (send km set-break-sequence-callback done)
                #t))]
           [make-make-repeater
            (λ (n)
              (λ (edit event)
                (repeater n edit)))]
           [current-macro '()] 
           [building-macro #f] [build-macro-km #f] [build-protect? #f]
           [show/hide-keyboard-macro-icon
            (λ (edit on?)
              (when (is-a? edit editor:basic<%>)
                (let ([frame (send edit get-top-level-window)])
                  (when (is-a? frame frame:text-info<%>)
                    (send frame set-macro-recording on?)
                    (send frame update-shown)))))]
           
           [do-macro
            (λ (edit event)
              ; If c:x;e during record, copy the old macro
              (when building-macro
                (set! building-macro (append (reverse current-macro) 
                                             (cdr building-macro))))
              (let ([bm building-macro]
                    [km (send edit get-keymap)])
                (dynamic-wind
                 (λ ()
                   (set! building-macro #f)
                   (send edit begin-edit-sequence))
                 (λ ()
                   (let/ec escape
                     (for-each
                      (λ (f)
                        (let ([name (car f)]
                              [event (cdr f)])
                          (if name
                              (unless (send km call-function name edit event #t)
                                (escape #t))
                              (send edit on-char event))))
                      current-macro)))
                 (λ ()
                   (send edit end-edit-sequence)
                   (set! building-macro bm))))
              #t)]
           [start-macro
            (λ (edit event)
              (if building-macro
                  (send build-macro-km break-sequence)
                  (letrec ([km (send edit get-keymap)]
                           [done
                            (λ ()
                              (if build-protect?
                                  (send km set-break-sequence-callback done)
                                  (begin
                                    (set! building-macro #f)
                                    (show/hide-keyboard-macro-icon edit #f)
                                    (send km set-break-sequence-callback void)
                                    (send km remove-grab-key-function))))])
                    (set! building-macro '())
                    (show/hide-keyboard-macro-icon edit #t)
                    (set! build-macro-km km)
                    (send km set-grab-key-function
                          (λ (name local-km edit event)
                            (dynamic-wind
                             (λ ()
                               (set! build-protect? #t))
                             (λ ()
                               (if name
                                   (send local-km call-function name edit event)
                                   (send edit on-default-char event)))
                             (λ ()
                               (set! build-protect? #f)))
                            (when building-macro
                              (set! building-macro 
                                    (cons (cons name event)
                                          building-macro)))
                            #t))
                    (send km set-break-sequence-callback done)))
              #t)]
           [end-macro
            (λ (edit event)
              (when building-macro
                (set! current-macro (reverse building-macro))
                (set! build-protect? #f)
                (send build-macro-km break-sequence))
              #t)]
           [delete-key
            (λ (edit event)
              (let ([kmap (send edit get-keymap)])
                (send kmap call-function
                      (if (preferences:get 'framework:delete-forward?)
                          "delete-next-character"
                          "delete-previous-character")
                      edit event #t)))]
           
           [toggle-overwrite
            (λ (edit event)
              (when (preferences:get 'framework:overwrite-mode-keybindings)
                (send edit set-overwrite-mode
                      (not (send edit get-overwrite-mode)))))]
           
           [down-into-embedded-editor
            (λ (text event)
              (let ([start (send text get-start-position)]
                    [end (send text get-end-position)])
                (when (= start end)
                  (let* ([bx (box 0)]
                         [after-snip (send text find-snip start 'after-or-none bx)])
                    (cond
                      [(and (= (unbox bx) start)
                            after-snip
                            (is-a? after-snip editor-snip%))
                       (let ([embedded-editor (send after-snip get-editor)])
                         (when (is-a? embedded-editor text%)
                           (send embedded-editor set-position 0))
                         (send embedded-editor set-caret-owner #f 'global))]
                      [else
                       (let ([before-snip (send text find-snip start 'before-or-none bx)])
                         (when (and (= (+ (unbox bx) 1) start)
                                    before-snip
                                    (is-a? before-snip editor-snip%))
                           (let ([embedded-editor (send before-snip get-editor)])
                             (when (is-a? embedded-editor text%)
                               (send embedded-editor set-position
                                     (send embedded-editor last-position)))
                             (send embedded-editor set-caret-owner #f 'global))))]))))
              #t)]
           
           [forward-to-next-embedded-editor
            (λ (text event)
              (let ([start-pos (send text get-start-position)]
                    [end-pos (send text get-end-position)])
                (when (= start-pos end-pos)
                  (let loop ([snip (send text find-snip start-pos 'after-or-none)])
                    (cond
                      [(not snip) (void)]
                      [(is-a? snip editor-snip%)
                       (send text set-position (send text get-snip-position snip))]
                      [else (loop (send snip next))]))))
              #t)]
           
           [back-to-prev-embedded-editor
            (λ (text event)
              (let ([start-pos (send text get-start-position)]
                    [end-pos (send text get-end-position)])
                (when (= start-pos end-pos)
                  (let loop ([snip (send text find-snip start-pos 'before-or-none)])
                    (cond
                      [(not snip) (void)]
                      [(is-a? snip editor-snip%)
                       (send text set-position (+ (send text get-snip-position snip) 1))]
                      [else (loop (send snip previous))]))))
              #t)]
           
           [up-out-of-embedded-editor
            (λ (text event)
              (let ([start (send text get-start-position)]
                    [end (send text get-end-position)])
                (when (= start end)
                  (let ([editor-admin (send text get-admin)])
                    (when (is-a? editor-admin editor-snip-editor-admin<%>)
                      (let* ([snip (send editor-admin get-snip)]
                             [snip-admin (send snip get-admin)])
                        (when snip-admin
                          (let ([editor (send snip-admin get-editor)])
                            (when (is-a? editor text%)
                              (let ([new-pos (send editor get-snip-position snip)])
                                (send editor set-position new-pos new-pos))
                              (send editor set-caret-owner #f 'display)))))))))
              #t)]
           
           [make-read-only
            (λ (text event)
              (send text lock #t)
              #t)]
           
           [newline
            (λ (text event)
              (send text insert "\n")
              #t)]
           
           [shift-focus
            (λ (adjust)
              (λ (text event)
                (when (is-a? text editor:basic<%>)
                  (let ([frame (send text get-top-level-window)]
                        [found-one? #f])
                    (let/ec k
                      (let ([go
                             (λ ()
                               (let loop ([obj frame])
                                 (cond
                                   [(and found-one? 
                                         (is-a? obj editor-canvas%)
                                         (is-a? (send obj get-editor) editor:keymap<%>))
                                    (send obj focus)
                                    (k (void))]
                                   [(and (is-a? obj window<%>) (send obj has-focus?))
                                    (set! found-one? #t)]
                                   [(is-a? obj area-container<%>)
                                    (for-each loop (adjust (send obj get-children)))])))])
                        (go)
                        ;;; when we get here, we either didn't find the focus anywhere,
                        ;;; or the last editor-canvas had the focus. either way,
                        ;;; the next thing should get the focus
                        (set! found-one? #t)
                        (go)))))))]
           
           [TeX-compress
            (let* ([biggest (apply max (map (λ (x) (string-length (car x))) tex-shortcut-table))])
              (define (meet s t)
                (substring s 0 (string-prefix-length s t 0)))
              (λ (text event)
                (define pos (send text get-start-position))
                (when (= pos (send text get-end-position))
                  (define slash (send text find-string "\\" 'backward pos (max 0 (- pos biggest 1))))
                  (when slash
                    (define entered (send text get-text slash pos))
                    (define completions
                      (filter (λ (shortcut) (string-prefix? entered (first shortcut)))
                              tex-shortcut-table))
                    (unless (empty? completions)
                      (define-values (replacement partial?)
                        (let ([complete-match 
                               (findf (λ (shortcut) (equal? entered (first shortcut)))
                                      completions)])
                          (if complete-match
                              (values (second complete-match) #f)
                              (if (= 1 (length completions))
                                  (values (second (first completions)) #f)
                                  (let ([tex-names (map first completions)])
                                    (values (foldl meet (first tex-names) (rest tex-names))
                                            #t))))))
                      (send text begin-edit-sequence)
                      (send text delete (if partial? slash (- slash 1)) pos)
                      (send text insert replacement)
                      (send text end-edit-sequence))))))]
           
           [greek-letters "αβγδεζηθι κλμνξοπρςστυφχψω"]
           [Greek-letters "ΑΒΓΔΕΖΗΘΙ ΚΛΜΝΞΟΠΡ ΣΤΥΦΧΨΩ"]
           ;; don't have a capital ς, just comes out as \u03A2 (or junk) 
     
           
           [find-beginning-of-line
            (λ (txt)
              (define pos-to-start-with
                (cond
                  [(= (send txt get-extend-start-position)
                      (send txt get-start-position))
                   (send txt get-end-position)]
                  [else
                   (send txt get-start-position)]))
              
              (cond
                [(is-a? txt text:basic<%>)
                 (send txt get-start-of-line pos-to-start-with)]
                [(is-a? txt text%)
                 (send txt line-start-position (send txt position-line pos-to-start-with))]
                [else #f]))]
           [beginning-of-line
             (λ (txt event)
               (define pos (find-beginning-of-line txt))
               (when pos
                 (send txt set-position pos pos)))]
           [select-to-beginning-of-line
            (λ (txt event)
              (define pos (find-beginning-of-line txt))
              (when pos
                (send txt extend-position pos)))]
           
           
           [normalize-unicode-ascii-art-box
            (λ (txt evt)
              (define start (send txt get-start-position))
              (when (= start (send txt get-end-position))
                (normalize-unicode-ascii-art-box txt start)
                (send txt set-position start)))]
           
           [widen-unicode-ascii-art-box
            (λ (txt evt)
              (define start (send txt get-start-position))
              (when (= start (send txt get-end-position))
                (widen-unicode-ascii-art-box txt start)))]
           
           [center-in-unicode-ascii-art-box
            (λ (txt evt)
              (define start (send txt get-start-position))
              (when (= start (send txt get-end-position))
                (center-in-unicode-ascii-art-box txt start)))])
      
      (λ (kmap #:alt-as-meta-keymap [alt-as-meta-keymap #f])
        (let* ([map (λ (key func) 
                      (send kmap map-function key func))]
               [map-meta (λ (key func)
                           (send-map-function-meta kmap key func
                                                   (regexp-match? has-control-regexp key)
                                                   #:alt-as-meta-keymap alt-as-meta-keymap))]
               [add (λ (name func)
                      (send kmap add-function name func))]
               [add-m (λ (name func)
                        (send kmap add-function name func)
                        (when alt-as-meta-keymap
                          (send alt-as-meta-keymap add-function name func)))])
          
          ; Map names to keyboard functions
          
          (for-each
           (λ (c) 
             (unless (equal? c #\space)
               (add (format "insert ~a" c) 
                    (λ (txt evt) (send txt insert c)))))
           (string->list (string-append greek-letters Greek-letters)))
          
          (add "normalize-unicode-ascii-art-box" normalize-unicode-ascii-art-box)
          (add "widen-unicode-ascii-art-box" widen-unicode-ascii-art-box)
          (add "center-in-unicode-ascii-art-box" center-in-unicode-ascii-art-box)
          (add "shift-focus" (shift-focus values))
          (add "shift-focus-backwards" (shift-focus reverse))
          
          (add "TeX compress" TeX-compress)
          (add "newline" newline)
          (add-m "down-into-embedded-editor" down-into-embedded-editor)
          (add-m "up-out-of-embedded-editor" up-out-of-embedded-editor)
          (add-m "forward-to-next-embedded-editor" forward-to-next-embedded-editor)
          (add-m "back-to-prev-embedded-editor" back-to-prev-embedded-editor)
          
          (add-m "toggle-overwrite (when enabled in prefs)" toggle-overwrite)
          
          (add "exit" (λ (edit event)
                        (let ([frame (send edit get-frame)])
                          (if (and frame
                                   (is-a? frame frame:standard-menus<%>))
                              (send frame file-menu:quit)
                              (bell)))))
          
          (add "ring-bell" ring-bell)
          
                    
          (add "toggle-anchor" toggle-anchor)
          (add "center-view-on-line" center-view-on-line)
          (add-m "collapse-space" collapse-space)
          (add "remove-space" remove-space)
          (add "collapse-newline" collapse-newline)
          (add "open-line" open-line)
          (add "transpose-chars" transpose-chars)
          (add-m "transpose-words" transpose-words)
          (add-m "capitalize-word" capitalize-word)
          (add-m "upcase-word" upcase-word)
          (add-m "downcase-word" downcase-word)
          (add-m "kill-word" kill-word)
          (add-m "backward-kill-word" backward-kill-word)
          
          (let loop ([n 9])
            (unless (negative? n)
              (let ([s (number->string n)])
                (add-m (string-append "command-repeat-" s)
                       (make-make-repeater n))
                (loop (sub1 n)))))
          
          (add "keyboard-macro-run-saved" do-macro)
          (add "keyboard-macro-start-record" start-macro)
          (add "keyboard-macro-end-record" end-macro)
          
          (add-m "copy-clipboard" mouse-copy-clipboard)
          (add-m "copy-clipboard/disable-anchor" mouse-copy-clipboard/disable-anchor)
          (add-m "cut-clipboard" mouse-cut-clipboard)
          (add-m "paste-clipboard" mouse-paste-clipboard)
          (add-m "copy-click-region" copy-click-region)
          (add-m "cut-click-region" cut-click-region)
          (add-m "paste-click-region" paste-click-region)
          (add-m "select-click-word" select-click-word)
          (add-m "select-click-line" select-click-line)
          
          (add-m "goto-line" goto-line)
          
          (add "delete-key" delete-key)
          
          (add "mouse-popup-menu" mouse-popup-menu)
          
          (add "make-read-only" make-read-only)
        
          (add "beginning-of-line" beginning-of-line)
          (add "select-to-beginning-of-line" select-to-beginning-of-line)
          
          ; Map keys to functions
          
          (let ([setup-mappings
                 (λ (greek-chars shift?)
                   (let loop ([i 0])
                     (when (< i (string-length greek-chars))
                       (let ([greek-char (string-ref greek-chars i)])
                         (unless (equal? greek-char #\space)
                           (let ([roman-char
                                  (integer->char
                                   (+ (char->integer #\a) i))])
                             (map (format "a:g;~a~a" 
                                          (if shift? "s:" "")
                                          roman-char)
                                  (format "insert ~a" greek-char))
                             (map (format "~~c:m:x;c:g;~a~a" 
                                          (if shift? "s:" "")
                                          roman-char)
                                  (format "insert ~a" greek-char))
                             (map (format "c:x;c:g;~a~a"
                                          (if shift? "s:" "")
                                          roman-char)
                                  (format "insert ~a" greek-char)))))
                       (loop (+ i 1)))))])
            (setup-mappings greek-letters #f)
            (setup-mappings Greek-letters #t))
          
          (map "c:x;r;a" "normalize-unicode-ascii-art-box")
          (map "c:x;r;w" "widen-unicode-ascii-art-box")
          (map "c:x;r;c" "center-in-unicode-ascii-art-box")
          
          (map "~m:c:\\" "TeX compress")
          (map "~c:m:\\" "TeX compress")
          (map "c:x;t" "TeX compress")
          
          (map "c:j" "newline")
          
          (map-meta "c:down" "down-into-embedded-editor")
          (map "a:c:down" "down-into-embedded-editor")
          (map-meta "c:up" "up-out-of-embedded-editor")
          (map "a:c:up" "up-out-of-embedded-editor")
          (map-meta "c:right" "forward-to-next-embedded-editor")
          (map "a:c:right" "forward-to-next-embedded-editor")
          (map-meta "c:left" "back-to-prev-embedded-editor")
          (map "a:c:left" "back-to-prev-embedded-editor")
          
          (map "c:c;c:g" "ring-bell")
          
          (map "c:p" "previous-line")
          (map "up" "previous-line")
          (map "s:c:p" "select-up")
          (map "s:up" "select-up")
          
          (map "c:n" "next-line")
          (map "down" "next-line")
          (map "s:c:n" "select-down")
          (map "s:down" "select-down")
          
          (map "c:e" "end-of-line")
          (map "d:right" "end-of-line")
          (map "end" "end-of-line")
          (map "s:end" "select-to-end-of-line")
          (map "s:c:e" "select-to-end-of-line")
          (map "s:d:right" "select-to-end-of-line")
          
          (map "c:a" "beginning-of-line")
          (map "d:left" "beginning-of-line")
          (map "home" "beginning-of-line")
          (map "s:home" "select-to-beginning-of-line")
          (map "s:c:a" "select-to-beginning-of-line")
          (map "s:d:left" "select-to-beginning-of-line")

          (map "c:f" "forward-character")
          (map "right" "forward-character")
          (map "s:c:f" "forward-select")
          (map "s:right" "forward-select")
          
          (map "c:b" "backward-character")
          (map "left" "backward-character")
          (map "s:c:b" "backward-select")
          (map "s:left" "backward-select")
          
          (map-meta "f" "forward-word")
          (map "c:right" "forward-word")
          (map-meta "s:f" "forward-select-word")
          (map "c:s:right" "forward-select-word")
          
          (map-meta "b" "backward-word")
          
          (map "c:left" "backward-word")
          (map-meta "s:b" "backward-select-word")
          (map "c:s:left" "backward-select-word")
          
          (map-meta "<" "beginning-of-file")
          (map "d:up" "beginning-of-file")
          (map "c:home" "beginning-of-file")
          (map "s:c:home" "select-to-beginning-of-file")
          (map "s:d:up" "select-to-beginning-of-file")
          
          (map-meta ">" "end-of-file")
          (map "d:down" "end-of-file")
          (map "c:end" "end-of-file")
          (map "s:c:end" "select-to-end-of-file")
          (map "s:d:down" "select-to-end-of-file")
          
          (map "c:v" "next-page")
          (map "pagedown" "next-page")
          (map "c:down" "next-page")
          (map "s:c:v" "select-page-down")
          (map "s:pagedown" "select-page-down")
          (map "s:c:down" "select-page-down")
          
          (map-meta "v" "previous-page")
          (map "pageup" "previous-page")
          (map "c:up" "previous-page")
          (map-meta "s:v" "select-page-up")
          (map "s:pageup" "select-page-up")
          (map "s:c:up" "select-page-up")
          
          (map "c:h" "delete-previous-character")
          (map "c:d" "delete-next-character")
          (map "del" "delete-key")
          
          (map-meta "d" "kill-word")
          (map-meta "del" "kill-word")
          (map-meta "backspace" "backward-kill-word")
          (map-meta "c" "capitalize-word")
          (map-meta "u" "upcase-word")
          (map-meta "l" "downcase-word")
          
          (map "c:l" "center-view-on-line")
          
          (map "c:k" "delete-to-end-of-line")
          (map "c:y" "paste-clipboard")
          (map-meta "y" "paste-next")
          (map "a:v" "paste-clipboard")
          (map "d:v" "paste-clipboard")
          (map "c:_" "undo")
          (map "c:/" "undo")
          (map (format "~a" (integer->char 31)) "undo") ; for Windows - strange
          (map "c:+" "redo")
          (map "a:z" "undo")
          (map "d:z" "undo")
          (map "c:x;u" "undo")
          (map "c:w" "cut-clipboard")
          (map "a:x" "cut-clipboard")
          (map "d:x" "cut-clipboard")
          (map-meta "w" "copy-clipboard/disable-anchor")
          (map "a:c" "copy-clipboard")
          (map "d:c" "copy-clipboard")
          
          (map "s:delete" "cut-clipboard")
          (map "c:insert" "copy-clipboard")
          (map "s:insert" "paste-clipboard")
          
          (map-meta "space" "collapse-space")
          (when (eq? (system-type) 'macosx)
            (map "a:space" "collapse-space"))
          ;(map-meta "\\" "remove-space") ; Conflicts with european keyboards.
          (map "c:x;c:o" "collapse-newline")
          (map "c:o" "open-line")
          (map "c:t" "transpose-chars")
          (map-meta "t" "transpose-words")
          
          (map "c:space" "toggle-anchor")
          
          (map "insert" "toggle-overwrite (when enabled in prefs)")
          (map-meta "o" "toggle-overwrite (when enabled in prefs)")
          
          (map-meta "g" "goto-line")
          
          (map "c:u" "command-repeat-0")
          (let loop ([n 9])
            (unless (negative? n)
              (let ([s (number->string n)])
                (map-meta s (string-append "command-repeat-" s))
                (loop (sub1 n)))))
          
          (map "c:x;e" "keyboard-macro-run-saved")
          (map "c:x;(" "keyboard-macro-start-record")
          (map "c:x;)" "keyboard-macro-end-record")
          
          (map "leftbuttontriple" "select-click-line")
          (map "leftbuttondouble" "select-click-word")
          
          ;; the "roller ball" mice map clicking the ball to button 2.
          (unless (eq? (system-type) 'windows)
            (map "middlebutton" "paste-click-region"))
          
          (map ":rightbuttonseq" "mouse-popup-menu")
          
          (map "c:c;c:r" "make-read-only")
          
          (map "c:x;o" "shift-focus")
          (map "c:x;p" "shift-focus-backwards")
          (map "c:f6" "shift-focus")
          (map "a:tab" "shift-focus")
          (map "a:s:tab" "shift-focus-backwards")))))
  
  (define setup-search
    (let* ([send-frame
            (λ (invoke-method)
              (λ (edit event)
                (let ([frame
                       (cond
                         [(is-a? edit editor<%>)
                          (let ([canvas (or (send edit get-active-canvas)
                                            (send edit get-canvas))])
                            (and canvas
                                 (send canvas get-top-level-window)))]
                         [(is-a? edit area<%>)
                          (send edit get-top-level-window)]
                         [else #f])])
                  (if frame
                      (invoke-method frame)
                      (bell)))
                #t))])
      (λ (kmap #:alt-as-meta-keymap [alt-as-meta-keymap #f])
        (let* ([map (λ (key func) 
                      (send kmap map-function key func))]
               [map-meta (λ (key func)
                           (send-map-function-meta kmap key func
                                                   (regexp-match? has-control-regexp key)
                                                   #:alt-as-meta-keymap alt-as-meta-keymap))]
               [add (λ (name func)
                      (send kmap add-function name func))]
               [add-m (λ (name func)
                        (send kmap add-function name func)
                        (when alt-as-meta-keymap
                          (send alt-as-meta-keymap add-function name func)))])
          
          (add-m "search forward" 
                 (send-frame (λ (f) (send f search 'forward))))
          (add "search backward" 
               (send-frame (λ (f) (send f search 'backward))))
          (add "replace & search forward" 
               (send-frame (λ (f) (send f replace&search 'forward))))
          (add "replace & search backward" 
               (send-frame (λ (f) (send f replace&search 'backward))))
          (add "unhide search and toggle focus"
               (send-frame (λ (f) (send f unhide-search-and-toggle-focus))))
          (add "hide-search" 
               (send-frame (λ (f) (send f hide-search))))
          
          (map "c:g" "hide-search")
          (map "f3" "unhide search and toggle focus")
          (map "c:s" "unhide search and toggle focus")
          (map "c:r" "search backward")
          (case (system-type)
            [(unix)
             (map-meta "%" "search forward")])))))
  
  (define setup-file
    (let* ([get-outer-editor ;; : text% -> text%
            ;; returns the outermost editor, if this editor is nested in an editor snip.
            (λ (edit)
              (let loop ([edit edit])
                (let ([admin (send edit get-admin)])
                  (cond
                    [(is-a? admin editor-snip-editor-admin<%>)
                     (loop (send (send (send admin get-snip) get-admin) get-editor))]
                    [else edit]))))]
           [save-file-as
            (λ (this-edit event)
              (let ([edit (get-outer-editor this-edit)])
                (parameterize ([finder:dialog-parent-parameter 
                                (and (is-a? edit editor:basic<%>)
                                     (send edit get-top-level-window))])
                  (let ([file (finder:put-file)])
                    (when file
                      (send edit save-file/gui-error file)))))
              #t)]
           [save-file
            (λ (this-edit event)
              (let ([edit (get-outer-editor this-edit)])
                (if (send edit get-filename)
                    (send edit save-file/gui-error)
                    (save-file-as edit event)))
              #t)]
           [load-file
            (λ (edit event)
              (define (fallback)
                (let ([fn (send edit get-filename)])
                  (handler:open-file
                   (and fn
                        (let-values ([(base name dir) (split-path fn)])
                          base)))))
              (cond
                [(is-a? edit editor:basic<%>)
                 (define fr (send edit get-top-level-window))
                 (cond
                   [(is-a? fr frame:standard-menus<%>)
                    (send fr file-menu:open-callback 
                          (send fr file-menu:get-open-item)
                          event)]
                   [else (fallback)])]
                [else (fallback)])
              #t)])
      (λ (kmap #:alt-as-meta-keymap [alt-as-meta-keymap #f])
        (let* ([map (λ (key func) 
                      (send kmap map-function key func))]
               [map-meta (λ (key func)
                           (send-map-function-meta kmap key func 
                                                   (regexp-match? has-control-regexp key)
                                                   #:alt-as-meta-keymap alt-as-meta-keymap))]
               [add (λ (name func)
                      (send kmap add-function name func))]
               [add-m (λ (name func)
                        (send kmap add-function name func)
                        (when alt-as-meta-keymap
                          (send alt-as-meta-keymap add-function name func)))])
          
          (add "save-file" save-file)
          (add "save-file-as" save-file-as)
          (add "load-file" load-file)
          
          (map "c:x;c:s" "save-file")
          (map "d:s" "save-file")
          (map "c:x;c:w" "save-file-as")
          (map "c:x;c:f" "load-file")))))
  
  (define (setup-editor kmap)
    (let ([add/map
           (λ (func op key)
             (send kmap add-function
                   func
                   (λ (editor evt)
                     (send editor do-edit-operation op)))
             (send kmap map-function
                   (string-append
                    (case (system-type)
                      [(macosx macos) "d:"]
                      [(windows unix) "c:"]
                      [else (error 'keymap.rkt "unknown platform: ~s" (system-type))])
                    key)
                   func))])
      (add/map "editor-undo" 'undo "z")
      (unless (eq? (system-type) 'macosx)
        (add/map "editor-redo" 'redo "y"))
      (add/map "editor-cut" 'cut "x")
      (add/map "editor-copy" 'copy "c")
      (add/map "editor-paste" 'paste "v")
      (add/map "editor-select-all" 'select-all "a")))
  
  (define (generic-setup keymap)
    (add-editor-keymap-functions keymap)
    (add-pasteboard-keymap-functions keymap)
    (add-text-keymap-functions keymap))
  
  (define user-keymap (make-object aug-keymap%))
  (define (get-user) user-keymap)
  
  (define global (make-object aug-keymap%))
  (define global-main (make-object aug-keymap%))
  (define global-alt-as-meta (make-object aug-keymap%))
  (send global chain-to-keymap global-main #f)
  (generic-setup global-main)
  (generic-setup global-alt-as-meta)
  (setup-global global-main #:alt-as-meta-keymap global-alt-as-meta)
  (define (get-global) global)
  
  (define file (make-object aug-keymap%))
  (define file-alt-as-meta (make-object aug-keymap%))
  (generic-setup file)
  (setup-file file #:alt-as-meta-keymap file-alt-as-meta)
  (define (-get-file) file)
  
  (define search (make-object aug-keymap%))
  (define search-alt-as-meta (make-object aug-keymap%))
  (generic-setup search)
  (setup-search search #:alt-as-meta-keymap search-alt-as-meta)
  (define (get-search) search)
  
  (define editor (make-object aug-keymap%))
  (setup-editor editor)
  (define (get-editor) editor)
  
  (preferences:set-default 'framework:alt-as-meta #f boolean?)
  (define (adjust-alt-as-meta on?)
    (send global-main remove-chained-keymap global-alt-as-meta)
    (send file remove-chained-keymap file-alt-as-meta)
    (send search remove-chained-keymap search-alt-as-meta)
    (when on?
      (send global-main chain-to-keymap global-alt-as-meta #f)
      (send file chain-to-keymap file-alt-as-meta #f)
      (send search chain-to-keymap search-alt-as-meta #f)))
  (preferences:add-callback 'framework:alt-as-meta
                            (λ (p v) (adjust-alt-as-meta v)))
  (adjust-alt-as-meta (preferences:get 'framework:alt-as-meta))
  
  (define (call/text-keymap-initializer thunk)
    (let ([ctki (current-text-keymap-initializer)])
      (parameterize ([current-text-keymap-initializer
                      (λ (keymap)
                        (send keymap chain-to-keymap global #t)
                        (ctki keymap))])
        (thunk))))

  (define (region-click text event f)
    (region-click/internal text event 
                           (λ (click-pos eol start end) (f click-pos eol))))
  
  (define (region-click/internal text event f)
    (when (and (is-a? event mouse-event%)
               (send event button-down?)
               (is-a? text text%))
      (define x-box (box (send event get-x)))
      (define y-box (box (send event get-y)))
      (define eol-box (box #f))
      (send text global-to-local x-box y-box)
      (define click-pos (send text find-position 
                              (unbox x-box)
                              (unbox y-box)
                              eol-box))
      (define start-pos (send text get-start-position))
      (define end-pos (send text get-end-position))
      (define eol (unbox eol-box))
      (if (< start-pos click-pos)
          (f click-pos eol start-pos click-pos)
          (f click-pos eol click-pos end-pos)))))

(define (widen-unicode-ascii-art-box t orig-pos)
  (define start-pos (scan-for-start-pos t orig-pos))
  (when start-pos 
    (send t begin-edit-sequence)
    (define-values (start-x start-y) (pos->xy t orig-pos))
    (define min-y #f)
    (define max-y #f)
    (trace-unicode-ascii-art-box 
     t start-pos #f 
     (λ (pos x y i-up? i-dn? i-lt? i-rt?)
       (when (= x start-x)
         (unless min-y
           (set! min-y y)
           (set! max-y y))
         (set! min-y (min y min-y))
         (set! max-y (max y max-y)))))
    (define to-adjust 0)
    (for ([y (in-range max-y (- min-y 1) -1)])
      (define-values (pos char) (xy->pos t start-x y))
      (when (< pos start-pos)
        (set! to-adjust (+ to-adjust 1)))
      (send t insert 
            (cond
              [(member char lt-chars) #\═]
              [else #\space])
            pos pos))
    (send t set-position (+ orig-pos to-adjust 1) (+ orig-pos to-adjust 1))
    (send t end-edit-sequence)))

(define (normalize-unicode-ascii-art-box t pos)
  (define start-pos (scan-for-start-pos t pos))
  (when start-pos
    (send t begin-edit-sequence)
    (trace-unicode-ascii-art-box 
     t start-pos #f 
     (λ (pos x y i-up? i-dn? i-lt? i-rt?)
       (cond
         [(and i-up? i-dn? i-lt? i-rt?) (set-c t pos "╬")]
         [(and i-dn? i-lt? i-rt?)       (set-c t pos "╦")]
         [(and i-up? i-lt? i-rt?)       (set-c t pos "╩")]
         [(and i-up? i-dn? i-rt?)       (set-c t pos "╠")]
         [(and i-up? i-dn? i-lt?)       (set-c t pos "╣")]
         [(and i-up? i-lt?)             (set-c t pos "╝")]
         [(and i-up? i-rt?)             (set-c t pos "╚")]
         [(and i-dn? i-lt?)             (set-c t pos "╗")]
         [(and i-dn? i-rt?)             (set-c t pos "╔")]
         [(or i-up? i-dn?)              (set-c t pos "║")]
         [else                          (set-c t pos "═")])))
     (send t end-edit-sequence)))

(define (center-in-unicode-ascii-art-box txt insertion-pos)
  (define (find-something start-pos inc char-p?)
    (define-values (x y) (pos->xy txt start-pos))
    (let loop ([pos start-pos])
      (cond
        [(char-p? (send txt get-character pos))
         pos]
        [else
         (define new-pos (inc pos))
         (cond
           [(<= 0 new-pos (send txt last-position))
            (define-values (x2 y2) (pos->xy txt new-pos))
            (cond
              [(= y2 y)
               (loop new-pos)]
              [else #f])]
           [else #f])])))
  
  (define (adjust-space before-space after-space pos)
    (cond
      [(< before-space after-space) 
       (send txt insert (make-string (- after-space before-space) #\space) pos pos)]
      [(> before-space after-space) 
       (send txt delete pos (+ pos (- before-space after-space)))]))
  
  (define left-bar (find-something insertion-pos sub1 (λ (x) (equal? x #\║))))
  (define right-bar (find-something insertion-pos add1 (λ (x) (equal? x #\║))))
  (when (and left-bar right-bar (< left-bar right-bar))
    (define left-space-edge (find-something (+ left-bar 1) add1 (λ (x) (not (char-whitespace? x)))))
    (define right-space-edge (find-something (- right-bar 1) sub1 (λ (x) (not (char-whitespace? x)))))
    (when (and left-space-edge right-space-edge)
      (define before-left-space-count (- left-space-edge left-bar 1))
      (define before-right-space-count (- right-bar right-space-edge 1))
      (define tot-space (+ before-left-space-count before-right-space-count))
      (define after-left-space-count (floor (/ tot-space 2)))
      (define after-right-space-count (ceiling (/ tot-space 2)))
      (send txt begin-edit-sequence)
      (adjust-space before-right-space-count after-right-space-count (+ right-space-edge 1))
      (adjust-space before-left-space-count after-left-space-count (+ left-bar 1))
      (send txt end-edit-sequence))))

(define (trace-unicode-ascii-art-box t start-pos only-double-barred-chars? f)
  (define visited (make-hash))
  (let loop ([pos start-pos])
    (unless (hash-ref visited pos #f)
      (hash-set! visited pos #t)
      (define-values (x y) (pos->xy t pos))
      (define c (send t get-character pos))
      (define-values (up upc) (xy->pos t x (- y 1)))
      (define-values (dn dnc) (xy->pos t x (+ y 1)))
      (define-values (lt ltc) (xy->pos t (- x 1) y))
      (define-values (rt rtc) (xy->pos t (+ x 1) y))
      (define (interesting-dir? dir-c dir-chars) 
        (or (and (not only-double-barred-chars?)
                 (member dir-c adjustable-chars)
                 (member c dir-chars))
            (and (member dir-c double-barred-chars)
                 (member c double-barred-chars))))
      (define i-up? (interesting-dir? upc up-chars))
      (define i-dn? (interesting-dir? dnc dn-chars))
      (define i-lt? (interesting-dir? ltc lt-chars))
      (define i-rt? (interesting-dir? rtc rt-chars))
      (f pos x y i-up? i-dn? i-lt? i-rt?)
      (when i-up? (loop up))
      (when i-dn? (loop dn))
      (when i-lt? (loop lt))
      (when i-rt? (loop rt)))))

(define (scan-for-start-pos t pos)
  (define-values (x y) (pos->xy t pos))
  (findf
   (λ (p) (adj? t p))
   (for*/list ([xadj '(0 -1)]
               [yadj '(0 -1 1)])
     (define-values (d dc) (xy->pos t (+ x xadj) (+ y yadj)))
     d)))
       
(define (adj? t pos)
  (and pos 
       (member (send t get-character pos) 
               adjustable-chars)))

(define (set-c t pos s)
  (unless (equal? (string-ref s 0) (send t get-character pos))
    (send t delete pos (+ pos 1))
    (send t insert s pos pos)))

(define (pos->xy text pos)
  (define para (send text position-paragraph pos))
  (define start (send text paragraph-start-position para))
  (values (- pos start) para))

(define (xy->pos text x y)
  (cond
    [(and (<= 0 x) (<= 0 y (send text last-paragraph)))
     (define para-start (send text paragraph-start-position y))
     (define para-end (send text paragraph-end-position y))
     (define pos (+ para-start x))
     (define res-pos
       (and (< pos para-end)
            ;; the newline at the end of the
            ;; line is not on the line, so use this guard
            pos))
     (if res-pos
         (values res-pos (send text get-character res-pos))
         (values #f #f))]
    [else (values #f #f)]))

(module+ test
  (require rackunit 
           racket/gui/base)
  (define sa string-append)
  
  (define (first-value-xy->pos a b c)
    (define-values (d e) (xy->pos a b c))
    d)
  
  (let ([t (new text%)])
    (send t insert (sa "abc\n"
                       "d\n"
                       "ghi\n"))
    (check-equal? (first-value-xy->pos t 0 0) 0)
    (check-equal? (first-value-xy->pos t 1 0) 1)
    (check-equal? (first-value-xy->pos t 0 1) 4)
    (check-equal? (first-value-xy->pos t 3 0) #f)
    (check-equal? (first-value-xy->pos t 0 3) #f)
    (check-equal? (first-value-xy->pos t 1 1) #f)
    (check-equal? (first-value-xy->pos t 2 1) #f)
    (check-equal? (first-value-xy->pos t 0 2) 6)
    (check-equal? (first-value-xy->pos t 1 2) 7)
    (check-equal? (first-value-xy->pos t 2 -1) #f)
    (check-equal? (first-value-xy->pos t -1 0) #f)
    (check-equal? (first-value-xy->pos t 2 2) 8)
    (check-equal? (first-value-xy->pos t 2 3) #f))
  
  (let ([t (new text%)])
    (send t insert (sa "abc\n"
                       "d\n"
                       "ghi"))
    (check-equal? (first-value-xy->pos t 2 2) 8)
    (check-equal? (first-value-xy->pos t 2 3) #f))
  
  (let ([t (new text%)])
    (send t insert (string-append "+-+\n"
                                  "| |\n"
                                  "+-+\n"))
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╗\n"
                   "║ ║\n"
                   "╚═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append "+=+\n"
                                  "| |\n"
                                  "+=+\n"))
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╗\n"
                   "║ ║\n"
                   "╚═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append "+-+-+\n"
                                  "| | |\n"
                                  "+-+-+\n"
                                  "| | |\n"
                                  "+-+-+\n"))
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═╦═╗\n"
                   "║ ║ ║\n"
                   "╠═╬═╣\n"
                   "║ ║ ║\n"
                   "╚═╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═══╗\n"
                    "║ - ║\n"
                    "╚═══╝\n"))
    
    (normalize-unicode-ascii-art-box t 0)
    (check-equal? (send t get-text)
                  (string-append
                   "╔═══╗\n"
                   "║ - ║\n"
                   "╚═══╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"
                    "╚═╩═╝\n"))
    (send t set-position 1 1)
    (widen-unicode-ascii-art-box t 1)
    (check-equal? (send t get-start-position) 2)
    (check-equal? (send t get-text)
                  (string-append
                   "╔══╦═╗\n"
                   "║  ║ ║\n"
                   "╠══╬═╣\n"
                   "║  ║ ║\n"
                   "╚══╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"
                    "╚═╩═╝\n"))
    (send t set-position 8 8)
    (widen-unicode-ascii-art-box t 8)
    (check-equal? (send t get-start-position) 10)
    (check-equal? (send t get-text)
                  (string-append
                   "╔══╦═╗\n"
                   "║  ║ ║\n"
                   "╠══╬═╣\n"
                   "║  ║ ║\n"
                   "╚══╩═╝\n")))
  
  (let ([t (new text%)])
    (send t insert (string-append
                    "╔═╦═╗\n"
                    "║ ║ ║\n"
                    "╠═╬═╣\n"
                    "║ ║ ║\n"))
    (send t set-position 8 8)
    (widen-unicode-ascii-art-box t 8)
    (check-equal? (send t get-text)
                  (string-append
                   "╔══╦═╗\n"
                   "║  ║ ║\n"
                   "╠══╬═╣\n"
                   "║  ║ ║\n")))
  
  (let ([t (new text%)])
    (send t insert "║ x   ║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║  x  ║\n"))
  
  (let ([t (new text%)])
    (send t insert "║x    ║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║  x  ║\n"))
  
  (let ([t (new text%)])
    (send t insert "║    x║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║  x  ║\n"))
  
  (let ([t (new text%)])
    (send t insert "║abcde║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║abcde║\n"))
  
  (let ([t (new text%)])
    (send t insert "║║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║║\n"))
  
  (let ([t (new text%)])
    (send t insert "║abcde \n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  "║abcde \n"))
  
  (let ([t (new text%)])
    (send t insert " abcde║\n")
    (center-in-unicode-ascii-art-box t 1)
    (check-equal? (send t get-text)
                  " abcde║\n"))
  
  )
