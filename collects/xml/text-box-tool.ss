(module text-box-tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           (lib "unitsig.ss")
           (lib "class.ss")
	   (lib "contract.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "include-bitmap.ss" "mrlib"))
  
  (provide tool@)

  ;; chunk-string: (listof any) -> (listof any)
  (define (chunk-string s acc)
    (cond
      ((and (null? s) (null? acc)) null)
      ((null? s) (list (list->string (reverse acc))))
      ((char? (car s)) (chunk-string (cdr s) (cons (car s) acc)))
      ((null? acc) (cons (car s) (chunk-string (cdr s) null)))
      (else (cons (list->string (reverse acc)) (cons (car s) (chunk-string (cdr s) null))))))
              
  ;; marshall: writable -> string
  (define (marshall s)
    (let ((os (open-output-string)))
      (with-handlers ((exn:fail? (lambda (x) "")))
        (write s os)
        (get-output-string os))))

  (define icon
    (let* ((x (make-object bitmap% 10 10))
           (y (make-object bitmap-dc% x)))
      (send y set-font (make-object font% 24 'default 'normal 'normal ))
      (send y draw-text "\"" 0 0)
      x))
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define snipclass-text-box%
        (class decorated-editor-snipclass%
          (define/override (make-snip stream-in) (new text-box%))
          (super-instantiate ())))
      
      (define snipclass (new snipclass-text-box%))
      (send snipclass set-version 1)
      (send snipclass set-classname "text-box%")
      (send (get-the-snip-class-list) add snipclass)

      (define text-box%
        (class* decorated-editor-snip% (readable-snip<%>)          
          (define/override (make-editor) (new text:keymap%))
          (define/override (make-snip) (make-object text-box%))
          (inherit get-editor get-admin)
          
          
          (define/override (get-corner-bitmap)
            icon)

          (define/override (get-menu)
            (let ([menu (new popup-menu%)])
              (new menu-item%
                   (label "Convert to string")
                   (parent menu)
                   (callback
                    (lambda (x y)
                      (let ([to-ed (find-containing-editor)])
                        (when to-ed
                          (let ([this-pos (find-this-position)])
                            (when this-pos
                              (let ([from-ed (get-editor)])
                                (send to-ed begin-edit-sequence)
                                (send from-ed begin-edit-sequence)
                                (send to-ed delete this-pos (+ this-pos 1))
                                (let* ((p (open-input-text-editor from-ed 0 'end
                                                                  (lambda (s)
                                                                    (values (box s) 1))))
                                       (contents
                                        (let loop ((next (read-char-or-special p)))
                                          (cond
                                            ((eof-object? next) null)
                                            (else
                                             (cons next (loop (read-char-or-special p)))))))
                                       (repaired-contents
                                        (map (lambda (x)
                                               (if (string? x)
                                                   (marshall x)
                                                   (send (unbox x) copy)))
                                             (chunk-string contents null))))
                                  (for-each
                                   (lambda (x)
                                     (send to-ed insert x this-pos))
                                   (reverse repaired-contents)))
                                (send to-ed end-edit-sequence)
                                (send from-ed end-edit-sequence)))))))))
              menu))
          
          ;; find-containing-editor : -> (union #f editor)
          (define/private (find-containing-editor)
            (let ([admin (get-admin)])
              (and admin
                   (send admin get-editor))))
          
          ;; find-this-position : -> (union #f number)
          (define/private (find-this-position)
            (let ([ed (find-containing-editor)])
              (and ed
                   (send ed get-snip-position this))))

          ;; input-port -> (union (listof char) char eof-object? syntax-object)
          (define/private (get-next port)
	    (let ([v (read-char-or-special port)])
	      (if (special-comment? v)
		  (get-next port)
		  v)))
          
          (define/public (read-special source line column position)
            (let* ((ed (get-editor))
                   (port (open-input-text-editor ed))
                   (str (let loop ((next (get-next port)))
                          (cond
                            ((eof-object? next) null)
                            ((char? next)
                             (cons next (loop (get-next port))))
                            (else (cons #`(marshall #,next) (loop (get-next port))))))))
	      #`(let ((marshall
		       (lambda (s)
			 (let ((os (open-output-string)))
			   (with-handlers ((exn:fail? (lambda (x) "")))
			     (display s os)
			     (get-output-string os))))))
		  (string-append #,@(chunk-string str null)))))
          
          (super-instantiate ())
          (inherit set-snipclass)
          (set-snipclass snipclass)))
                    
            
      (define (text-box-mixin %)
        (class %
          (inherit get-special-menu get-edit-target-object register-capability-menu-item)
          (super-new)
          (new menu-item%
               (label (string-constant insert-text-box-item))
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([c-box (new text-box%)]
                        [text (get-edit-target-object)])
                    (send text insert c-box)
                    (send text set-caret-owner c-box 'global)))))
          (register-capability-menu-item 'drscheme:special:slideshow-menu-item (get-special-menu))))
      
      (drscheme:get/extend:extend-unit-frame text-box-mixin)
      
      (drscheme:language:register-capability 'drscheme:special:insert-text-box (flat-contract boolean?) #t))))
