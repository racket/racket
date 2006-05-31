#cs
(module tool mzscheme
  (require "class.scm"
           "union.ss"
           "view.scm"
           "draw-txt.ss"
           "data-defs.scm"
           (lib "tool.ss" "drscheme")
           (only (lib "drsig.ss" "drscheme" "private") drscheme:language-configuration^)
           (lib "framework.ss" "framework")
           (lib "mred.ss" "mred")
           (lib "unitsig.ss") 
           (lib "etc.ss")
           (lib "class.ss")
	   (lib "string-constant.ss" "string-constants")
           (lib "contract.ss"))
  
  (provide tool@)
  
  ;; Strings
  (define INSERT-JAVA-CLASS "Insert Java Class")
  (define INSERT-JAVA-UNION "Insert Java Union")
  
  (define tool@
    (unit/sig drscheme:tool-exports^
      (import drscheme:tool^)
      
      (define (phase1) (void))
      (define (phase2) (void))
      
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;;
      ;;  Wire up to DrScheme
      ;;
      
      ;; insert a Java Class
      
      (define (java-class-wizard-mixin %)
        (class %
          (inherit get-special-menu get-edit-target-object register-capability-menu-item)
          
          (super-new)
          
          (define (tee x) x)

          #;(define-syntax tee
            (syntax-rules ()
              ((_ x)
               (let* ([a (format "--> ~a\n" 'x)]
                      [y x]
                      [b (format "==> ~a\n" y)])
                 (message-box "error" (format "~a~a" a b))
                 y))))
          
          ;; String (LANGUAGE-LEVEL -> X) (X ... -> String) (X ... -> String) -> Void
          ;; create a menu item for inserting classes and interfaces 
          (define (make-menu-item% descr get-info make draw)
            (define (A menu event)
              ;; ---------------------------------------------------------------
              ;; does the current language need 'public' for 'interface methods'
              (define foo (send this get-current-tab))
              (define bar (send foo get-defs))
              (define moo (send bar get-next-settings))
              (define koo 
                (drscheme:language-configuration:language-settings-language moo))
              (define current-language* (tee (send koo get-language-position)))
              ;; ---------------------------------------------------------------
              
              (define language-level 
                (let ([simple* (member "ProfessorJ" current-language*)])
                  (cond
                    [(boolean? simple*) PROFESSIONAL]
                    [(eq? (cadr simple*) BEGINNER) BEGINNER]
                    [(eq? (cadr simple*) INTERMEDIATE) INTERMEDIATE]
                    [else PROFESSIONAL])))
              
              ;; get the editor and insert the desired items ... 
              (define editor (get-edit-target-object))
              (define-values (b class-as-info) (get-info language-level))
              ;; ... the class 
              (when class-as-info
                (let ([class-as-text (apply make (append class-as-info (list language-level)))])
                  (when b 
                    ;; ... the diagram 
                    (send editor insert (format "/*~n~a~n*/~n~n" (draw (car class-as-info)))))
                  (send editor insert class-as-text))))
            (new menu-item% (label descr) (parent (get-special-menu)) (callback A)))
          
          (make-menu-item% INSERT-JAVA-CLASS get-class-info make-class class-draw)
          (register-capability-menu-item 'profjWizard:special:java-class (get-special-menu))
          (make-menu-item% INSERT-JAVA-UNION get-union-info make-union dt-draw)
          (register-capability-menu-item 'profjWizard:special:java-union (get-special-menu))))
      
      (drscheme:get/extend:extend-unit-frame java-class-wizard-mixin)
      (drscheme:language:register-capability 'profjWizard:special:java-class 
                                             (flat-contract boolean?) #f)
      (drscheme:language:register-capability 'profjWizard:special:java-union
                                             (flat-contract boolean?) #f)
      ))
  
  )
