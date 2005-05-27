;;; CustomSwindle
;;;   Name:       CustomSwindle
;;;   DialogName: Customized Swindle
;;;   OneLine:    Sample Customized Swindle
;;;   URL:        http://www.barzilay.org/Swindle/

;;; This file demonstrates how a customized Swindle-based language can be
;;; created.  Most of these things could be done with the GUI language
;;; customizing, but (a) it will make it very verbose, (b) most syntax settings
;;; are things that beginners should not know about, (c) it will not allow
;;; things like the redefinition of `lambda' which is done below.  To make a
;;; customization file, it should be some *.ss file in this directory, that
;;; begins in the same way as above commented prefix: beginning with the magic
;;; string, and then specifying some parameters for this language.  Specifying
;;; the language's name as it appears at the top of the interactions menu
;;; (defaults to the file name minus the ".ss"), the name as it appears in the
;;; language selection dialog box (defaults to the Name), the one-line
;;; description (appears at the bottom of the language dialog), and a URL to
;;; jump to when the name in the interactions is clicked.  Remember that since
;;; the language can be pretty different than Swindle, then appropriate
;;; documentation should be added too.
;;;
;;; This is a good place to add common functionality and customizations, but
;;; not things that can be made into a module -- a teachpack is better for
;;; those.

(module custom (lib "swindle.ss" "swindle")
  ;; provide all swindle, minus `lambda' which is overriden to `method'
  (provide (all-from-except (lib "swindle.ss" "swindle") lambda))
  (provide (rename lambda~ lambda))
  (defsubst lambda~ method)
  ;; some default customizations
  (*make-safely* #t)
  ;; set some syntax parameters -- must use eval!
  (eval #'(begin
            ;; simple defclass forms:
            (-defclass-auto-initargs-
             (;; auto acccessors, constructors, and predicates
              :auto #t
              ;; first two things after a slot name are type and initvalue
              :default-slot-options '(:type :initvalue)
              ;; printed representation of objects shows slot contents
              :printer print-object-with-slots))
            ;; set the accessor names made by the above
            (-defclass-autoaccessors-naming- :class-slot)
            ;; always use an explicit generic
            (-defmethod-create-generics- #f)
            ;; use defgeneric + add-method for accessors (since defmethod now
            ;; wouldn't create the generic)
            (-defclass-accessor-mode- :defgeneric))))

;;; To make thins even better, it is best to change preferences so Swindle
;;; syntax get indented correctly.  For this, create the default preference
;;; file "plt/collects/defaults/plt-prefs.ss", and in it you can put any
;;; specific preferences you want as the defaults for people who run the system
;;; for the first time (see the "Preference Files" section in the Help Desk).
;;; The two relevant settings are -- make Swindle the default language:
;;;   (drscheme:205-settings
;;;     (("Swindle" "Full Swindle")
;;;      #6(#f current-print mixed-fraction-e #f #t debug)))
;;; And to make indentation handle Swindle forms correctly, locate the tab
;;; specifications line and add the swindle forms indentation:
;;;   (framework:tabify
;;;    (... stuff which is already there ...
;;;     (define* define) (define-syntax* define) (defsyntax define)
;;;     (defsyntax* define) (letsyntax lambda) (defsubst define)
;;;     (defsubst* define) (letsubst lambda) (defmacro define)
;;;     (defmacro* define) (letmacro lambda) (named-lambda lambda)
;;;     (thunk lambda) (while lambda) (until lambda) (dotimes lambda)
;;;     (dolist lambda) (no-errors lambda) (regexp-case lambda)
;;;     (generic lambda) (defgeneric define) (method lambda)
;;;     (named-method lambda) (qualified-method lambda) (defmethod define)
;;;     (beforemethod lambda) (aftermethod lambda) (aroundmethod lambda)
;;;     (defbeforemethod define) (defaftermethod define)
;;;     (defaroundmethod define) (class lambda) (entityclass lambda)
;;;     (defclass define) (defentityclass define) (defgeneric* define)
;;;     (defclass* define) (defentityclass* define) (with-slots lambda)
;;;     (with-accessors lambda) (matcher lambda) (match lambda)
;;;     (defmatcher define) (defmatcher0 define)))
