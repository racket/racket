#lang scheme/base
(require "prefix-dispatcher.ss"
         scheme/cmdline
         (for-syntax scheme/base))

(provide svn-style-command-line)

;; implements an "svn-style" command-line interface as a wrapper around scheme/cmdline. At the moment,
;; it is light on error-checking and makes choices that are somewhat specific to the PLaneT commandline
;; tool, thus its inclusion in planet/private rather than somewhere more visible. The idea is that you
;; write
#|

(svn-style-command-line
   #:program <name-of-the-program-string>
   #:argv <argument vector, generally (current-command-line-arguments)>
   <program-general-description string>
   [<command1> <brief-help-string> <long-help-description-listof-strings>
    ... arguments just like the command-line macro takes, until ...
    #:args formals
    body-expr] ...)
|#

;; This macro turns that into a command-line type of thing that implements
;;    program command1 ... args ...
;;    program command2 ... args ...
;; etc.
;; It provides two nonobvious features:
;;   1. It automatically includes a help feature that prints out all available subcommands
;;   2. It automatically lets users use any unambiguous prefix of any command.
;;      This means that no command name may be a prefix of any other command name, because it
;;      would mean there was no way to unambiguously name the shorter one.

(define-syntax (svn-style-command-line stx)
  (syntax-case stx ()
    [(_ #:program prog 
        #:argv args
        general-description
        [name description long-description body ... #:args formals final-expr] ...)
     (with-syntax ([(n ...) (generate-temporaries #'(name ...))])
       #'(let* ([p prog]
                [a args]
                [n name] ...
                [argslist (cond 
                            [(list? a) a]
                            [(vector? a) (vector->list a)]
                            [else (error 'command "expected a vector or list for arguments, received ~e" a)])]
                [help (λ () (display-help-message p general-description '((name description) ...)))])
           (let-values ([(the-command remainder)
                         (if (null? argslist)
                             (values "help" '())
                             (values (car argslist) (cdr argslist)))])
             (prefix-case the-command
                          [n 
                           (command-line 
                            #:program (format "~a ~a" p n)
                            #:argv remainder 
                            body ...
                            #:handlers
                            (λ (_ . formals) final-expr)
                            (pimap symbol->string 'formals)
                            (λ (help-string)
                              (for-each (λ (l) (display l) (newline)) long-description)
                              (newline)
                              (display "Usage:\n")
                              (display help-string)
                              (exit)))] ...
                          ["help" (help)]
                          [else (help)]))))]))


;; display-help-message : string (listof (list string string)) -> void
;; prints out the help message
(define (display-help-message prog general-description commands)
  (let ([maxlen (apply max (map (λ (p) (string-length (car p))) commands))])
    (begin
      (printf "Usage: ~a <subcommand> [option ...] <arg ...>\n" prog)
      (printf "[note: you can name a subcommand by typing any unambiguous prefix of it.]\n\n")
      (display general-description)
      (newline)
      (newline)
      (display "For help on a particular subcommand, type 'planet <subcommand> --help'\n")
      (display "Available subcommands:\n")
      (for-each (λ (command) 
                  (let ([padded-name (pad (car command) maxlen)]
                        [desc        (cadr command)])
                    (printf "  ~a    ~a\n" padded-name desc)))
                commands))))


;; ----------------------------------------
;; utility

;; pad : string nat[>= string-length str] -> string
;; pads the given string up to the given length.
(define (pad str n)
  (let* ([l (string-length str)]
         [extra (build-string (- n l) (λ (n) #\space))])
    (string-append str extra)))

;; pimap : (A -> B) improper-listof A -> improper-listof B 
(define (pimap f pil)
  (cond
    [(null? pil) '()]
    [(pair? pil) (cons (pimap f (car pil))
                       (pimap f (cdr pil)))]
    [else (f pil)]))