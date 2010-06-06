#lang racket/base

(define -versions+dates-
  '(["5.0"   "June 2010"]
    ["4.2.5" "April 2010"]
    ["4.2.4" "January 2010"]
    ["4.2.3" "December 2009"]
    ["4.2.2" "October 2009"]
    ["4.2.1" "July 2009"]
    ["4.2"   "June 2009"]
    ["4.1.5" "March 2009"]
    ["4.1.4" "January 2009"]
    ["4.1.3" "November 2008"]
    ["4.1.2" "October 2008"]
    ["4.1.1" "October 2008"]
    ["4.1"   "August 2008"]
    ["4.0.2" "July 2008"]
    ["4.0.1" "June 2008"]
    ["4.0"   "June 2008"]
    ["372"   "December 2007"]
    ["371"   "August 2007"]
    ["370"   "May 2007"]
    ["360"   "November 2006"]
    ["352"   "July 2006"]
    ["351"   "July 2006"]
    ["350"   "June 2006"]
    ["301"   "January 2006"]
    ["300"   "December 2005"]
    ["209"   "December 2004"]
    ["208"   "August 2004"]
    ["207"   "May 2004"]
    ["206p1" "January 2004"]
    ["206"   "January 2004"]
    ["205"   "August 2003"]
    ["204"   "May 2003"]
    ["203"   "December 2002"]
    ["202"   "August 2002"]
    ["201"   "July 2002"]
    ["200"   "June 2002"]
    ["103p1" "August 2001"]
    ))

;; ----------------------------------------------------------------------------

(provide versions+dates all-versions current-version)

(require racket/list racket/file version/utils racket/runtime-path)

;; ----------------------------------------------------------------------------

(define versions+dates
  (sort -versions+dates- <
        #:key (compose version->integer car) #:cache-keys? #t))

(define all-versions (map car versions+dates))

(define current-version (last all-versions))

;; ----------------------------------------------------------------------------

(define-runtime-path installers-data "installers.txt")

(struct installer
        (path     ; path to file from the installers directory
         version  ; version of the installer (as a string)
         size     ; human-readable size string
         package  ; package kind symbol 'racket or 'racket-textual
         binary?  ; #t = binary distribution, #f = source distribution
         platform ; platform name string (generic for srcs, cpu-os for bins)
         suffix   ; string
         ))

(define installer-rx
  (pregexp (string-append
            "^"
            "([0-9.]+[A-Z]+)"       ; size
            "\t"
            "("                     ; path
            "([0-9p.]+)"            ; version
            "/"
            "(racket(?:-textual)?)" ; package
            "/\\4-\\3-"             ; <package>-<version>-
            "(bin|src)-"            ; binary/source
            "([^.]+)"               ; platform
            "\\."
            "([a-z]+)"              ; suffix
            ")$")))

(define (make-installer size path version package type platform suffix)
  (installer path version size (string->symbol package) (equal? "bin" type)
             platform suffix))

(define (parse-installers in)
  (port-count-lines! in)
  (for/list ([line (in-lines in)] [num (in-naturals 1)])
    (apply make-installer
           (cdr (or (regexp-match installer-rx line)
                    (error 'installers "bad installer data line#~a: ~s"
                           num line))))))

(define installers (call-with-input-file installers-data parse-installers))
