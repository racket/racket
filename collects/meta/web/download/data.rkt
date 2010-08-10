#lang racket/base

(define -versions+dates-
  '(["5.0.1" "August 2010"]
    ["5.0"   "June 2010"]
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
    ["103"   "September 2000"]
    ["053"   "July 1998"]
    ))

(define -platform-names-
  `(;; source platforms
    ["win"  "Windows"]
    ["mac"  "Macintosh"]
    ["unix" "Unix"]
    ;; binary platforms
    ["i386-win32" "Windows x86"]
    ["(ppc|i386)-osx-mac"
     ,(lambda (_ cpu)
        (format "Macintosh OS X (~a)" (if (equal? cpu "ppc") "PPC" "Intel")))]
    ["(ppc|68k)-mac-classic"   "Macintosh Classic (\\1)"]
    ["(ppc|i386)-darwin"
     ,(lambda (_ cpu)
        (format "Macintosh Darwin (~a)"
                (if (equal? cpu "ppc") "PPC" "Intel")))]
    ["i386-linux(-gcc2)?"            "Linux (i386)"]
    ["i386-linux-fc([0-9]+)"         "Linux - Fedora Core \\1 (i386)"]
    ["(i386|x86_64)-linux-f([0-9]+)" "Linux - Fedora \\2 (\\1)"]
    ["i386-linux-debian"             "Linux - Debian Stable (i386)"]
    ["i386-linux-debian-(testing|unstable)"  "Linux - Debian \\1 (i386)"]
    ["i386-linux-ubuntu[0-9]*"       "Linux - Ubuntu (i386)"]
    ["i386-linux-ubuntu-([a-z]*)"    "Linux - Ubuntu \\1 (i386)"]
    ["i386-freebsd"                  "FreeBSD (i386)"]
    ["sparc-solaris"                 "Sparc Solaris (SunOS)"]
    ["i386-kernel"                   "x86 Standalone Kernel"]
    ))

(define -file-type-names-
  '(["sh"  "Self-extracting shell script"]
    ["exe" "Windows Installer"]
    ["tgz" "Gzipped TAR Archive"]
    ["zip" "Zipped Archive"]
    ["dmg" "Disk Image"]
    ["plt" "Racket Package"]
    ["sit" "StuffIt Archive"]))

(define -mirrors-
  ;; This is a sequence of
  ;;   (location url reposnisble-name email [techincal-contact])
  '(["Main download (USA, Massachusetts, Northeastern University)"
     "http://download.racket-lang.org/installers/"
     "Eli Barzilay"
     "eli@barzilay.org"]
    ["USA, Illinois (Northwestern University)"
     "http://www.eecs.northwestern.edu/racket/"
     "Robby Findler"
     "robby@eecs.northwestern.edu"]
    ["USA, Utah (University of Utah)"
     "http://www.cs.utah.edu/plt/installers/"
     "Matthew Flatt"
     "mflatt@cs.utah.edu"]
    ["Canada, Ontario (University of Waterloo)"
     "http://mirror.csclub.uwaterloo.ca/racket/racket-installers/"
     "Systems Committee"
     "syscom@csclub.uwaterloo.ca"]
    ["Germany (Universität Tübingen)"
     "http://mirror.informatik.uni-tuebingen.de/mirror/racket/"
     "Marcus Crestani"
     "crestani@informatik.uni-tuebingen.de"]
    ["Belgium (Infogroep, Vrije Universiteit Brussel)"
     "ftp://infogroep.be/pub/racket/installers/"
     "Infogroep"
     "research@infogroep.be"]
    ["Turkey, Istanbul (Bilgi University)"
     "http://russell.cs.bilgi.edu.tr/racket-installers/"
     "Onur Gungor"
     "onurgu@cs.bilgi.edu.tr"]
    #;
    ["Austria (Vienna University of Technology)"
     "http://gd.tuwien.ac.at/languages/scheme/plt/"
     "Rudolf Ladner"
     "ladner@zid.tuwien.ac.at"]
    #; ; Scheme guy left
    ["France (Institut Pasteur)"
     "ftp://ftp.pasteur.fr/pub/computing/Scheme/plt-scheme/"
     "Marc Badouin"
     "babafou@pasteur.fr"
     "Pasteur Institute FTP ftpmain@pasteur.fr"]
    #; ; ftp down (permanently?)
    ["Mexico (Wish Computing)"
     "ftp://morpheus.wish.com.mx/pub/plt/"
     "Francisco Solsona"
     "solsona@acm.org"]
    ))

;; ----------------------------------------------------------------------------

(provide versions+dates all-versions current-version version->date
         (struct-out mirror) mirrors
         (struct-out installer) all-installers
         package->name platform->name suffix->name)

(require racket/list racket/file version/utils racket/runtime-path)

;; ----------------------------------------------------------------------------

(define versions+dates
  (sort -versions+dates- <
        #:key (lambda (vd)
                (version->integer (regexp-replace #rx"^0+" (car vd) "")))
        #:cache-keys? #t))

(define all-versions (map car versions+dates))

(define current-version (last all-versions))

(define version->date
  (let ([t (make-hash)])
    (for ([vd (in-list versions+dates)])
      (hash-set! t (car vd) (cadr vd)))
    (lambda (v)
      (hash-ref t v (lambda ()
                      (error 'version->date "unknown version: ~e" v))))))

;; ----------------------------------------------------------------------------

(struct mirror (location url person email))

(define mirrors
  (map (lambda (m)
         (mirror (car m) (regexp-replace #rx"/?$" (cadr m) "/")
                 (caddr m) (cadddr m)))
       -mirrors-))

;; ----------------------------------------------------------------------------

(define-runtime-path installers-data "installers.txt")

(struct installer
        (path     ; path to file from the installers directory
         file     ; just the file name
         version  ; version of the installer (as a string)
         size     ; human-readable size string
         package  ; package kind symbol 'racket or 'racket-textual
         binary?  ; #t = binary distribution, #f = source distribution
         platform ; platform name string (generic for srcs, cpu-os for bins)
         suffix)) ; string

(define installer-rx
  (pregexp (string-append
            "^"
            "([0-9.]+[A-Z]+)"       ; size
            "\t"
            "("                     ; path
            "([0-9p.]+)"            ; version
            "/"
            "(racket(?:-textual)?)" ; package
            "/("                    ; file
            "\\4-\\3-"              ; <package>-<version>-
            "(bin|src)-"            ; binary/source
            "([^.]+)"               ; platform
            "\\."
            "([a-z]+)"              ; suffix
            "))$")))

(define (make-installer size path version package file type platform suffix)
  (installer path file version size (string->symbol package)
             (equal? "bin" type) platform suffix))

(define (parse-installers in)
  (port-count-lines! in)
  (for/list ([line (in-lines in)] [num (in-naturals 1)])
    (apply make-installer
           (cdr (or (regexp-match installer-rx line)
                    (error 'installers "bad installer data line#~a: ~s"
                           num line))))))

(define all-installers (call-with-input-file installers-data parse-installers))

(define package->name
  (let ([t (make-hasheq)])
    (lambda (package)
      (hash-ref! t package
        (lambda ()
          (string-titlecase
           (regexp-replace #rx"-" (symbol->string package) " ")))))))

(define platform-names
  (for/list ([pn (in-list -platform-names-)])
    (list (regexp (string-append "^" (car pn) "$")) (cadr pn))))

(define platform->name
  (let ([t (make-hash)])
    (lambda (platform)
      (hash-ref! t platform
        (lambda ()
          (or (for/or ([pn (in-list platform-names)])
                ;; find out if a regexp applied by checking if the result is
                ;; different (relies on regexp-replace returning the same
                ;; string when fails)
                (let ([new (regexp-replace (car pn) platform (cadr pn))])
                  (and (not (eq? new platform)) new)))
              (error 'platform->name "unrecognized platform: ~e"
                     platform)))))))

(define (suffix->name suffix)
  (cond [(assoc suffix -file-type-names-) => cadr]
        [else (error 'suffix->name "unrecognized suffix: ~e" suffix)]))
