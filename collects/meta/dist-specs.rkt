#lang reader meta/spec-reader

;; -*- scheme -*-

;; ============================================================================
;; This file holds the specifications for creating Racket distributions.  These
;; specifications are defined by a sequence of <sym> := <spec>... definitions
;; (note: no parens), which binds the symbol to a tree specification.  In
;; addition, a definition can use `:=tag' which will go into a special space of
;; definitions that are used in `tag' forms.
;; Each <spec> is a form that can be a pattern string or a combination.
;; Pattern strings are matched recursively over path trees and they can:
;;   - contain shell-glob chars ("*" (will not match "/"s), "?", ranges),
;;   - the shell-globbing is extended with "{|}" which are used for alternative
;;     parts (the braces are translated to regexp parens),
;;   - have a "/" prefix to anchor the pattern at the path beginning,
;;   - have a "/" suffix to restrict the pattern to directories,
;;   - contain a "/**/" pattern to match over arbitrary directories nesting.
;; Pattens can be combined with a few primitive operators than can be taken as
;; operations over either predicates or sets:
;;   - `and', `or', `not' have usual meaning (can be considered as combining
;;     predicate functions or set operations),
;;   - `none', `all' are an always-false and always-true (useful for `cond's).
;; Expanding specs works as if the language is always splicing-in definitions,
;; which has no effects on most expression.  It does have an effect when a
;; symbol is defined as a sequence of specs, and in the conditional forms
;; below.
;; There are a few special spec forms that can be used to conditionalize
;; expressions:
;;   - (cond <cond> => <spec> ...
;;           ...
;;           else => <spec>)
;;     This is a conditional form: the condition can be a symbol which is true
;;     if this spec is achieved through a usage of a `tag' form (see below).
;;     `else' is the default condition.  The resulting spec(s) are spliced into
;;     the form they were used in -- which means that this form is used to
;;     modify an embedding spec form, unlike any of the above.  If no <cond>
;;     holds and no `else' is used, the `cond' form just disappears.  The
;;     condition can contain `and', `or', and `not' expressions.  A common
;;     idiom is:
;;       (- foo (cond (not bar) => baz))
;;     meaning that `foo' is used without `baz' when `bar' doesn't hold.
;;   - (cond* <cond> => <spec> ...
;;             ...)
;;     This is similar to the `cond' form, except that all matching branches
;;     are used, so this form is equivalent to:
;;       (cond <cond> => <spec> ...) ...
;;   - (tag <tag> <spec>) is the same as using <spec>, except that the <tag> is
;;     added to the expansion environment, so it is available for nested cond
;;     clauses.  Instead of a single <tag>, you can use a list of tags.  The
;;     tags are expanded using definitions made with :=tag, and the result is
;;     added to current tag list -- this expansion is a little different from
;;     the normal one in that the result contains all of the defintion it went
;;     through (so if x expands to y which expands to z, expanding x will
;;     result in x, y, and z).
;; Finally, it is possible to define `macro' constructs by using a lambda spec:
;;   - (lambda <args> <body>) if this is the only spec on a rhs of a
;;     definition, it is evaluated, and the resulting function is used to
;;     expand instances of what is bound to it.  The body itself is almost a
;;     normal lambda body, except that using multiple expressions will splice
;;     them into the calling location.
;; There are a few predefined macro constructs:
;;   - symbols are references to other spec definitions
;;   - `+' is a synonym for `or', `-' is for set-difference

;; ============================================================================
;; Distributions
;;   these are used to specify distributions, starting from the top and
;;   expanding down collecting the tags and the resulting tags are used to
;;   create the distribution file list.  The tags and the target entry name are
;;   strings to avoid expanding them prematurely.  When expansion gets to the
;;   `distribute!', it will use the tags to do a distribution with the given
;;   name -- converting them all to symbols.

distributions := (tag "mz" bin+src+dist)
                 (tag "mr" bin+src-dist)
                 (tag "dr" bin+src-dist)
                 (tag "racket" bin+src+dist)
                 (tag ("full" "bin") (distribute!))
bin+src+dist  := (tag "bin" (verify!) (distribute!))
                 (tag "src" (verify!) (distribute!))
bin+src-dist  := (tag "bin" (verify!))
                 (tag "src" (verify!))

;; Platform tags, lhs are binary types, rhs are source types
i386-linux        :=tag unix
i386-linux-gcc2   :=tag unix
i386-linux-fc2    :=tag unix
i386-linux-fc5    :=tag unix
i386-linux-fc6    :=tag unix
i386-linux-f7     :=tag unix
x86_64-linux-f7   :=tag unix
i386-linux-f9     :=tag unix
i386-linux-f12    :=tag unix
i386-linux-debian :=tag unix
i386-linux-debian-testing  :=tag unix
i386-linux-debian-unstable :=tag unix
i386-linux-ubuntu          :=tag unix
i386-linux-ubuntu-dapper   :=tag unix
i386-linux-ubuntu-edgy     :=tag unix
i386-linux-ubuntu-feisty   :=tag unix
i386-linux-ubuntu-hardy    :=tag unix
i386-linux-ubuntu-intrepid :=tag unix
i386-linux-ubuntu-jaunty   :=tag unix
i386-freebsd      :=tag unix
i386-win32        :=tag win
ppc-darwin        :=tag unix
i386-darwin       :=tag unix
ppc-osx-mac       :=tag mac
i386-osx-mac      :=tag mac
sparc-solaris     :=tag unix

;; tag specs to make each distribution a proper superset of the previous
mr  :=tag mz
dr  :=tag mr
plt :=tag dr

;; tag connections, mostly used for the below filtering in `distribution'
dr   :+=tag docs   ; include docs with dr & plt
plt  :+=tag docsrc ; include doc sources with the plt distro
unix :=tag man     ; man goes with unix
mac  :=tag man     ; ... and with mac
tests :=cond (and plt src) ; include tests when plt-src is used
docs  :=cond (and plt (not src))

;; distribution main entry point, apply selected global filters
distribution := (- (cond full => all plt => plt dr => dr mr => mr mz => mz)
                   distribution-filters)
distribution-filters :=
  (cond full => none
        else => (cond* src          => compiled-filter
                       (not src)    => src-filter
                       (not docs)   => docs-filter
                       (not docsrc) => docsrc-filter
                       (not man)    => man-filter
                       (not tests)  => tests-filter
                       (not mr)     => gui-filter
                       (not dr)     => tools-filter))

;; used for sanity checking: must be empty
;; (note: this rule means that we could avoid specifying docs and just include
;; the whole thing -- but this way we make sure that all doc sources are
;; included too (since they're specified together).)
must-be-empty := (cond docs => (- "/racket/doc/" distribution) else => none)

compiled-filter := (- (collects: "**/compiled/")
                      (cond verifying => "*.dep"))
                   "/racket/bin/" "/racket/lib/"
src-filter      := (src: "")
docs-filter     := (- (doc: "")   ; all docs,
                      (notes: "") ; excluding basic stuff
                      std-docs)   ; and things in git
docsrc-filter   := (+ (collects: "setup/scribble.rkt") ; only with doc sources
                      (collects: "**/scribblings/")
                      (srcfile: "*.{scrbl|scribble}")
                      std-docs)
man-filter      := (man: "*")
tests-filter    := (+ (collects: "**/tests/") (srcfile: "tests.rkt"))
gui-filter      := (- (+ (collects: "**/gui/") (srcfile: "gui.rkt"))
                      ;; for use in mz code that works in mr too
                      (srcfile: "scheme/gui/dynamic.rkt")
                      (srcfile: "racket/gui/dynamic.rkt"))
tools-filter    := (+ (collects: "**/tools/") (srcfile: "tools.rkt"))

;; these are in the doc directory, but are comitted in git and should be
;; considered like sources
std-docs        := (doc: "doc-license.txt" "*-std/")

;; ============================================================================
;; Junk

;; This is removed from the original tree only (*not* from the binary trees)
;; (the first line shouldn't be necessary, but be safe)
junk := (+ ".git*" "/.mailmap" ".svn" "CVS/" "[.#]*" "*~"
           ;; binary stuff should come from the platform directories
           "/racket/bin/" "/racket/lib/" "/racket/src/*build*/")

;; These are handled in a special way by the bundle script: the binary trees
;; are scanned for paths that have "<pfx>{3m|cgc}<sfx>" where a "<pfx><sfx>"
;; exists too, and will choose to keep the `binary-keep' version.  It will do
;; some sanity checking: allow only the patterns that are listed below
;; (otherwise: error), also throw an error if some path has all three versions
;; (3m, cgc, and none).  These specs must contain a parenthesized regular
;; expressions.

;; The following three definitions are treated in a special way.  They specify
;; which files to get rid of so we get a clean 3m (or cgc) distribution (used
;; in all distributions except for the `full' ones) .  The first one is a set
;; of template specification -- each must have this form: "...<...!...>...".
;; The actual patterns are created by substituting "!" with the `binary-keep'
;; and `binary-throw' patters and removing the "<>"s.  Both resulting patterns
;; are searched in the tree.  Say that the pattern is "111<222!333>444", the
;; two patterns that will be used are "111222KEEP333444" and
;; "111222THROW333444".  Also, for every found path in the tree, the "<...>"
;; part is removed to get a `plain' version.  So we have possible triplet of
;; paths -- one with the throw pattern, one with the keep, and one without the
;; "<...>" part (called plain).  It is an error if all three exist; otherwise,
;; keep the `keep' path (or the `plain' if there is no `keep' path in this
;; set), and throw away the `throw' path (or the `plain' if there is no
;; `throw').  There is a sanity check that verifies that all 3m/cgc paths are
;; covered by these templates.

binary-keep/throw-templates :=
  "/racket/{lib|include}/**/*<!>.*"
  "/racket/bin/*<!>"
  (cond win => "/racket/*<!>.exe"
               "/racket/lib/**/lib*<!>???????.{dll|lib|exp}"
        mac => "/racket/*<!>.app/"
               "/racket/lib/*Racket*.framework/Versions/*<_!>/")
  "/racket/collects/**/compiled/**/<!/>*.*"

binary-keep  := "3[mM]"
binary-throw := "{cgc|CGC}"

;; additional patterns that are removed from the distributions, things that
;; don't follow the above (have no 3m or cgc in the name, and no keep version
;; of the same name that will make them disappear)
binary-throw-more :=
  "/racket/lib/**/libmzgc???????.{dll|lib}"

;; ============================================================================
;; Convenient macros

plt-path: := (lambda (prefix . paths)
               (let* ([prefix (expand-spec-1 prefix)]
                      [paths  (expand-specs paths)]
                      [prefix (regexp-replace #rx"^/?(.+?)/?$" prefix "\\1/")]
                      [suffix ""])
                 (when (and (pair? paths) (eq? ': (car paths)))
                   (set! suffix (cadr paths)) (set! paths (cddr paths)))
                 `(+ ,@(map (lambda (path)
                              (concat "/racket/" prefix
                                      (regexp-replace #rx"^/" path "")
                                      suffix))
                            paths))))

src:       := (lambda ps `(plt-path: "src" ,@ps))

collects:  := (lambda ps `(plt-path: "collects" ,@ps))

doc:       := (lambda ps `(plt-path: "doc" ,@ps))

scribblings: := (lambda ps `(plt-path: "collects/scribblings" ,@ps))

doc+src:   := (lambda ps `(+ (doc: ,@ps) (scribblings: ,@ps)))

bin:       := (lambda ps
                (let ([ps (map (lambda (p)
                                 (regexp-replace*
                                  #rx"[a-zA-Z]"
                                  (regexp-replace* #rx"[ -]"
                                                   (expand-spec-1 p)
                                                   "[ -]")
                                  (lambda (ch)
                                    (string-append "[" (string-downcase ch)
                                                   (string-upcase ch) "]"))))
                               ps)])
                  `(+ (plt-path: "bin" : "{|3[mM]|cgc|CGC}" ,@ps)
                      (plt-path: "" : "{|3[mM]|cgc|CGC}.{exe|app}" ,@ps))))

notes:     := (lambda ps `(plt-path: "doc/release-notes" ,@ps))

lib:       := (lambda ps `(plt-path: "lib" ,@ps))

man:       := (lambda ps `(plt-path: "man/man1" : ".1" ,@ps))

tests:     := (lambda ps `(plt-path: "collects/tests" ,@ps))

srcfile: :=
  (lambda fs
    `(collects:
      ,@(mappend
         (lambda (f)
           (let* ([f     (if (regexp-match #rx"\\." f) f (concat f ".*"))]
                  [p+n+s (regexp-match #rx"^(.*/)?([^/]+)\\.([^.]+)$" f)]
                  [dir   (or (cadr p+n+s) "")]
                  [name  (concat (caddr p+n+s) "_" (cadddr p+n+s))])
             (list (concat "**/" f)
                   (concat "**/" dir "compiled/" name ".zo")
                   (concat "**/" dir "compiled/" name ".dep"))))
         fs)))

dll: := (lambda fs
          `(+ ,@(map (lambda (f)
                       (concat "/racket/lib/"
                               (regexp-replace #rx"^/" (expand-spec-1 f) "")
                               "{|3[mM]|cgc|CGC}{|???????}.dll"))
                     fs)
              ,@(map (lambda (f)
                       (concat "/racket/lib/**/"
                               (regexp-replace #rx"^.*/" (expand-spec-1 f) "")
                               "{|3[mM]|cgc|CGC}{|???????}.lib"))
                     fs)))

package: :=
  (lambda (p . more)
    (let* ([p (expand-spec-1 p)]
           [getkey
            (let loop ([l more] [ks '()])
              (cond
                [(null? l) (lambda (key [default #f])
                             (cond [(assq key ks) => cdr] [else default]))]
                [(null? (cdr l)) (error 'package "bad args")]
                [(not (keyword? (car l))) (error 'package "bad args")]
                [else (loop (cddr l)
                            (cons (cons (car l) (expand-spec-1 (cadr l)))
                                  ks))]))]
           [p (regexp-replace #rx"/$" p "")]
           [p/ (concat p "/")])
      `(+ (collects: ,(getkey '#:collection p/))
          (bin: ,(getkey '#:executable p))
          (doc+src: ,(getkey '#:docs p/))
          (notes: ,p/) (man: ,p) (tests: ,p/)
          ,@(if (getkey '#:src?) `((src: ,p/ ,(concat "worksp/" p/))) '()))))

;; ============================================================================
;; Base distribution specs

mz := (+ mz-base mz-src mz-bins mz-manuals mz-tests mz-extras)

mr := (+ mz mr-base mr-src mr-bins mr-manuals mr-extras)

dr := (+ mr dr-base dr-manuals dr-extras)

plt := (+ dr plt-extras)

;; ============================================================================
;; Packages etc

mz-base := "/racket/README"
           (package: "racket") (package: "mzscheme")
           "/racket/include/"
           ;; configuration stuff
           (cond (not src) => (collects: "info-domain/")) ; filtered
           (package: "config")
           ;; basic code
           (collects: "scheme" "s-exp" "reader")
           ;; include the time-stamp collection when not a public release
           (cond (not release)
                 => (- (collects: "repo-time-stamp/")
                       (cond (not dr) => (srcfile: "time-stamp.rkt"))))
mz-manuals := (scribblings: "main/") ; generates main pages (next line)
              (doc: "license/" "release/" "acks/" "search/"
                    "getting-started/")
              (notes: "COPYING.LIB" "COPYING-libscheme.txt")
              (doc: "doc-license.txt") ; needed (when docs are included)
              (doc+src: "reference/" "guide/" "quick/" "more/"
                        "foreign/" "inside/" ;; "places/" <- not ready yet
                        "scheme/"
                        "honu/")
              (doc: "*.{html|css|js|sxref}")
              (scribblings: "{{info|icons}.rkt|*.png}" "compiled")

mr-base := (package: "gracket") (bin: "gracket-text")
           (package: "mred") (bin: "mred-text")
           (collects: "afm/")
mr-manuals := (doc+src: "gui/")

dr-base := (package: "drracket") (package: "drscheme") (package: "framework")
dr-manuals := (doc+src: "tools/")

;; Misc hooks, to be added on by package rules below
mz-extras  :=
mr-extras  :=
dr-extras  :=
plt-extras :=

;; Tests definitions
mz-tests := (tests: "racket/" "info.rkt" "utils/" "match/" "eli-tester.rkt")

;; ============================================================================
;; Source definitions

mz-src := (+ (- (src: "README" "Makefile.in" "configure" "lt/" "racket/"
                      (cond win => "worksp/{README|mzconfig.h}"
                                   "worksp/{racket|libracket|libmzgc|gc2}/"
                                   "worksp/{mzstart|starters}/"
                                   "worksp/extradlls/"))
                (cond (not mr) => (src: "worksp/starters/mrstart.ico")))
             foreign-src)

mr-src := (src: "gracket/" "mred/" "wxcommon/"
                (cond unix => "wxxt/"
                      mac  => "mac/" "a-list/" "wxmac/"
                      win  => "wxwindow/"
                              "worksp/{jpeg|libgracket|gracket|mrstart}/"
                              "worksp/{png|wxme|wxs|wxutils|wxwin|zlib}/"))

foreign-src := (src: "foreign/{Makefile.in|README}"
                     "foreign/{foreign.*|rktc-utils.rkt}"
                     (cond win  => "foreign/libffi_msvc"
                           else => "foreign/gcc"))

;; ============================================================================
;; Binary definitions (`in-binary-tree' is used with binary trees, these
;; queries have no point elsewhere.)

mz-bins := (lib: "buildinfo" "**/mzdyn{|w}{|3[mM]|cgc|CGC}.{o|obj|exp|def}")
           (cond mac => (lib: "Racket*/")
                 win => (dll: "lib{mzgc|racket}" "UnicoWS" "iconv")
                        (lib: "gcc/{fixup|init}.o" "bcc/mzdynb.{obj|def}")
                 unix => (lib: "starter"))
           extra-dynlibs

mr-bins := (cond mac => (lib: "GRacket*/")
                 win => (dll: "libgracket"))

extra-dynlibs := (cond win => (dll: "{ssl|lib}eay32"))

;; ============================================================================
;; This filter is used on the full compiled trees to get the binary
;; (platform-dependent) portion out.

binaries := (+ "/racket/bin/"
               "/racket/lib/"
               "/racket/include/"
               "/racket/collects/**/compiled/native/"
               (cond unix => "/racket/bin/{|g}racket*"
                             "/racket/bin/{mzscheme|mred}*"
                     win  => "/racket/*.exe"
                             "/racket/*.dll"
                             "/racket/collects/launcher/*.exe"
                     mac  => "/racket/bin/racket*"
                             "/racket/bin/mzscheme*"
                             "/racket/*.app"
                             "/racket/collects/launcher/*.app")
               platform-dependent)

platform-dependent := ; hook for package rules

;; ============================================================================
;; Package rules

;; -------------------- setup
mz-extras :+= (- (package: "setup-plt" #:collection "setup/")
                 (cond (not dr) => (srcfile: "plt-installer{|-sig|-unit}.rkt")))

;; -------------------- raco
mz-extras :+= (package: "raco")

;; -------------------- launcher
mz-extras :+= (- (collects: "launcher")
                 (cond (not mr) => "[Mm]r[Ss]tart*.exe"))

;; -------------------- make
mz-extras :+= (package: "make/")

;; -------------------- dynext
mz-extras :+= (package: "dynext")

;; -------------------- mzlib (compatibility layer)
mz-extras :+= (package: "mzlib")

;; -------------------- compiler (mzc)
mz-extras :+= (package: "mzc" #:collection "compiler/") (doc+src: "cffi/")

;; -------------------- scribble
mz-extras :+= (package: "scribble") (collects: "at-exp")

;; -------------------- scriblib
mz-extras :+= (package: "scriblib")

;; -------------------- syntax
mz-extras :+= (package: "syntax")

;; -------------------- errortrace
mz-extras :+= (package: "errortrace")

;; -------------------- trace
mz-extras :+= (package: "trace")

;; -------------------- profile
mz-extras :+= (package: "profile")

;; -------------------- specific file format libraries
mz-extras :+= (package: "file")

;; -------------------- network protocols
mz-extras :+= (package: "net")

;; -------------------- openssl interface
mz-extras :+= (package: "openssl")

;; -------------------- parser
mz-extras :+= (package: "parser-tools/")

;; -------------------- html
mz-extras :+= (package: "html/")

;; -------------------- r5rs
mz-extras :+= (package: "r5rs/" #:executable "plt-r5rs")
              (doc: "r5rs-std")

;; -------------------- r6rs
mz-extras :+= (collects: "rnrs/")
              (package: "r6rs/" #:executable "plt-r6rs")
              (doc: "r6rs-std" "r6rs-lib-std")

;; -------------------- readline
mz-extras :+= (package: "readline/")

;; -------------------- wxme
mz-extras :+= (collects: "wxme/")

;; -------------------- web-server
mz-extras :+=
  (+ (package: "web-server" #:executable "PLT Web Server")
     (doc: "continue" "web-server-internal"))

;; -------------------- srfi
mz-extras :+= (package: "srfi") (doc: "srfi-std")

;; -------------------- xml
mz-extras :+= (- (package: "xml/")
                 (cond* (not plt) => (srcfile: "*-{tool|snipclass}.rkt"
                                               "xml.png")))

;; -------------------- ffi
mz-extras :+= (collects: "ffi/") (doc: "objc")

;; -------------------- preprocessor
mz-extras :+= (package: "preprocessor/") (bin: "mzpp" "mztext")

;; -------------------- tex2page & slatex
plt-extras :+= (package: "tex2page")
               (package: "slatex")
               (bin: "PDF SLaTeX")
               (doc+src: "slatex-wrap/")

;; -------------------- planet
mz-extras :+= (package: "planet")

;; -------------------- mrlib
mr-extras :+= (- (+ (package: "mrlib/")
                    (collects: "hierlist/")
                    (collects: "icons/turn-{up|down}{|-click}.png")
                    (tests: "aligned-pasteboard/")))

;; -------------------- sgl
mr-extras :+= (package: "sgl/")
;; gl-info.rkt doesn't exist, but gl-info.zo holds platform-dependent data
platform-dependent :+= (and (collects: "sgl/")
                            (srcfile: "sgl/gl-info"))

;; -------------------- syntax-color
dr-extras :+= (package: "syntax-color")

;; -------------------- plt-help
dr-extras :+= (collects: "help") (bin: "Racket Documentation")
              (bin: "plt-help") (man: "plt-help")

;; -------------------- lang
plt-extras :+= (package: "lang/" #:docs "htdp-langs/")

;; -------------------- htdp, tests, teachpacks
plt-extras :+=
  (collects: "htdp/")
  (doc: "htdp-lib")
  (- (package: "teachpack/") (collects: "teachpack/deinprogramm/"))
  (- (package: "2htdp/")
     "uchat/") ; Matthias doesn't want this in now
  (package: "test-engine/")

;; -------------------- stepper
plt-extras :+= (package: "stepper")

;; -------------------- macro-debugger
plt-extras :+= (package: "macro-debugger")

;; -------------------- lazy
plt-extras :+= (package: "lazy")

;; -------------------- combinator-parser
plt-extras :+= (collects: "combinator-parser")

;; -------------------- icons
dr-extras :+= (collects: "icons/")

;; -------------------- string
dr-extras :+= (package: "string-constants")

;; -------------------- defaults
dr-extras :+= (collects: "defaults/")

;; -------------------- version
mz-extras :+= (- (package: "version/")
                 (cond* (not dr) => (srcfile: "tool.rkt")))

;; -------------------- browser
dr-extras :+= (package: "browser/")

;; -------------------- graphics
plt-extras :+= (package: "graphics/") (doc: "turtles")

;; -------------------- embedded
plt-extras :+= (package: "embedded-gui/")

;; -------------------- eopl
plt-extras :+= (package: "eopl/")

;; -------------------- algol60
plt-extras :+= (package: "algol60/")

;; -------------------- games
plt-extras :+= (- (+ (package: "games/" #:executable "plt-games")
                     (doc+src: "gl-board-game/" "cards/"))
                  "paint-by-numbers/{hattori|solution-sets|raw-problems}")

;; -------------------- texpict & slideshow
plt-extras :+= (collects: "texpict/")
               (package: "slideshow")

;; -------------------- frtime
plt-extras :+= (package: "frtime/")

;; -------------------- typed-scheme
dr-extras :+= (package: "typed-scheme/" ; used in drracket
                        #:docs "ts-{reference|guide}/")
              (- (collects: "typed/")
                 (cond (not plt) => (collects: "typed/test-engine/")
                                    (collects: "typed/rackunit/")
                                    (srcfile: "typed/rackunit.rkt")))

;; -------------------- gui-debugger
plt-extras :+= (collects: "gui-debugger/")

;; -------------------- swindle
mz-extras :+= (- (package: "swindle")
                 (cond (not dr) => (srcfile: "tool.rkt" "swindle*.png")))

;; -------------------- plot
plt-extras :+=
  (- (package: "plot")
     ;; src should be included, otherwise it will be impossible to recompile it
     ;; (cond (not src) => "src/")
     )

;; -------------------- mzcom
plt-extras :+= (- (package: "mzcom" #:src? #t)
                  (cond (not win) => (src: "")))

;; -------------------- mysterx
plt-extras :+= (- (+ (package: "mysterx" #:src? #t)
                     (src: "worksp/libmysterx/")
                     (dll: "myspage" "myssink"))
                  (cond (not win) => (src: "")))

;; -------------------- srpersist
;; not included
;; plt-extras :+= (package: "srpersist" #:src? #t)

;; -------------------- temporary tool for converting old files
plt-extras :+= (package: "test-box-recovery")

;; -------------------- redex
plt-extras :+= (package: "redex")

;; -------------------- deinprogramm
plt-extras :+= (package: "deinprogramm/")
               (collects: "teachpack/deinprogramm/")
               (doc: "DMdA-lib")

;; -------------------- unstable
mz-extras :+= (- (package: "unstable")
                 ;; should "gui" mean DrRacket or GRacket? It's not
                 ;; obvious that "framework" is only in DrRacket.
                 (cond (not dr) => (collects: "unstable/gui")))

;; -------------------- plai
plt-extras :+= (package: "plai/")

;; -------------------- rackunit & older schemeunit compatibility
plt-extras :+= (package: "rackunit/")
plt-extras :+= (package: "schemeunit/")

;; -------------------- racklog (aka schelog)
plt-extras :+= (package: "racklog/")

;; ============================================================================
;; Readme header

version := (lambda () (version))

platform
:= (cond i386-linux        => "Linux (i386)"
         i386-linux-gcc2   => "Linux (i386/gcc2)"
         i386-linux-fc2    => "Fedora Core 2 (i386)"
         i386-linux-fc5    => "Fedora Core 5 (i386)"
         i386-linux-fc6    => "Fedora Core 6 (i386)"
         i386-linux-f7     => "Fedora 7 (i386)"
         x86_64-linux-f7   => "Fedora 7 (x86_64)"
         i386-linux-f9     => "Fedora 9 (i386)"
         i386-linux-f12    => "Fedora 12 (i386)"
         i386-linux-debian => "Debian Stable (i386)"
         i386-linux-debian-testing  => "Debian Testing (i386)"
         i386-linux-debian-unstable => "Debian Unstable (i386)"
         i386-linux-ubuntu          => "Ubuntu (i386)"
         i386-linux-ubuntu-dapper   => "Ubuntu Dapper (i386)"
         i386-linux-ubuntu-edgy     => "Ubuntu Edgy (i386)"
         i386-linux-ubuntu-feisty   => "Ubuntu Feisty (i386)"
         i386-linux-ubuntu-hardy    => "Ubuntu Hardy (i386)"
         i386-linux-ubuntu-intrepid => "Ubuntu Intrepid (i386)"
         i386-linux-ubuntu-jaunty   => "Ubuntu Jaunty (i386)"
         i386-freebsd      => "FreeBSD (i386)"
         sparc-solaris     => "Solaris"
         ppc-osx-mac       => "Mac OS X (PPC)"
         i386-osx-mac      => "Mac OS X (Intel)"
         ppc-darwin        => "Mac OS X using X11 (PPC)"
         i386-darwin       => "Mac OS X using X11 (Intel)"
         i386-win32        => "Windows"
         ;; generic platforms for source distributions
         unix => "Unix"
         mac  => "Macintosh"
         win  => "Windows")

readme-header
:= "This is the Racket v"(version)" "(cond src  => "source" else => "binary")
   " package for "platform".\n"
   (cond src => "See the build instructions in src/README.\n")

;; ============================================================================
