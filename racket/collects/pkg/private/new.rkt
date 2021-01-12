#lang racket/base

(provide pkg-new)

(require racket/match
         racket/port
         racket/system
         racket/string
         racket/date
         racket/format
         setup/collection-name
         "print.rkt")

(define (package-name? package)
  (regexp-match-exact? #rx"[-_a-zA-Z0-9]*" package))

(define (pkg-new name)

  ;; Useful strings
  (define user
    (string-trim
     (with-output-to-string
         (lambda ()
           (match (system-type)
             [(or 'unix 'macosx)
              (system "whoami")]
             ['windows
              (system "echo %username%")]
             [_ (pkg-error "not supported")])))))

  (define ====
    (make-string (string-length name) #\=))

  (define year
    (number->string (date-year (current-date))))

  (define sanitized-name
    (if (string->number name)
        (string-append "|" name "|")
        name))

  ;; Because I wish I had @-expressions
  (define (expand/display str [table (hash #"name" name #"user" user
                                           #"====" ==== #"year" year
                                           #"sanitized-name"
                                           sanitized-name)])
    (let ([in (open-input-string str)])
      (let loop ()
        (let ([m (regexp-match #rx"<<([^>]*)>>" in 0 #f (current-output-port))])
          (when m
            (display (hash-ref table (cadr m)))
            (loop))))))

  ;; Initialize the new package
  (cond
   [(directory-exists? name)
    (pkg-error (format "cannot make package, folder exists~n  path: ~a" name))]
   [(not (package-name? name))
    (pkg-error (format "cannot make package, invalid collection name~n  name:  ~a"
                       name))]
   [else
    (make-directory name)
    (parameterize ([current-directory name])

      ;; LICENSE files
      (displayln "Generating LICENSE-APACHE and LICENSE-MIT files. You are free to change the license.")
      (with-output-to-file "LICENSE-MIT"
        (lambda () (expand/display #<<EOS
<<name>> 

MIT License

Copyright (c) <<year>> <<user>>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.

EOS
)))
      (with-output-to-file "LICENSE-APACHE"
        (lambda () (expand/display #<<EOS
Copyright <<year>> <<user>>

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.

EOS
)))

      ;; .gitignore
      (with-output-to-file ".gitignore"
        (lambda () (display #<<EOS
*~
\#*
.\#*
.DS_Store
compiled/
/doc/

EOS
)))

      ;; .github/workflows/ci.yml
      (make-directory ".github")
      (make-directory ".github/workflows")
      (with-output-to-file ".github/workflows/ci.yml"
        (lambda () (expand/display #<<EOS
on: [push, pull_request]
name: CI
jobs:
  build:
    name: "Build on Racket '${{ matrix.racket-version }}' (${{ matrix.racket-variant }})"
    runs-on: ubuntu-latest
    strategy:
      matrix:
        racket-version: ["stable", "current"]
        racket-variant: ["BC", "CS"]
    steps:
      - uses: actions/checkout@v2
      - uses: Bogdanp/setup-racket@v0.12
        with:
          architecture: x64
          distribution: full
          variant: ${{ matrix.racket-variant }}
          version: ${{ matrix.racket-version }}
      - name: Installing <<name>> and its dependencies
        run: raco pkg install --no-docs --auto --name <<name>>
      - name: Compiling <<name>> and building its docs
        run: raco setup --check-pkg-deps --unused-pkg-deps <<name>>
      - name: Testing <<name>>
        run: raco test -x -p <<name>>

EOS
)))
      ;; info.rkt
      (with-output-to-file "info.rkt"
        (lambda () (expand/display #<<EOS
#lang info
(define collection "<<name>>")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/<<name>>.scrbl" ())))
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(<<user>>))

EOS
)))

      ;; README.md
      (with-output-to-file "README.md"
        (lambda () (expand/display #<<EOS
<<name>>
<<====>>
README text here.

EOS
)))

      ;; main.rkt
      (with-output-to-file "main.rkt"
(lambda () (display #<<EOS
#lang racket/base

(module+ test
  (require rackunit))

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

;; Code here



(module+ test
  ;; Any code in this `test` submodule runs when this file is run using DrRacket
  ;; or with `raco test`. The code here does not run when this file is
  ;; required by another module.

  (check-equal? (+ 2 2) 4))

(module+ main
  ;; (Optional) main submodule. Put code here if you need it to be executed when
  ;; this file is run using DrRacket or the `racket` executable.  The code here
  ;; does not run when this file is required by another module. Documentation:
  ;; http://docs.racket-lang.org/guide/Module_Syntax.html#%28part._main-and-test%29

  (require racket/cmdline)
  (define who (box "world"))
  (command-line
    #:program "my-program"
    #:once-each
    [("-n" "--name") name "Who to say hello to" (set-box! who name)]
    #:args ()
    (printf "hello ~a~n" (unbox who))))

EOS
)))
      (make-directory "scribblings")
      (parameterize ([current-directory "scribblings"])

        ;; scribblings/name.scrbl
        (with-output-to-file (format "~a.scrbl" name)
          (lambda () (expand/display #<<EOS
#lang scribble/manual
@require[@for-label[<<sanitized-name>>
                    racket/base]]

@title{<<name>>}
@author{<<user>>}

@defmodule[<<sanitized-name>>]

Package Description Here

EOS
)))))]))
