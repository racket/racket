#lang at-exp racket/base
(require scribble/html
         plt-web
         (only-in plt-web/style columns))

(provide make-about)

(define (here . c)
  (columns 10 #:row? #t (body c)))

(define (hx . c)
  @h5{@b[c]})

(define (make-about page-site)
  (define page-title "About Package Builds")
  (define (literal-url s)
    @a[href: s s])
  (page #:site page-site
        #:file "about.html"
        #:title page-title
        (html (head (title page-title))

              @here{@h3[page-title]

                    @p{For every package that is registered at
@literal-url{http://pkgs.racket-lang.org/}, the package-build service
starts with the current release, periodically checks for package
updates, and attempts to build each package that has changed or has a
dependency that has changed.}

                    @p{If a package installation succeeds, any
tests in the package are run with}

                    @pre{  raco test --drdr}

                    @p{}

                    @p{Packages are built on a 64-bit Linux virtual
machine (VM) that is isolated from the network. Each package build
starts with a fresh instance of the virtual machine, and built
packages are re-packaged in built form for use by other
packages. Testing of a package starts with a fresh instance of the
virtual machine and a fresh installation of the package from its built
form.}

                    @; ----------------------------------------
                    @h3{Limitations}

                    @hx{Only Packages from the Main Catalog are Supported}

                    @p{The package-build service does not support
references to @a[href: "http://planet.racket-lang.org"]{PLaneT
packages} or to compatibility packages at
@literal-url{http://planet-compats.racket-lang.org/}. When a package
depends on one of those, then the package installation fails, because
package builds are performed on a VM without network
connectivity.}

                    @hx{The Package-Build Machine is Minimal}

                    @p{Each package is installed on a minimal VM that
omits as many system libraries and tools as is practical. If building
on the minimal VM fails, the package build is retried on a VM with
more tools and libraries, including a C compiler and an X server
running at @tt{:1}. Look for @|ldquo|extra system dependencies@|rdquo| in
the result column for packages that don@|rsquo|t work in the minimal
environment but do work in the extended one.}

                    @p{The idea behind the minimal VM is that a
package generally shouldn@|rsquo|t rely on tools that a Racket user
may not have installed@|mdash|and so it@|rsquo|s worth reporting
those problems from the package-build service. At the same time, a
package might be intended to work only in a typical Unix setup, and
disallowing access a C compiler would be especially uncooperative of
the package-build service.}

                    @hx{Foreign Libraries Need Special Handling}

                    @p{Even on the extend VM, the available system
libraries are limited. See @a[href: "#foreign"]{Working with Foreign
Libraries} below for information on implementing packages that rely on
foreign libraries.}

                    @hx{Test Capabilities May Be Limited}

                    @p{Limited system libraries, missing network
connectivity, or other constraints may prevent the package-build
service from straighforwardly running a package@|rsquo|s tests. See
@a[href: "#test"]{Dealing with Test Failures}.}

                    @; ----------------------------------------
                    @h3[name: "test"]{Dealing Test Failures}

                    @p{In the absence of any @tt{"info.rkt"}-based
specifications or @tt{test} submodules, @tt{raco test} runs each
module in a package. Running a particular module might fail if
it@|rsquo|s a program-starting module that expects command-line
arguments, or a module might start a program that expects input and
causes the test to time out.}

                    @p{In the simplest case, you can add a `test` submodule as}

                    @pre{  (module test racket/base)}

                    @p{}

                    @p{to make @tt{raco test} ignore the enclosing
module. You can control @tt{raco test} in various other ways with
submodules and @tt{"info.rkt"} files@";" see
@a[href: "http://docs.racket-lang.org/raco/test.html"]{the
documentation}.}

                    @p{The default timeout on an individual test is 90 seconds, and the
overall timeout for testing a package is 10 minutes. You can adjust the
former, but the latter is a hard limit for now.}

                    @p{Tests are always run on the extended VM, but
sometimes the package-build service cannot run a package@|rsquo|s tests. For
example, if a package needs network access for testing, the
package-build service can@|rsquo|t help, because it runs on an isolated
VM. There@|rsquo|s no way to @|ldquo|opt out@|rdquo| of packaging
testing, but a package author can adjust a test suite to skip tests
under adverse conditions. In case there@|rsquo|s no other way for a test
suite to determine that it can@|rsquo|t run, the package-build service sets
the @tt{PLT_PKG_BUILD_SERVICE} environment variable when running
tests@";" a test suite can explicitly check for the environment
variable and skip tests that can@|rsquo|t work.}


                    @; ----------------------------------------
                    @h3[name: "foreign"]{Handling Foreign Libraries}

                    @p{The @|ldquo|minimal@|rdquo| versus
@|ldquo|extended@|rdquo| VM distinction begs the question of how the
package-build service can support a package that relies on a foreign
library@|mdash|one that is not installed even on the extended VM.}

                    @p{It would be nice to have a bridge between the
Racket package system and the OS package manager so that dependencies
on OS packages could be declared and installed. One catch is that the
bridge would have to work with a package-build VM that is isolated
from the network. The networking, permission, and maintenance issues
seem complex enough that we haven@|rsquo|t embarked on that direction.}

                    @p{For now, the package-build installation
identifies itself as running on the @tt{"x86_64-linux-natipkg"}
platform, as opposed to plain @tt{"x86_64-linux"}. On the plain
@tt{"x86_64-linux"} platform, foreign libraries as needed by Racket
packages are expected to be installed by a user through the OS@|rsquo|s
package manager. On the @tt{"x86_64-linux-natipkg"} platform, however,
foreign libraries are handled as on Windows and Mac OS X: provided by
platform-specific packages.}

                    @p{For example, on the @tt{"x86_64-linux-natipkg"}
platform, the @tt{"math-lib"} package depends on the
@tt{"math-x86_64-linux-natipkg"} package, which provides 64-bit Linux
builds of GMP and MPFR. You can see that dependency declaration in the
@tt{"info.rkt"} file for the @tt{"math-lib"} package:}

                    @pre{  @literal-url{https://github.com/plt/racket/blob/master/pkgs/math-pkgs/math-lib/info.rkt}}

                    @p{}

                    @p{If your package depends on a foreign
library, then you currently have two main options:}

                    @hx{Accomodate Unavailable Libraries}

                    @p{One option is to make the package behave when the foreign library is unavailable.}

                    @p{Typically, a foreign library isn@|rsquo|t needed just to build a package
                       (including its documentation). If possible, delay any use of the
                       foreign library to run time, and that solves the build problem.}

                    @p{For tests, you can either just let them fail, or you can adjust the
                       test suite to avoid failure reports when the foreign library is
                       unavailable or (if you must) when @tt{PLT_PKG_BUILD_SERVICE} is defined.}

                    @hx{Distribute Native Libraries}

                    @p{Another option is to build a 64-bit Linux
version of the foreign library, distribute it as a package, and make
the package a platform-specific dependency of your package for the
@tt{"x86_64-linux-natipkg"} platform.}

                    @p{This option is in many ways the best one, for
now@|mdash|especially if Windows and Mac OS X libraries packages
also provided@|mdash|but it@|rsquo|s more work for a package
implementer.}

                    })))
