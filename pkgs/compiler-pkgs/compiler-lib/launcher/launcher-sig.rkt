#lang racket/signature

make-gracket-launcher
make-racket-launcher
make-mred-launcher
make-mzscheme-launcher

make-gracket-program-launcher
make-racket-program-launcher
make-mred-program-launcher
make-mzscheme-program-launcher

gracket-program-launcher-path
racket-program-launcher-path
mred-program-launcher-path
mzscheme-program-launcher-path

install-gracket-program-launcher
install-racket-program-launcher
install-mred-program-launcher
install-mzscheme-program-launcher

gracket-launcher-up-to-date?
racket-launcher-up-to-date?
mred-launcher-up-to-date?
mzscheme-launcher-up-to-date?

gracket-launcher-is-directory?
racket-launcher-is-directory?
mred-launcher-is-directory?
mzscheme-launcher-is-directory?

gracket-launcher-is-actually-directory?
racket-launcher-is-actually-directory?
mred-launcher-is-actually-directory?
mzscheme-launcher-is-actually-directory?

gracket-launcher-add-suffix
racket-launcher-add-suffix
mred-launcher-add-suffix
mzscheme-launcher-add-suffix

gracket-launcher-put-file-extension+style+filters
racket-launcher-put-file-extension+style+filters
mred-launcher-put-file-extension+style+filters
mzscheme-launcher-put-file-extension+style+filters

build-aux-from-path
extract-aux-from-path
current-launcher-variant
available-mred-variants
available-mzscheme-variants
available-gracket-variants
available-racket-variants

installed-executable-path->desktop-path
installed-desktop-path->icon-path
