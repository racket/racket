#lang racket/base
(require racket/file
         racket/format
         "shelly.rkt"
         "util.rkt")

(pkg-tests
 (with-fake-root
  (shelly-case
   "install package, copy to other, remove, then migrate"
   $ "raco pkg config --set catalogs http://localhost:9990"
   (hash-set! *index-ht-1* "pkg-b"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-b-second.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-b-second.plt"))
   (hash-set! *index-ht-1* "pkg-a"
              (hasheq 'checksum
                      (file->string "test-pkgs/pkg-a-first.plt.CHECKSUM")
                      'source
                      "http://localhost:9999/pkg-a-first.plt"))
   $ "raco pkg install -u --deps search-auto pkg-b" =exit> 0
   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-a\\* +[a-f0-9]+    \\(catalog pkg-a\\)\npkg-b +[a-f0-9]+ +\\(catalog pkg-b\\)\n"
   $ (~a "racket"
         " -e \"(require racket/file setup/dirs)\""
         " -e \"(copy-directory/files (build-path (find-system-path 'addon-dir) (get-installation-name))"
         "                            (build-path (find-system-path 'addon-dir) (symbol->string 'other)))\"")
   $ "raco pkg remove -u --auto pkg-b"
   $ "raco pkg show -u -a" =stdout> " [none]\n"
   $ "raco pkg migrate -u other"
   $ "raco pkg show -u -a" =stdout> #rx"Package\\[\\*=auto\\] +Checksum +Source\npkg-a\\* +[a-f0-9]+    \\(catalog pkg-a\\)\npkg-b +[a-f0-9]+ +\\(catalog pkg-b\\)\n")))
