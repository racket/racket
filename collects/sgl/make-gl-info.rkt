#lang mzscheme

(require (prefix dynext: dynext/compile)
         dynext/file
         (prefix dynext: dynext/link)
         mzlib/file
         setup/dirs
         launcher
         srfi/13/string)

(provide make-gl-info)

(define c-file #<<end-string
#include <escheme.h>
#include <GL/gl.h>
#include <GL/glu.h>

Scheme_Object *scheme_reload(Scheme_Env *env)
{
  Scheme_Env *mod_env;

  mod_env = scheme_primitive_module(scheme_intern_symbol("make-gl-info-helper"), env);
  scheme_add_global("gl-byte-size",
                    scheme_make_integer_value(sizeof(GLbyte)),
                    mod_env);
  scheme_add_global("gl-ubyte-size",
                    scheme_make_integer_value(sizeof(GLubyte)),
                    mod_env);
  scheme_add_global("gl-short-size",
                    scheme_make_integer_value(sizeof(GLshort)),
                    mod_env);
  scheme_add_global("gl-ushort-size",
                    scheme_make_integer_value(sizeof(GLushort)),
                    mod_env);
  scheme_add_global("gl-int-size",
                    scheme_make_integer_value(sizeof(GLint)),
                    mod_env);
  scheme_add_global("gl-uint-size",
                    scheme_make_integer_value(sizeof(GLuint)),
                    mod_env);
  scheme_add_global("gl-float-size",
                    scheme_make_integer_value(sizeof(GLfloat)),
                    mod_env);
  scheme_add_global("gl-double-size",
                    scheme_make_integer_value(sizeof(GLdouble)),
                    mod_env);
  scheme_add_global("gl-boolean-size",
                    scheme_make_integer_value(sizeof(GLboolean)),
                    mod_env);
  scheme_add_global("gl-sizei-size",
                    scheme_make_integer_value(sizeof(GLsizei)),
                    mod_env);
  scheme_add_global("gl-clampf-size",
                    scheme_make_integer_value(sizeof(GLclampf)),
                    mod_env);
  scheme_add_global("gl-clampd-size",
                    scheme_make_integer_value(sizeof(GLclampd)),
                    mod_env);
  scheme_add_global("gl-enum-size",
                    scheme_make_integer_value(sizeof(GLenum)),
                    mod_env);
  scheme_add_global("gl-bitfield-size",
                    scheme_make_integer_value(sizeof(GLbitfield)),
                    mod_env);
  scheme_finish_primitive_module(mod_env);

  return scheme_void;
}

Scheme_Object *scheme_initialize(Scheme_Env *env)
{
  return scheme_reload(env);
}

Scheme_Object *scheme_module_name(void)
{
  return scheme_intern_symbol("make-gl-info-helper");
}

end-string
)

(define (delete/continue x)
  (with-handlers ([exn:fail:filesystem? void])
    (delete-file x)))

(define (parse-includes s)
  (map (lambda (s) (substring s 2 (string-length s)))
       (string-tokenize s)))

(define (get-args which-arg)
  (let ([fp (build-path (find-lib-dir) "buildinfo")])
    (if (file-exists? fp)
      (call-with-input-file fp
        (lambda (i)
          (let loop ([l (read-line i)])
            (if (eof-object? l)
              ""
              (let ([m (regexp-match (format "^~a=(.*)$" which-arg) l)])
                (if m
                  (cadr m)
                  (loop (read-line i))))))))
      "")))

(define (compile-c-to-so file file.c file.so)
  (let ([file.o (append-object-suffix file)])
    (dynext:compile-extension #f file.c file.o
                              `(,@(parse-includes (get-args "X_CFLAGS"))
                                ,(collection-path "compiler")))
    (dynext:link-extension #f (list file.o) file.so)
    (delete/continue file.o)))

(define (build-helper compile-directory variant)
  (let* ([file "make-gl-info-helper.rkt"]
         [c (build-path compile-directory (append-c-suffix file))]
         [so (build-path compile-directory "native"
                         (system-library-subpath variant)
                         (append-extension-suffix file))])
    (make-directory* (build-path compile-directory "native"
                                 (system-library-subpath variant)))
    (with-output-to-file c (lambda () (display c-file)) 'replace)
    (compile-c-to-so file c so)))

(define (effective-system-type)
  (let ([t (system-type)])
    (if (not (eq? t 'unix))
      t
      ;; Check "buildinfo" for USE_GL flag:
      (let ([buildinfo (build-path (find-lib-dir) "buildinfo")])
        (if (not (file-exists? buildinfo))
          (begin (printf "WARNING: buildinfo file missing: ~a\n" buildinfo)
                 t)
          (with-input-from-file buildinfo
            (lambda ()
              (if (regexp-match? #rx"-DUSE_GL" (current-input-port))
                t
                (begin (printf "WARNING: no GL support\n")
                       'no-gl)))))))))

(define (make-gl-info compile-directory)
  (let ([zo (build-path compile-directory (append-zo-suffix "gl-info.rkt"))]
        [mod
         (compile
          (case (effective-system-type)
            [(macosx windows no-gl)
             `(,#'module gl-info mzscheme
                (provide (all-defined))
                (define gl-byte-size 1)
                (define gl-ubyte-size 1)
                (define gl-short-size 2)
                (define gl-ushort-size 2)
                (define gl-int-size 4)
                (define gl-uint-size 4)
                (define gl-boolean-size 1)
                (define gl-sizei-size 4)
                (define gl-bitfield-size 4)
                (define gl-enum-size 4)
                (define gl-float-size 4)
                (define gl-double-size 8)
                (define gl-clampf-size 4)
                (define gl-clampd-size 8))]
            [else
             (for-each (lambda (variant)
                         (parameterize ([dynext:link-variant variant])
                           (build-helper compile-directory variant)))
                       (available-mzscheme-variants))
             `(,#'module gl-info mzscheme
                (provide (all-defined))
                ,@(map
                   (lambda (x)
                     `(define ,x ,(dynamic-require 'sgl/make-gl-info-helper x)))
                   '(gl-byte-size gl-ubyte-size gl-short-size gl-ushort-size
                     gl-int-size gl-uint-size gl-boolean-size gl-sizei-size
                     gl-bitfield-size gl-enum-size gl-float-size gl-double-size
                     gl-clampf-size gl-clampd-size)))]))])
    (with-output-to-file zo
      (lambda () (write mod))
      'replace)))
