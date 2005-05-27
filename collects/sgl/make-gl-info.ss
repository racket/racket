(module make-gl-info mzscheme
  (require (prefix dynext: (lib "compile.ss" "dynext"))
           (all-except (lib "file.ss" "dynext") append-c-suffix)
           (prefix dynext: (lib "link.ss" "dynext"))
           (lib "file.ss")
           (lib "13.ss" "srfi"))
  
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
    (with-handlers ((exn:fail:filesystem? void))
      (delete-file x)))

  (define (parse-includes s)
    (map (lambda (s)
           (substring s 2 (string-length s)))
         (string-tokenize s)))
  
  (define (get-args which-arg home)
    (let ((fp (build-path home "lib" "buildinfo")))
      (cond
        ((file-exists? fp)
         (call-with-input-file fp
           (lambda (i)
             (let loop ((l (read-line i)))
               (cond
                 ((eof-object? l) "")
                 (else
                  (let ((m (regexp-match (format "^~a=(.*)$" which-arg) l)))
                    (if m
                        (cadr m)
                        (loop (read-line i))))))))))
        (else ""))))

  (define (compile-c-to-so file file.c file.so home)
    (let ((file.o (append-object-suffix file)))
      (dynext:compile-extension #f 
                                file.c
                                file.o
                                `(,@(parse-includes (get-args "X_CFLAGS" home))
                                    ,(build-path home "collects" "compiler")))
      (dynext:link-extension #f (list file.o) file.so)
      (delete/continue file.o)))

  (define (build-helper compile-directory home)
    (let ((file (build-path compile-directory "make-gl-info-helper"))
          (c (build-path compile-directory "make-gl-info-helper.c"))
          (so (build-path compile-directory
                          "native"
                          (system-library-subpath #f) 
                          "make-gl-info-helper.so")))
      (make-directory* (build-path compile-directory "native" (system-library-subpath #f)))
      (with-output-to-file c
        (lambda () (display c-file))
        'replace)
      (compile-c-to-so file c so home)))
  
  (define (make-gl-info compile-directory home)
    (let ((zo (build-path compile-directory "gl-info.zo"))
          (mod
           (compile
            (case (system-type)
              ((macosx windows)
               '(module gl-info mzscheme
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
                  (define gl-clampd-size 8)))
             (else
              (build-helper compile-directory home)
              `(module gl-info mzscheme
                 (provide (all-defined))
                 ,@(map 
                    (lambda (x)
                      `(define ,x ,(dynamic-require '(lib "make-gl-info-helper.ss" "sgl") x)))
                    '(gl-byte-size gl-ubyte-size gl-short-size gl-ushort-size
                      gl-int-size gl-uint-size gl-boolean-size gl-sizei-size
                      gl-bitfield-size gl-enum-size gl-float-size gl-double-size
                      gl-clampf-size gl-clampd-size))))))))
      (with-output-to-file zo
        (lambda () (write mod))
        'replace))))
