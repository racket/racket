(define (show-help)
  (define init-filename
    (let-values ([(base name dir?) (split-path (find-system-path 'init-file))])
      (path->string name)))
  (#%printf "usage: ~a [<option> ...] <argument> ...\n" (if gracket? "gracket" "racket"))
  (when (and gracket? (eq? 'unix (system-type)))
    (#%printf
     (string-append
      "\n"
      "X configuration options (must precede all other options):\n\n"
      "  -display <display>, -geometry <geometry>, -fn <font>,\n"
      "  -font <font>, -bg <color>, -background <color>, -fg <color>,\n"
      "  -foreground <color>, -iconic, -name <name>, -rv, -reverse,\n"
      "  +rv, -selectionTimeout <timeout>, -synchronous, -title <title>,\n"
      "  -xnllanguage <language>, -xrm <file>\n")))
  (#%printf
   (string-append
    "\n"
    "File and expression options:\n\n"
    "  -e <exprs>, --eval <exprs>\n"
    "     Evaluate <exprs>, print results\n"
    "  -f <file>, --load <file>\n"
    "     Like -e '(load \"<file>\")' without printing\n"
    "  -t <file>, --require <file>\n"
    "     Like -e '(require (file \"<file>\"))' [*]\n"
    "  -l <path>, --lib <path>\n"
    "     Like -e '(require (lib \"<path>\"))' [*]\n"
    "  -p <package>\n"
    "     Like -e '(require (planet \"<package>\")' [*]\n"
    "  -r <file>, --script <file>\n"
    "     Same as -f <file> -N <file> --\n"
    "  -u <file>, --require-script <file>\n"
    "     Same as -t <file> -N <file> --\n"
    "  -k <n> <m> <p>\n"
    "     Load executable-embedded code from offset <n> to <p>\n"
    "  -Y <file> <n> <m> <p>\n"
    "     Like -k <n> <m> <p>, but from <file>\n"
    "  -m, --main\n"
    "     Call `main` with command-line arguments, print results\n"
    "\n"
    " [*] Also `require`s a `main` submodule, if any\n"
    "\n"
    "Interaction options:\n\n"
    "  -i, --repl\n"
    "     Run interactive read-eval-print loop; implies -v\n"
    "  -n, --no-lib\n"
    "     Skip `(require (lib \"<init-lib>\"))` for -i/-e/-f/-r\n"
    "  -v, --version\n"
    "     Show version\n"))
  (when gracket?
    (#%printf
     (string-append
     "  -K, --back\n"
     "     Don't bring application to the foreground (Mac OS X)\n")))
  (#%printf
   (string-append
    "  -V, --no-yield\n"
    "     Skip `((executable-yield-handler) <status>)` on exit\n"
    "\n"
    "Configuration options:\n\n"
    "  -y, --make\n"
    "     Yes, enable automatic update of compiled files\n"
    "  -c, --no-compiled\n"
    "     Disable loading of compiled files\n"
    "  -q, --no-init-file\n"
    "     Skip load of " init-filename " for -i\n"))
  (when gracket?
    (#%printf
     (string-append
      "  -z, --text-repl\n"
      "     Use text `read-eval-print-loop` for -i\n")))
  (#%printf
   (string-append
    "  -I <path>\n"
    "     Set <init-lib> to <path> (sets language)\n"
    "  -X <dir>, --collects <dir>\n"
    "     Main collects at <dir> (or \"\" disables all)\n"
    "  -S <dir>, --search <dir>\n"
    "     More collects at <dir> (after main collects)\n"
    "  -G <dir>, --config <dir>\n"
    "     Main configuration directory at <dir>\n"
    "  -A <dir>, --addon <dir>\n"
    "     Addon directory at <dir>\n"
    "  -U, --no-user-path\n"
    "     Ignore user-specific collects, etc.\n"
    "  -R <paths>, --compiled <paths>\n"
    "     Set compiled-file search roots to <paths>\n"
    "  -C, --cross\n"
    "     Cross-build mode; save current collects and config\n"
    "     as host\n"
    "  -N <file>, --name <file>\n"
    "     Sets `(find-system-path 'run-file)` to <file>\n"
    "  -E <file>, --exec <file>\n"
    "     Sets `(find-system-path 'exec-file)` to <file>\n"))
  (when gracket?
    (#%printf
     (string-append
      "  -J <name>, ---wm-class <name>\n"
      "     Set WM_CLASS class to <name> (Unix)\n")))
  (#%printf
   (string-append
    "  -j, --no-jit\n"
    "     No effect, since there is no just-in-time compiler\n"
    "  -M, --compile-any\n"
    "     Compile to machine-independent form\n"
    "  -d, --no-delay\n"
    "     Disable on-demand loading of syntax and code\n"
    "  -b, --binary\n"
    "     No effect, since stdin and stdout/stderr are\n"
    "     always binary\n"
    "  -W <levels>, --warn <levels>\n"
    "     Set stderr logging to <levels>\n"
    "  -O <levels>, --stdout <levels>\n"
    "     Set stdout logging to <levels>\n"
    "  -L <levels>, --syslog <levels>\n"
    "     Set syslog logging to <levels>\n"
    "  --compile-machine <machine>\n"
    "     Compile for <machine>\n"
    "  --cross-compiler <machine> <plugin-dir>\n"
    "     Use compiler plugin for <machine>\n"
    "  --cross-server <mach> <comp> <lib>\n"
    "     Drive cross-compiler (as only option)\n"
    "\n"
    "Meta options:\n\n"
    "  --\n"
    "     No argument following this switch is used as a switch\n"
    "  -Z\n"
    "     Ignore the argument following this switch\n"
    "  -h, --help\n"
    "     Show this information and exits, ignoring other options\n"
    "\n"
    "Default options:\n\n"
    "  * If only configuration options are provided, -i is added\n"
    "  * If only configuration options are before the first\n"
    "    argument, -u is added\n"
    "  * If -t/-l/-p/-u appears before the first -i/-e/-f/-r,\n"
    "    -n is added\n"
    "  * <init-lib> defaults to " (if gracket? "racket/gui/init" "racket/init") "\n"
    "\n"
    "Switch syntax:\n\n"
    "  Multiple single-letter switches can be collapsed, with\n"
    "  arguments placed after the collapsed switches; the first\n"
    "  collapsed switch cannot be --\n\n"
    "  For example,\n"
    "\n"
    "      -ifve file expr\n"
    "\n"
    "  is the same as\n"
    "\n"
    "      -i -f file -v -e expr\n"
    "\n"
    "Start-up sequence:\n\n"
    "  1. Set `current-library-collection-paths`\n"
    "  2. Require `(lib \"<init-lib>\")` [when -i/-e/-f/-r, unless -n]\n"
    "  3. Evaluate/load expressions/files in order, until first error\n"
    "  4. Load \"" init-filename "\" [when -i]\n"
    "  5. Run read-eval-print loop [when -i]\n"))
  (when gracket?
    (#%printf
     "  6. Run `((executable-yield-handler) <status>)` [unless -V]\n")))
