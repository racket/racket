(define (show-help)
  (define init-filename
    (let-values ([(base name dir?) (split-path (find-system-path 'init-file))])
      (path->string name)))
  (#%printf "~a: [<option> ...] <argument> ...\n" (if gracket? "gracket" "racket"))
  (when (and gracket? (eq? 'unix (system-type)))
    (#%printf
     (string-append
      " X configuration options (must precede all other options):\n"
      "  -display <display>, -geometry <geometry>, -fn <font>, -font <font>,\n"
      "  -bg <color>, -background <color>, -fg <color>, -foreground <color>,\n"
      "  -iconic, -name <name>, -rv, -reverse, +rv, -selectionTimeout <timeout>,\n"
      "  -synchronous, -title <title>, -xnllanguage <language>, -xrm <file>\n")))
  (#%printf
   (string-append
    " File and expression options:\n"
    "  -e <exprs>, --eval <exprs> : Evaluate <exprs>, prints results\n"
    "  -f <file>, --load <file> : Like -e '(load \"<file>\")' without printing\n"
    "  -t <file>, --require <file> : Like -e '(require (file \"<file>\"))' [*]\n"
    "  -l <path>, --lib <path> : Like -e '(require (lib \"<path>\"))' [*]\n"
    "  -p <package> : Like -e '(require (planet \"<package>\")' [*]\n"
    "  -r <file>, --script <file> : Same as -f <file> -N <file> --\n" 
    "  -u <file>, --require-script <file> : Same as -t <file> -N <file> --\n" 
    "  -k <n> <m> <p> : Load executable-embedded code from offset <n> to <p>\n"
    "  -m, --main : Call `main' with command-line arguments, print results\n"
    "  [*] Also `require's a `main' submodule, if any\n"
    " Interaction options:\n"
    "  -i, --repl : Run interactive read-eval-print loop; implies -v\n"
    "  -n, --no-lib : Skip `(require (lib \"<init-lib>\"))' for -i/-e/-f/-r\n"
    "  -v, --version : Show version\n"))
  (when gracket?
    (#%printf
     (string-append
     "  -K, --back : Don't bring application to the foreground (Mac OS X)\n")))
  (#%printf
   (string-append
    "  -V, --no-yield : Skip `((executable-yield-handler) <status>)' on exit\n"
    " Configuration options:\n"
    "  -c, --no-compiled : Disable loading of compiled files\n"
    "  -q, --no-init-file : Skip load of " init-filename " for -i\n"))
  (when gracket?
    (#%printf
     "  -z, --text-repl : Use text `read-eval-print-loop' for -i\n"))
  (#%printf
   (string-append
    "  -I <path> : Set <init-lib> to <path> (sets language)\n"
    "  -X <dir>, --collects <dir> : Main collects at <dir> (or \"\" disables all)\n"
    "  -S <dir>, --search <dir> : More collects at <dir> (after main collects)\n"
    "  -G <dir>, --config <dir> : Main configuration directory at <dir>\n"
    "  -A <dir>, --addon <dir> : Addon directory at <dir>\n"
    "  -U, --no-user-path : Ignore user-specific collects, etc.\n"
    "  -R <paths>, --compiled <paths> : Set compiled-file search roots to <paths>\n"
    "  -C, --cross : Cross-build mode; save current collects and config as host\n"
    "  -N <file>, --name <file> : Sets `(find-system-path 'run-file)' to <file>\n"))
  (when gracket?
    (#%printf
     "  -J <name>, ---wm-class <name> : Set WM_CLASS class to <name> (Unix)\n"))
  (#%printf
   (string-append
    "  -j, --no-jit : No effect, since there is no just-in-time compiler\n"
    "  -M, --compile-any : Compile to machine-independent form\n"
    "  -d, --no-delay : Disable on-demand loading of syntax and code\n"
    "  -b, --binary : No effect, since stdin and stdout/stderr are always binary\n"
    "  -W <levels>, --warn <levels> : Set stderr logging to <levels>\n"
    "  -O <levels>, --stdout <levels> : Set stdout logging to <levels>\n"
    "  -L <levels>, --syslog <levels> : Set syslog logging to <levels>\n"
    "  --compile-machine <machine> : Compile for <machine>\n"
    "  --cross-compiler <machine> <plugin-dir> : Use compiler plugin for <machine>\n"
    "  --cross-server <mach> <comp> <lib> : Drive cross-compiler (as only option)\n"
    " Meta options:\n"
    "  -- : No argument following this switch is used as a switch\n"
    "  -h, --help : Show this information and exits, ignoring other options\n"
    "Default options:\n"
    " If only configuration options are provided, -i is added\n"
    " If only configuration options are before the first argument, -u is added\n"
    " If -t/-l/-p/-u apears before the first -i/-e/-f/-r, -n is added\n"
    " <init-lib> defaults to " (if gracket? "racket/gui/init" "racket/init") "\n"
    "Switch syntax:\n"
    " Multiple single-letter switches can be collapsed, with arguments placed\n"
    "   after the collapsed switches; the first collapsed switch cannot be --\n"
    " Example: `-ifve file expr' is the same as `-i -f file -v -e expr'\n"
    "Start-up sequence:\n"
    " 1. Set `current-library-collection-paths'\n"
    " 2. Require `(lib \"<init-lib>\")' [when -i/-e/-f/-r, unless -n]\n"
    " 3. Evaluate/load expressions/files in order, until first error\n"
    " 4. Load \"" init-filename "\" [when -i]\n"
    " 5. Run read-eval-print loop [when -i]\n"))
  (when gracket?
    (#%printf
     " 6. Run `((executable-yield-handler) <status>)' [unless -V]\n")))
