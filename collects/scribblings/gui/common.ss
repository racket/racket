
(module common mzscheme
  (require (lib "manual.ss" "scribble")
           (lib "basic.ss" "scribble")
           (lib "class.ss")
           (lib "contract.ss")
           "blurbs.ss"
           (only "../reference/mz.ss" AllUnix exnraise))
  (provide (all-from (lib "manual.ss" "scribble"))
           (all-from (lib "basic.ss" "scribble"))
           (all-from (lib "class.ss"))
           (all-from (lib "contract.ss"))
           (all-from "blurbs.ss")
           (all-from "../reference/mz.ss"))

  (require-for-label (lib "mred.ss" "mred")
                     (lib "class.ss")
                     (lib "lang.ss" "big"))
  (provide-for-label (all-from (lib "mred.ss" "mred"))
                     (all-from (lib "class.ss"))
                     (all-from (lib "lang.ss" "big"))))

