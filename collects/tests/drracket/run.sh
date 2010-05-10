#!/bin/sh -x
mred module-lang-test.ss &&
mred repl-test.ss &&
mred io.ss &&
mred language-test.ss &&
mred syncheck-test.ss &&
mred teachpack.ss &&
mred save-teaching-lang-file.ss
