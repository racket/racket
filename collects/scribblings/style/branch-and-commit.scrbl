#lang scribble/base

@(require "shared.rkt")

@title{Branch and Commit}

@section{Commit}

So what is the major lesson of this section? When you fix a bug, make sure
 to commit (1) the code delta, (2) the new test case, and (3) the revised
 docs (if applicable) in one batch. If the creation of a single commit is
 too complex of if you wish to factor out one of the commits, please push
 all pieces at once. That way the code base is always in a state where
 code, tests, and documentation are in sync, and readers of commit messages
 can evaluate changes completely.

@section{No Commit ``Bombs'' Please}

On occasion, you will find that you are spending a significant amount of
 time working with someone else's code. To avoid potentially painful
 merges, please (1) inform the author when you create the branch and (2)
 set the mail hook @margin-note*{See @hyperlink["http://tmp.barzilay.org/git.txt"]{Eli's write-up} on using git in PLT for
 information on the mechanics.} so that git sends a commit message to both
 you and the original author. Furthermore, you should test your changes on
 the actual code base. In some cases it is acceptable to delay such tests,
 e.g., when you will not know for a long time whether the performance
 implication allow a commit to the PLT repository.
