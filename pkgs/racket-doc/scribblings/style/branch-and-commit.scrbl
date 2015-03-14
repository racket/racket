#lang scribble/base

@(require "shared.rkt")

@title[#:tag "branch-and-commit"]{Retiquette: Branch and Commit}

 This section is specifically for Racketeers who commit to the Racket code
 base.

 Working with the bug database requires one critical work flow rule.

 Working with the code base requires style rules for actions on the
 repository. Currently we are using Git and below are a few rules on how to
 act in this context.

@;-----------------------------------------------------------------------------
@section{Bugfix Workflow}

Re-assign bug reports only after you can eliminate your own code as the source
 of a bug. The best way to accomplish this goal is to create a new example that
 re-creates the problem without involvement of your code. When you have such a
 code snippet, re-assign the code to the person responsible for the apparently
 buggy component and submit the code snippet as part of the justification.

@; -----------------------------------------------------------------------------
@section{Commit}

@bold{New feature commit:} Commit the new feature, its tests, and its
 documentations as you wish, but please push them together. However, do not
 commit states that don't run. (In Git, this means 'commit' and not just
 'push'.)

@bold{Bug fix commit:} When you fix a bug, make sure to commit (1) the
 code delta, (2) the new test case, and (3) the revised docs (if
 applicable) in one batch. If the creation of a single commit is too
 complex of if you wish to factor out one of the commits, please push all
 pieces at once. That way the code base is always in a state where code,
 tests, and documentation are in sync, and readers of commit messages can
 evaluate changes completely.

@bold{Style change commit:} Submit changes to the style of a file
 separately from changes to its behavior (new features, bugs).

Write meaningful commit messages. The first line (say 72 chars) should
 provide a concise summary of the commit. If the message must be longer,
 edit the rest of the message in your text editor and leave a blank line
 between the summary line and the rest of the message, like this:
@verbatim[#:indent 2]{
 some quick description

 more blah blah blah, with more
 details about the actual change
}
 The advantage of a blank line is that ``git log'' and other tools display
 the commit messages properly. If you prefer the ``-m'' command line flag
 over an editor, you can use several of them in a row.

The message for bug report fixes should contain ``Close PR NNNNN'' so that
 bug reports are automatically closed.

To avoid 'merge commits', update your repository with @tt{git --rebase pull}.

@; -----------------------------------------------------------------------------
@section{No Commit ``Bombs,'' Please}

On occasion, you will find that you are spending a significant amount of
 time working with someone else's code. To avoid potentially painful
 merges, please (1) inform the author when you create the branch and (2)
 set the mail hook so that git sends a commit message to both you and the
 original author. Furthermore, you should test your changes on the actual
 code base. In some cases it is acceptable to delay such tests, e.g., when
 you will not know for a long time whether the performance implications
 allow a commit to the PLT repository.

@margin-note*{See
@hyperlink["http://git.racket-lang.org/intro.html"]{the write-up} on
using git in PLT for details.}

As a reminder, here are the essential elements of git for working on a
fork:
@itemlist[

@item{setup a fork:
  @verbatim{
    ssh pltgit fork plt eli/my-plt}}

@item{setup mail notifications:
  @verbatim{
    ssh pltgit config set eli/my-plt hooks.counter true
    ssh pltgit config set eli/my-plt hooks.mailinglist @eli,...}}

@item{allow someone else to push commits to my repository:
  @verbatim{
    ssh pltgit setperms eli/my-plt
    RW eli
    RW someone-else
    ^D}}

]

