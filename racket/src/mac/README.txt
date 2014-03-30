See "../native-libs/README.txt" for information about building most
3rd-party native libraries that are needed for Racket libraries


In addition, the `racket/gui' library uses PSMTabBarControl on Mac OS X.

Download:

    https://github.com/dorianj/PSMTabBarControl
 or 
    maccode.googlecode.com [PowerPC]

 Note that the version at from maccode has a bug on dealloc() and uses
 methods that are now deprecated.

Patches:

 PowerPC: PSMTabBarControl/PSMTabBarControl.m:216: change to

     // copy _cells because removing a cell
     // can modify the array (which is not allowed)
     NSArray *copyOfCells = [NSArray arrayWithArray: _cells];
     NSEnumerator *enumerator = [copyOfCells objectEnumerator];

In XCode:

 Build PSMTabBarControl. You only need the Framework target, and
 in Release mode (which is "Build for Archiving" in Xcode 4.5).
 Use `ditto' to reduce the framework to one architecture.
 Add back the "LICENSE.txt" file.

Install:

  Copy "PSMTabBarControl.framework" into the installation's "lib"
  directory or a suitable package-source directory. You can flatten
  all the auto-version soft links (moving "PSMTabBarControl" and
  "Resources" to immediately inside "PSMTabBarControl), and you can
  use `ditto' to prune the binary to just the platform that you're
  using.
