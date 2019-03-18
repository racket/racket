# Forking the Repository

If you want to contribute to Racket, it's a good idea to fork the repository so	you have your own version.  The process is simple:

* Log in to GitHub (create an account if necessary)
* Go to https://github.com/racket/racket
* Click 'Fork' at the top right, just underneath the black bar at top of page
 
You now have your own version of the repository.  As a general policy, all changes should be made to your fork.

# Making changes
### Summary
The basic procedure is to find the error in the main repository, change to your own repository, make the change, then submit a pull request so that the Racket maintainers can review and integrate your change.  The process is relatively straightforward:
* Log in to GitHub
* Go to the main Racket repository:  https://github.com/racket/racket
* Search for the text of the error so you know what file the error is in (the search box is at top left)
* Go to that file in your own fork (easy way:  edit the URL by replacing https://github.com/racket/racket/ with https://github.com/[MY-USERNAME]/racket/)
* Edit the file
* Commit and send a pull request

## Details
Be sure that you're searching in the main Racket repository (link above) instead of in your own fork.  Github does not allow searching in forks unless the fork has more stars than the parent repo.
* Log in to GitHub
* Go to https://github.com/racket/racket
* Click in the search field at the top left of the page.  (It's grey on a grey background so can be hard to notice)
* Paste the text of the thing you're trying to find
* Find the relevant page in the search results, click the link to go to it
* Go to that file in your own fork (easy way:  edit the URL by replacing https://github.com/racket/racket/ with https://github.com/[MY-USERNAME]/racket/)
* Look for the 'Branch' droplist.  It's on the left side at the same level as the green 'Clone or Download' button (which is on the right).  This will tell you what branch you're on. It might be something unhelpful like 'Tree: 5bb83...'
* Click on the dropdown, choose an existing branch or create a new one
* Click the edit button (pen icon) on the right hand side, just above the contents of the file
* Make your changes
* Scroll to the bottom of the page, enter a message that describes what you did
* Choose the radio button that says "*Create a new branch for this commit and start a pull request. Learn more about pull requests*"  It will propose a branch name for you.
* Click the green "*Propose File Change*"
* Click on the 'Pull Requests' button, just below the name of the repository
* Find the branch name that you just created, click 'Compare and pull request'
* Add some explanatory text to the message box, explaining what you did
* Click 'Create Pull Request'
* Bask in the glory of helping the Racket Community

# Where to go from there?

You can read the [Racket contribution guide](https://blog.racket-lang.org/2017/09/tutorial-contributing-to-racket.html).
