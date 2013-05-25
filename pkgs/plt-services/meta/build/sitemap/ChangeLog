Wed Jun 01 01:00:00 2005  Google Inc. <opensource@google.com>

	* sitemap_gen: initial release:
	  This directory contains Python utilities for creating
	  Sitemaps.

Mon Jun 13 01:00:00 2005  Google Inc. <opensource@google.com>

	* sitemap_gen.py: v1.1

	  [BIG]
	  Not blow up when dealing with international character encodings.

	  [MODERATE]
	  Fix platform and Python version issues.  In some versions of 2.2
	  and certain platforms, True was not defined.  Gak!
	
Tue Jul 12 01:00:00 2005  Google Inc. <opensource@google.com>

	* sitemap_gen.py: v1.2
	  
	  [MODERATE]
	  Default_file option added to directory walking
	  Support for Extended Logfile Format (IIS's log format)
	  Allow wildcards in the "path" attribute on accesslog and urllist
	    input methods.
	  Running on Python 1.5 should exit cleanly with an error message
	  Stricter processing of configuration files
	  
	  [SMALL]
	  XML files written in "text" mode, so linefeeds are correct
	  One more Unicode issue fixed: Sitemap filenames with non-ascii
	    characters had still been problematic
	  In directory walking, the root URL of the walk now gets included
	  In directory walking, URLs to directories now have a "/" appended
	  URLs to files we recognize as our own script's Sitemap output files
	    are suppressed.
	  'suppress_search_engine_notify="0"' now does what you would expect
	  Default priority on URLs is now 0.5 instead of 1.0
	  Priority values written by default to only 4 decimal places
	  URLs to Sitemap files in the Sitemap index file are now encoded
	  according to the user's default_encoding, instead of forcing to UTF-8

Mon Aug 01 01:00:00 2005  Google Inc. <opensource@google.com>

	* sitemap_gen.py: v1.3

	  [BIG]
	  <sitemap ... /> input method added.

	  [MODERATE]
	  Use proper IDNA encoding on international domain names.  This is
	    only available on Python2.3 or higher.

	  [SMALL]
	  Fixed Windows bug where directory walking would generate bad URLs on
	    2+ deep subdirectories

Wed Nov 03 01:00:00 2005  Google Inc. <opensource@google.com>

	* sitemap_gen.py: v1.4

	  [SMALL]
	  Fixed bug where writing a gzipped sitemap would store the server's
	  file path in the archive.
