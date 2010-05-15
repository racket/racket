#!/usr/bin/env python
#
# Copyright (c) 2004, 2005 Google Inc.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
#
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in
#   the documentation and/or other materials provided with the
#   distribution.
#
# * Neither the name of Google nor the names of its contributors may
#   be used to endorse or promote products derived from this software
#   without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
# FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
# COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
# ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#
#
# The sitemap_gen.py script is written in Python 2.2 and released to
# the open source community for continuous improvements under the BSD
# 2.0 new license, which can be found at:
#
#   http://www.opensource.org/licenses/bsd-license.php
#

"""Unit tests for sitemap_gen.py, a script for generating sitemaps
for a web server.
"""

# Please be careful that all syntax used in this file can be parsed on
# Python 1.5 -- this version check is not evaluated until after the
# entire file has been parsed.
import sys
if sys.hexversion < 0x02020000:
  print 'This script requires Python 2.2 or later.'
  print 'Currently run with version: %s' % sys.version
  sys.exit(1)

import binascii
import fnmatch
import gzip
import os
import tempfile
import unittest
import xml.dom.minidom
import sitemap_gen

# True and False were introduced in Python2.2.2
try:
  testTrue=True
  del testTrue
except NameError:
  True=1
  False=0


class URLCounter(object):
  """Counts returned URLs, determines how many valid v. invalid we get.
  This is a helper for consuming what the many Input* objects produce."""
  def __init__(self, root, print_invalid, expected):
    """Input:
      root          :: root URL for calling the URL's own Validate()
      print_invalid :: print to output all invalid URLs
      expected      :: sequence of wildcard filters to validate against
    """
    self._root     = root
    self._print    = print_invalid
    self._expected = expected
    self._valid    = 0
    self._invalid  = 0
  #end def __init__

  def Reset(self):
    """Reset our counts without harming the validity filters."""
    self._valid   = 0
    self._invalid = 0
  #end def Reset

  def Valid(self):
    """Returns number of valid URLs."""
    return self._valid
  #end def Valid
 
  def Invalid(self):
    """Returns number of invalid URLs."""
    return self._invalid
  #end def Valid

  def Count(self, url, allow_fragment):
    """The 'please consume this URL' function called by the URL producer."""
    valid = True
    if valid:
      valid = url.Validate(self._root, allow_fragment)
    if valid:
      for filter in self._expected:
        valid = fnmatch.fnmatchcase(url.loc, filter)
        if valid:
          break
    if valid:
      self._valid = self._valid + 1
    else:
      if self._print:
        url.Log(prefix='URLCounter', level=0)
      self._invalid = self._invalid + 1
  #end def Count
#end class URLCounter


class TestSiteMap(unittest.TestCase):
  """Tests the sitemap_gen application."""

  def testTimestampISO8601(self):
    """ Checks whether the TimestampISO8601 function works. """
    self.assertEqual(sitemap_gen.TimestampISO8601(23),
                     '1970-01-01T00:00:23Z')
    self.assertEqual(sitemap_gen.TimestampISO8601(549876543),
                     '1987-06-05T07:29:03Z')
  #end def testTimestampISO8601

  def testExpandPathAttribute(self):
    """ Verifies our path globbing function works. """
    temppath = tempfile.mktemp()
    tempwild = tempfile.tempdir
    if tempwild:
      tempwild = tempwild + os.sep
    tempwild = tempwild + '*'
    try:
      open(temppath, 'w').close()

      dict1 = {}
      dict2 = {'alpha' : 'beta', 'path' : 'DoesNotExist987654321.xyz'}
      dict3 = {'alpha' : 'beta', 'path' : tempwild}

      res1  = sitemap_gen.ExpandPathAttribute(dict1, 'path')
      res2  = sitemap_gen.ExpandPathAttribute(dict2, 'path')
      res3  = sitemap_gen.ExpandPathAttribute(dict3, 'path')

      self.assertEqual(len(res1), 1)
      self.assertEqual(res1[0], dict1)

      self.assertEqual(len(res2), 1)
      self.assertEqual(res2[0], dict2)
   
      self.assert_(len(res3) >= 1)
      anymatch = False
      for res in res3:
        path = res['path']
        if path.find(temppath) >= 0:
          anymatch = True
        self.assertEqual(res['alpha'], 'beta')
      self.assert_(anymatch)

    finally:
      os.unlink(temppath)
  #end def testExpandPathAttribute

  def testEncoder(self):
    """ Tests minimal functionality of the learning Unicode codec """
    ENC_UTF8      = 'UTF-8'
    ENC_LATIN1    = 'ISO-8859-1'
    ENC_CYRILLIC  = 'ISO-8859-5'

    STR1_LATIN1   = 'has an ' + binascii.a2b_hex('FC') + 'mlat'
    STR1_UTF8     = 'has an ' + binascii.a2b_hex('C3BC') + 'mlat'
    STR1_UCS2     = 'has an ' + unichr(252) + 'mlat'

    STR2_LATIN1   = 'DRAGON' + binascii.a2b_hex('A7') + '!'
    STR2_CYRILLIC = 'DRAGON' + binascii.a2b_hex('FD') + '!'
    STR2_UCS2     = 'DRAGON' + unichr(167) + '!'
    
    # Spawn our own encoder instance so we don't abuse the module one.
    encoder = sitemap_gen.Encoder()

    # Convert Latin-1 to UTF-8, by way of Unicode
    encoder.SetUserEncoding(ENC_LATIN1)
    self.assertEqual(encoder.WidenText(STR1_LATIN1, None), STR1_UCS2)
    self.assertEqual(encoder.NarrowText(STR1_UCS2, ENC_UTF8), STR1_UTF8)

    # Test learning.  STR1 has no Cyrillic equivalent, STR2 just changes.
    encoder.SetUserEncoding(None)
    encoder._learned = []
    self.assertEqual(encoder.WidenText(STR2_CYRILLIC, ENC_CYRILLIC), STR2_UCS2)
    self.assertEqual(encoder.WidenText(STR2_CYRILLIC, None), STR2_UCS2)
    self.assertEqual(encoder.NarrowText(STR1_UCS2, None), STR1_UTF8)
    self.assert_(not encoder._learned)
    self.assertEqual(encoder.NarrowText(STR1_UCS2, ENC_LATIN1), STR1_LATIN1)
    self.assertEqual(encoder.NarrowText(STR1_UCS2, None), STR1_LATIN1)
    self.assertEqual(encoder.NarrowText(STR2_UCS2, None), STR2_LATIN1)
  #end def testEncoder

  def testURL(self):
    """ Vigorously tests our URL attribute processing. """

    # Test the IsAbsolute method
    self.assert_(sitemap_gen.URL.IsAbsolute('http://a.b.c/d/e.txt?f=g#h'))
    self.assert_(sitemap_gen.URL.IsAbsolute('http://a.b.c'))
    self.assert_(not sitemap_gen.URL.IsAbsolute('http:///d/e.txt?f=g#h'))
    self.assert_(not sitemap_gen.URL.IsAbsolute('http:a.b.c/d/e.txt?f=g#h'))
    self.assert_(not sitemap_gen.URL.IsAbsolute('a.b.c/d/e.txt?f=g#h'))
    self.assert_(not sitemap_gen.URL.IsAbsolute('/d/e.txt?f=g#h'))

    # Canonicalize our base URL
    BASE_R = 'http://www.example.com/f' + binascii.a2b_hex('F6F6') + '/'
    BASE_C = 'http://www.example.com/f%F6%F6/'
    sitemap_gen.encoder.SetUserEncoding('ISO-8859-1')
    self.assertEqual(sitemap_gen.URL.Canonicalize(BASE_R), BASE_C)

    # Test how canonicalization handles pre-quoted values
    self.assertEqual(sitemap_gen.URL.Canonicalize(
      'http://www.example.com/my%25thing'),
      'http://www.example.com/my%25thing')
    self.assertEqual(sitemap_gen.URL.Canonicalize(
      'http://www.example.com/my%thing'),
      'http://www.example.com/my%25thing')

    # Test IDNA encoding
    # The generator can only do the "right thing" on Python 2.3 or higher
    warn = sitemap_gen.output.num_warns
    if sys.hexversion >= 0x02030000:
      self.assertEqual(sitemap_gen.URL.Canonicalize(
        'http://www.' + unichr(252) + 'mlat.com/' + unichr(252) + 'mlat.txt'),
        'http://www.xn--mlat-zra.com/%FCmlat.txt')
      self.assertEqual(sitemap_gen.output.num_warns, warn)
    else:
      self.assertEqual(sitemap_gen.URL.Canonicalize(
        'http://www.' + unichr(252) + 'mlat.com/' + unichr(252) + 'mlat.txt'),
        'http://www.%FCmlat.com/%FCmlat.txt')
      self.assertEqual(sitemap_gen.output.num_warns, warn + 2)

    # All valid data
    warn = sitemap_gen.output.num_warns
    url1 = sitemap_gen.URL()
    url1.TrySetAttribute('loc', BASE_R + 'bar.html')
    url1.TrySetAttribute('lastmod', '1987-06-05T07:29:03Z')
    url1.TrySetAttribute('changefreq', 'daily')
    url1.TrySetAttribute('priority', '0.3')
    self.assert_(url1.Validate(BASE_C, True))
    self.assertEqual(sitemap_gen.output.num_warns, warn)

    # Valid ref, all else invalid
    warn = sitemap_gen.output.num_warns
    url2 = sitemap_gen.URL()
    url2.TrySetAttribute('loc', BASE_C + 'bar.html')
    url2.TrySetAttribute('lastmod', 'June 1, 2005')
    url2.TrySetAttribute('changefreq', 'every second')
    url2.TrySetAttribute('priority', 'infinite')
    url2.TrySetAttribute('badattr', 'Nope!')
    self.assert_(url2.Validate(BASE_C, True))
    self.assertEqual(sitemap_gen.output.num_warns, warn + 4)

    # Two URLs with same ref should compare equal
    self.assertEqual(url1, url2)

    # A ref not based
    warn = sitemap_gen.output.num_warns
    url3 = sitemap_gen.URL()
    url3.TrySetAttribute('loc', 'http://www.example.com/bar/foo.html')
    self.assert_(not url3.Validate(BASE_C, True))
    self.assertEqual(sitemap_gen.output.num_warns, warn + 1)

    # A fragmentary URL
    warn = sitemap_gen.output.num_warns
    url4 = sitemap_gen.URL()
    url4.TrySetAttribute('loc', '/foo.html')
    self.assert_(not url4.Validate(BASE_C, False))
    self.assertEqual(sitemap_gen.output.num_warns, warn + 1)
    url4.TrySetAttribute('loc', '/xyzzy/foo.html')
    self.assert_(url4.Validate('http://www.example.com/', True))
    self.assertEqual(url4.loc, 'http://www.example.com/xyzzy/foo.html')
    self.assertEqual(sitemap_gen.output.num_warns, warn + 1)

    # Test a whole sequence of good and bad timestamp values
    timestamps_good = [
      '2001',
      '2001-01',
      '2001-01-02',
      '2001-01-03T01:02Z',
      '2001-01-03T01:02:03Z',
      '2001-01-03T01:02:03.0123Z',
      '2001-01-03T01:02+00:00',
      '2001-01-03T01:02:03-99:99',
      '2001-01-03T01:02:03.0123+88:88',
      ]
    timestamps_bad = [
      '2001:01:03T01:02Z',
      '2001-01-03T01:02:03.Z',
      'a2001-01-06T01:02:05-99:99',
      '2001-01-06T01:02:05-99:99Z',
      '2001-1-6T01:02:05-99:99',
      'xyzzy',
      '2001-01-03T01:02:03.1.2Z',
      ]
    warn = sitemap_gen.output.num_warns
    url3.TrySetAttribute('loc', BASE_C + 'foo.html')
    for ts in timestamps_good:
      url3.TrySetAttribute('lastmod', ts)
      self.assert_(url3.Validate(BASE_C, True))
    self.assertEqual(sitemap_gen.output.num_warns, warn)
    for ts in timestamps_bad:
      url3.TrySetAttribute('lastmod', ts)
      self.assert_(url3.Validate(BASE_C, True))
    self.assertEqual(sitemap_gen.output.num_warns, warn + len(timestamps_bad))
  #end def testURL

  def testFilter(self):
    """ Test the filtering object """
    url1 = sitemap_gen.URL()
    url2 = sitemap_gen.URL()
    url1.TrySetAttribute('loc', 'http://www.example.com/foo/bar.html')
    url2.TrySetAttribute('loc', 'http://www.example.com/bar/foo.html')
    url1.Validate('http://www.example.com', True)
    url2.Validate('http://www.example.com', True)

    # Arguments
    error = sitemap_gen.output.num_errors
    args_bad = [
      {},
      {'pattern' : '*', 'type' : 'unknown'},
      {'pattern' : '*', 'type' : 'wildcard', 'action' : 'look pretty'},
      {'pattern' : '*', 'type' : 'regexp'},
      ]
    error = sitemap_gen.output.num_errors
    for args in args_bad:
      sitemap_gen.Filter(args)
    self.assertEqual(sitemap_gen.output.num_errors, error + len(args_bad))

    # Wildcard
    filt_w = sitemap_gen.Filter({'pattern' : '*/foo/*', 'type' : 'wildcard' })
    self.assertEqual(filt_w.Apply(url1), False)
    self.assertEqual(filt_w.Apply(url2), None)

    # Regexp
    filt_r = sitemap_gen.Filter({'pattern' : '/bar/[^/]+$', 'type' : 'REGEXP',
                                 'action' : 'PASS'})
    self.assertEqual(filt_r.Apply(url1), None)
    self.assertEqual(filt_r.Apply(url2), True)
  #end def testFilter

  def Count(self, url, allow_fragment):
    if url.Validate('http://www.example.com/', allow_fragment):
      self.valid_urls = self.valid_urls + 1
    else:
      self.invalid_urls = self.invalid_urls + 1
  #end def Count
  valid_urls   = 0
  invalid_urls = 0

  def testInputURL(self):
    """ Test one of the Input mechanisms: InputURL """
    
    # Feed a couple URLs.  Make sure we get an error on extra attributes.
    self.valid_urls   = 0
    self.invalid_urls = 0
    error = sitemap_gen.output.num_errors
    warn = sitemap_gen.output.num_warns
    generator1 = sitemap_gen.InputURL({'href' : 'http://www.example.com/1',
                                      'priority' : '0.3',
                                      'lastmod' : '2004-11-14T01:00-07:00',
                                      'changefreq' : 'hourly',
                                      'unknownInURL' : 'attribute'})
    generator2 = sitemap_gen.InputURL({'href' : 'http://www.example.com/2',
                                      'priority' : '0.3',
                                      'lastmod' : '2004-11-14T01:00-07:00',
                                      'changefreq' : 'hourly'})
    generator1.ProduceURLs(self.Count)
    generator2.ProduceURLs(self.Count)
    self.assertEqual(self.valid_urls, 1)
    self.assertEqual(self.invalid_urls, 0)
    self.assertEqual(sitemap_gen.output.num_errors, error + 1)
    self.assertEqual(sitemap_gen.output.num_warns, warn)
  #end def testInputURL

  def testInputURLList(self):
    """ Test one of the Input mechanisms: InputURLList """
    path = tempfile.mktemp()
    file = open(path, 'w')

    try:
      # Create a temp file we can read
      testText = """
http://www.example.com/foo/bar unknownInURLList=attribute
http://www.example.com/foo/xxx.pdf lastmod=2003-12-31T14:05:06+00:00
http://www.example.com/foo/yyy?x=12&y=23   changefreq=weekly   priority=0.3
      """
      file.write(testText)
      file.close()
      
      # Feed in the data.  Make sure we get a warning on the bad attribute.
      self.valid_urls   = 0
      self.invalid_urls = 0
      warn = sitemap_gen.output.num_warns
      generator = sitemap_gen.InputURLList({'path' : path})
      generator.ProduceURLs(self.Count)
      self.assertEqual(self.valid_urls, 3)
      self.assertEqual(self.invalid_urls, 0)
      self.assertEqual(sitemap_gen.output.num_warns, warn + 1)

    finally:
      os.unlink(path)
  #end def testInputURLList

  def testInputDirectory(self):
    """Test one of the Input mechanisms: InputDirectory.
    I've seen a subtle path-bug appear when going into sub-sub-directories
    that didn't under just sub-directories.  So we go to the trouble to
    make a whole little directory tree to read.
    """
    counter = URLCounter('http://www.example.com/', True, (
      'http://www.example.com/',
      'http://www.example.com/one.html',
      'http://www.example.com/two.html',
      'http://www.example.com/xyzzy/',
      'http://www.example.com/xyzzy/thr.html',
      'http://www.example.com/xyzzy/zyxxy/',
      'http://www.example.com/xyzzy/zyxxy/fiv.html',
      ))
    path = tempfile.mktemp()
    subpath = os.path.join(path, 'xyzzy')
    subsubpath = os.path.join(subpath, 'zyxxy')
   
    try:
      # Create some dummy empty files
      os.mkdir(path)
      os.mkdir(subpath)
      os.mkdir(subsubpath)
      path_one = os.path.join(path, 'one.html')
      path_two = os.path.join(path, 'two.html')
      path_thr = os.path.join(subpath, 'thr.html')
      path_for = os.path.join(subpath, 'default.html')
      path_fiv = os.path.join(subsubpath, 'fiv.html')
      open(path_one, 'w').close()
      open(path_two, 'w').close()
      open(path_thr, 'w').close()
      open(path_for, 'w').close()
      open(path_fiv, 'w').close()

      # Feed in the data.  There should be no warnings.
      warn = sitemap_gen.output.num_warns
      generator = sitemap_gen.InputDirectory({'path' : path,
        'url' : 'http://www.example.com/', 'default_file' : 'default.html'},
        'http://www.example.com/')
      generator.ProduceURLs(counter.Count)
      self.assertEqual(counter.Valid(), 7)
      self.assertEqual(counter.Invalid(), 0)
      self.assertEqual(sitemap_gen.output.num_warns, warn)

    finally:
      os.unlink(path_one)
      os.unlink(path_two)
      os.unlink(path_thr)
      os.unlink(path_for)
      os.unlink(path_fiv)
      os.rmdir(subsubpath)
      os.rmdir(subpath)
      os.rmdir(path)
  #end def testInputDirectory

  def testInputAccessLogCLF(self):
    """ Test one of the Input mechanisms: InputAccessLog (Common logfile) """
    path = tempfile.mktemp()
    file = open(path, 'w')

    try:
      # Create a temp file we can read
      testText = '''
msnbot.msn.com - - [15/May/2005:07:46:50 -0700] "GET /~guest/main/ HTTP/1.0" 200 5670
221.216.237.71 - - [15/May/2005:07:59:25 -0700] "GET /~guest/bookmark/ HTTP/1.1" 200 39195
221.216.237.71 - - [15/May/2005:07:59:27 -0700] "GET /favicon.ico HTTP/1.1" 404 217
c-67-161-121-105.hsd1.wa.comcast.net - - [15/May/2005:11:17:23 -0700] "GET /picts/top.jpg HTTP/1.1" 200 10044
cpe-65-24-155-46.columbus.res.rr.com - - [16/May/2005:22:53:07 -0700] "HEAD http://www.example.com/~guest HTTP/1.1" 200 0
      '''
      file.write(testText)
      file.close()
      
      # Feed in the data
      self.valid_urls   = 0
      self.invalid_urls = 0
      warn = sitemap_gen.output.num_warns
      generator = sitemap_gen.InputAccessLog({'path' : path})
      generator.ProduceURLs(self.Count)
      self.assertEqual(self.valid_urls, 4)
      self.assertEqual(self.invalid_urls, 0)
      self.assertEqual(sitemap_gen.output.num_warns, warn)

    finally:
      os.unlink(path)
  #end def testInputAccessLogCLF

  def testInputAccessLogELF(self):
    """ Test one of the Input mechanisms: InputAccessLog (Extended logfile) """
    path = tempfile.mktemp()
    file = open(path, 'w')

    try:
      # Create a temp file we can read
      testText = '''
#Software: Microsoft Internet Information Services 6.0
#Version: 1.0
#Date: 2004-03-22 09:20:36
#Fields: date time s-ip cs-method cs-uri-stem cs-uri-query s-port cs-username c-ip cs(User-Agent) sc-status sc-substatus sc-w
in32-status
2004-03-22 09:20:36 192.168.0.58 GET /Default.htm - 80 - 4.5.11.3 Mozilla/4.0+(compatible;+MSIE+5.5;+Windows+98) 200 0 64
2004-03-22 09:22:58 192.168.0.58 GET /Default.htm - 80 - 24.87.160.82 Mozilla/4.0+(compatible;+MSIE+5.5;+Windows+98) 200 0 6
4
      '''
      file.write(testText)
      file.close()
      
      # Feed in the data
      self.valid_urls   = 0
      self.invalid_urls = 0
      warn = sitemap_gen.output.num_warns
      generator = sitemap_gen.InputAccessLog({'path' : path})
      generator.ProduceURLs(self.Count)
      self.assertEqual(self.valid_urls, 2)
      self.assertEqual(self.invalid_urls, 0)
      self.assertEqual(sitemap_gen.output.num_warns, warn)

    finally:
      os.unlink(path)
  #end def testInputAccessLogELF

  def testInputSitemap(self):
    """ Test one of the Input mechanisms: InputSitemap """
    path1 = tempfile.mktemp('.xml')
    path2 = tempfile.mktemp('.xml')
    path3 = tempfile.mktemp('.xml')
    path4 = tempfile.mktemp('.xml')
    file1 = None
    file2 = None
    file3 = None
    file4 = None

    index = '''<?xml version="1.0" encoding="UTF-8"?>
<sitemapindex
  xmlns="http://www.google.com/schemas/sitemap/0.84"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84
                     http://www.google.com/schemas/sitemap/0.84/siteindex.xsd">
 <sitemap>
  <loc>http://www.example.com/path/to/%(PATH2)s</loc>
  <lastmod>2005-07-15T17:41:22Z</lastmod>
 </sitemap>
 <sitemap>
  <loc>http://www.example.com/path/to/%(PATH3)s</loc>
  <lastmod>2005-07-15T17:41:22Z</lastmod>
 </sitemap>
</sitemapindex>
'''
    content1 = '''<?xml version="1.0" encoding="UTF-8"?>
<urlset
  xmlns="http://www.google.com/schemas/sitemap/0.84"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84
                      http://www.google.com/schemas/sitemap/0.84/sitemap.xsd">
 <url>
  <loc>http://www.example.com/another/path/to/samplefile1.html</loc>
  <lastmod>2005-07-13T00:00:12Z</lastmod>
  <priority>0.5000</priority>
 </url>
 <url>
  <loc>http://www.example.com/another/path/to/samplefile2.html</loc>
  <lastmod>2004-11-16T20:22:06Z</lastmod>
  <priority>0.5000</priority>
 </url>
</urlset>
'''
    content2 = '''<?xml version="1.0" encoding="UTF-8"?>
<urlset
  xmlns="http://www.google.com/schemas/sitemap/0.84"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84
                      http://www.google.com/schemas/sitemap/0.84/sitemap.xsd">
 <url badSitemapAttr="Hello, World!">
  <loc>http://www.example.com/another/path/to/samplefile3.html</loc>
  <lastmod>2005-07-13T00:00:12Z</lastmod>
  <priority>0.5000</priority>
 </url>
 <url>
  <loc>http://www.example.com/another/path/to/samplefile4.html</loc>
  <lastmod>2004-11-16T20:22:06Z</lastmod>
  <priority>0.5000</priority>
 </url>
</urlset>
'''
    # This index is invalid because it points to another index file.
    badindex = '''<?xml version="1.0" encoding="UTF-8"?>
<sitemapindex
  xmlns="http://www.google.com/schemas/sitemap/0.84"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
  xsi:schemaLocation="http://www.google.com/schemas/sitemap/0.84
                     http://www.google.com/schemas/sitemap/0.84/siteindex.xsd">
 <sitemap>
  <loc>http://www.example.com/path/to/%(PATH2)s</loc>
  <lastmod>2005-07-15T17:41:22Z</lastmod>
 </sitemap>
 <sitemap>
  <loc>http://www.example.com/path/to/%(PATH1)s</loc>
  <lastmod>2005-07-15T17:41:22Z</lastmod>
 </sitemap>
</sitemapindex>
'''

    # Make a nice complicated set of two index files and two sitemaps.
    try:
      file1 = open(path1, 'wt')
      file2 = open(path2, 'wt')
      file3 = open(path3, 'wt')
      file4 = open(path4, 'wt')
      file1.write(index % {
        'PATH1' : os.path.basename(path1),
        'PATH2' : os.path.basename(path2),
        'PATH3' : os.path.basename(path3)})
      file2.write(content1)
      file3.write(content2)
      file4.write(badindex % {
        'PATH1' : os.path.basename(path1),
        'PATH2' : os.path.basename(path2),
        'PATH3' : os.path.basename(path3)})
      file1.close()
      file1 = None
      file2.close()
      file2 = None
      file3.close()
      file3 = None
      file4.close()
      file4 = None

      # Feed in the good data.  Make sure we get warned on the bad attribute.
      self.valid_urls   = 0
      self.invalid_urls = 0
      warn = sitemap_gen.output.num_warns
      generator = sitemap_gen.InputSitemap({'path' : path1})
      generator.ProduceURLs(self.Count)
      self.assertEqual(self.valid_urls, 4)
      self.assertEqual(self.invalid_urls, 0)
      self.assertEqual(sitemap_gen.output.num_warns, warn + 1)

      # Feed in the bad data.  Should error once on the bad index and once
      # because it aborts processing the XML.
      self.valid_urls   = 0
      self.invalid_urls = 0
      errors = sitemap_gen.output.num_errors
      generator = sitemap_gen.InputSitemap({'path' : path4})
      generator.ProduceURLs(self.Count)
      self.assertEqual(self.valid_urls, 2)
      self.assertEqual(self.invalid_urls, 0)
      self.assertEqual(sitemap_gen.output.num_errors, errors + 2)

    finally:
      if file1 is not None:
        file1.close()
      if file2 is not None:
        file2.close()
      if file3 is not None:
        file3.close()
      if os.path.exists(path1):
        os.unlink(path1)
      if os.path.exists(path2):
        os.unlink(path2)
      if os.path.exists(path3):
        os.unlink(path3)
  #end def testInputSitemap

  def testFilePathGenerator(self):
    """ Test our iteration of filenames """
    gen1 = sitemap_gen.FilePathGenerator()
    gen2 = sitemap_gen.FilePathGenerator()
    gen3 = sitemap_gen.FilePathGenerator()
    self.assert_(gen1.Preload('/tmp/bar/foo.xml'))
    self.assert_(gen2.Preload('foo.xml.gz'))
    self.assert_(gen3.Preload('/foo.gz'))
    self.assert_(not gen1.is_gzip)
    self.assert_(    gen2.is_gzip)
    self.assert_(    gen3.is_gzip)
    self.assertEqual(gen1.GeneratePath(0),
                     os.path.normpath('/tmp/bar/foo.xml'))
    self.assertEqual(gen2.GeneratePath(1),'foo1.xml.gz')
    self.assertEqual(gen1.GeneratePath('_index.xml'),
                     os.path.normpath('/tmp/bar/foo_index.xml'))
    self.assertEqual(gen1.GenerateURL('_index.xml', 'http://www.example.com/'),
                     'http://www.example.com/foo_index.xml')
    self.assertEqual(gen1.GenerateURL(2, 'http://www.example.com/'),
                     'http://www.example.com/foo2.xml')
    self.assertEqual(gen2.GenerateWildURL('http://www.example.com/'),
                     'http://www.example.com/foo*.xml.gz')
  #end def testFilePathGenerator

  def testSitemap(self):
    """Test a basic config of the overall sitemap class."""
    path1 = tempfile.mktemp()
    path2 = tempfile.mktemp(".xml.gz")
    file = open(path1, 'w')

    try:
      # Create a temp file we can read
      testText = '''<?xml version="1.0" encoding="UTF-8"?>
<site
  base_url="http://www.example.com/"
  store_into="%(OUTPUTFILENAME)s"
  default_encoding="UTF-8"
  verbose="3"
>
  <url href="http://www.example.com/.htaccess" />
  <url href="http://www.example.com/foo/bar.html" />
  <url href="http://www.example.com/foo/bar.gif" />
  <url href="http://www.example.com/foo/bar.html" />
  <url href="http://www.example.com/percent%%%%percent.html" />
  <url href="http://www.example.com/&#252;mlat.html" />
  <filter action="drop" type="regexp" pattern="/\.[^/]*$" />
</site>
'''
      file.write(testText % {'OUTPUTFILENAME' : path2})
      file.close()

      # Bring up the engine
      warn = sitemap_gen.output.num_warns
      error = sitemap_gen.output.num_errors
      sitemap = sitemap_gen.CreateSitemapFromFile(path1, True)
      self.assert_(sitemap)
      sitemap.Generate()
      self.assertEqual(sitemap_gen.output.num_warns, warn)
      self.assertEqual(sitemap_gen.output.num_errors, error)

      # Verify we got readable XML out of it
      file = gzip.open(path2, mode='rb')
      result = file.read()
      file.close()
      dom = xml.dom.minidom.parseString(result)
      self.assertEqual(len(dom.getElementsByTagName('url')), 4)
      self.assert_(result.find('http://www.example.com/foo/bar.html') > 0)
      self.assert_(result.find('http://www.example.com/foo/bar.gif') > 0)
      self.assert_(result.find('%25%25') > 0)
      self.assert_(result.find('%C3%BC') > 0)
    finally:
      if os.path.exists(path2):
        os.unlink(path2)
      os.unlink(path1)
  #end def testSitemap
    
#end class TestSiteMap

#
# __main__
#

if __name__ == '__main__':
  unittest.main()
