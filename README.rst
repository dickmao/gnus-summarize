|build-status|

+------------------+--------------------------------------+
| dickmao          | When you've been programming as long |
|                  | as I have, you realize more than     |
|                  | ninety percent of the discussion     |
|                  | that takes place before a working    |
|                  | draft is irrelevant. So you          |
|                  | physically take                      |
+------------------+--------------------------------------+
| 2021 maintainer  | Do you have a way to recover the 10% |
|                  | that _is_ relevant without having    |
|                  | the other 90%? If so, please         |
|                  | describe that way, and we will see   |
|                  | if we can follow it.                 |
+------------------+--------------------------------------+

|---| https://lists.gnu.org/archive/html/emacs-devel/2021-08/msg01246.html

Install
=======
Add your Google gemini-api password to ~/.authinfo.  Then,

::

   git clone https://github.com/dickmao/debbugs-summarize.git
   make -C debbugs-summarize install

Usage
=====
::

   M-x debbugs-summarize-bug
   
   Or from gnus summary buffer of gmane.emacs.bugs,
   M-x debbugs-summarize-from-summary

.. |build-status|
   image:: https://github.com/dickmao/debbugs-summarize/workflows/CI/badge.svg?branch=dev
   :target: https://github.com/dickmao/debbugs-summarize/actions
   :alt: Build Status

.. |---| unicode:: U+02014 .. em dash
   :trim:
