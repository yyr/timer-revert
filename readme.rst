timer-revert-mode
=================

A minor mode to revert buffer for a given time interval.

This is more like `auto-revert-mode` but with a specified time interval. see
`timer-revert-delay`, defaults to 15 seconds.  This is useful because emacs'
auto-revert-mode doesn't have a facility to tell the frequency.

My use case is while writing latex documents, background running make need
some time to finish, usually 5 to 10 seconds. unlike auto-revert-mode which
eager to load as soon as the file changed, this lazily waits for 15
seconds. For best experience, if the background process takes 5 seconds then
`timer-revert-delay` would be around 10 seconds.

License: GPL v3 or later
