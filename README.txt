                            nisp.nistilities
                            ================



These are just utilities I have had to write at one time or another. I'd
not say that these are really ready for anyone else to use or even if
they are the best or most efficient implentations. See the changelog in
nisp for change history. I generally log all changes to that project
only.

The interesting things follow:

Table of Contents
=================
1 Helpers 
    1.1 range START END 
    1.2 strip-newlines STRING &optional REPLACE-CHAR 


1 Helpers 
~~~~~~~~~~

1.1 range START END 
====================
    This was written to quickly and easily generate a range of numbers
    or letters for nisp.safe and lisp demonstrations on eighthbit. If
    START is larger then END we count down to END.

    This is well tested for inputs of characters, strings, and integers.

    See the function and the tests both defined in util.lisp

1.2 strip-newlines STRING &optional REPLACE-CHAR 
=================================================
    Given a string, return it with newlines stripped and replaced with
    nothing. If you supply a REPLACE-CHAR this function will replace the
    newline with that char.

    My purpose when making this was to assist in fitting various outputs
    into one irc line for nisp.bot.
