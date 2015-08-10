# Fake.Shake

*Build what you need, when you need it.*

Based on the [Shake paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf), Fake.Shake stores a cache of previous builds and only rebuilds what you need when you run it.

Differences from the paper/Haskell Shake, or implementation details:

* Assumes you'll be using Fake helpers, will integrate with them as we go along
* Rules are defined a little differently, see [Fake.Shake.DefaultRules.fsx](Fake.Shake.DefaultRules.fsx)
* Currently using SHA1 of content of files rather than last modified time - will revisit if it causes performance issues
* Using FsPickler to store cache results

Fake.Shake builds itself; to make the bootstrapping it ``#load``s the Fake.Shake source files rather then
referencing the compiled binary. There shouldn't be any need to do this in any other situation.

The current build contains the packaging and testing rules; changes to files that are depended on by
one should not require a rebuild of the other, but changing anything both steps depend or in the core
assembly will trigger partial rebuilds as required.

Have fun, send pull requests!
