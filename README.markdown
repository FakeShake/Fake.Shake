# Fake.Shake

*Build what you need, when you need it.*

Based on the [Shake paper](http://community.haskell.org/~ndm/downloads/paper-shake_before_building-10_sep_2012.pdf), Fake.Shake stores a cache of previous builds and only rebuilds what you need when you run it.

Differences from the paper/Haskell Shake, or implementation details:

* Assumes you'll be using Fake helpers, will integrate with them as we go along
* No concurrency (yet); the State type will need some amendments to allow for it
* Rules are defined a little differently, see [Fake.Shake.DefaultRules.fsx](Fake.Shake.DefaultRules.fsx)
* Currently using SHA1 of content of files rather than last modified time - will revisit if it causes performance issues
* Using FsPickler to store cache results

Fake.Shake builds itself; to make the bootstrapping easier it doesn't use an fsproj, but is
instead compiled from a set fsx script files. Have a look at [build.fsx](build.fsx) for the
rule that compiles them. That file also contains the packaging and testing rules; changes to
one should not require a rebuild of the other, but changing any dependency versions or
source files for the main Fake.Shake.dll will cause all three to rebuild.

Have fun, send pull requests!
