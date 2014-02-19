Sokoban
=======

A Sokoban implementation using Haskell and Gloss.

Currently uses David W. Skinner's Sasquatch packs for levels: http://users.bentonrea.com/~sasquatch/sokoban/

Controls:

* Move: Arrow keys

* Retry: r

If you wish to load a new pack of levels from the sasquatch pack, simply run:

    $ ./sokoban sasquatch "filename.txt"


which should parse and load the new pack, but note: this will wipe any saved progress.

To reset your progress:

    $ ./sokoban reset

To play from a certain level:

    $ ./sokoban level [level number]

Note: When playing like this, your progress will not save.
