# gitlog - git log formatter

[![build](https://api.travis-ci.org/kongo2002/gitlog.png)][travis]


## Building

You can build **gitlog** using *cabal*:

    $ cabal install --dependencies-only
    $ cabal configure
    $ cabal build

Maybe you want to create a cabal sandbox before:

    $ cabal sandbox init


## Usage

**gitlog** will operate on the git repository in the current working directory
by default. That's why usually have to specify the commit range only:

    $ gitlog origin/master feature-branch

You may specify the git directory with the `-d` option:

    $ gitlog origin/master master -d ~/repos/gitlog


[travis]: https://travis-ci.org/kongo2002/gitlog/
