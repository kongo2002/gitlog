# gitlog - git log formatter

[![build](https://api.travis-ci.org/kongo2002/gitlog.svg)][travis]


## Building

You can build **gitlog** using *cabal*:

    $ cabal install --dependencies-only
    $ cabal configure
    $ cabal build

Maybe you want to create a cabal sandbox before:

    $ cabal sandbox init


## Usage

**gitlog** will operate on the git repository in the current working directory
by default. That's why usually you have to specify the commit range only:

    $ gitlog origin/master feature-branch

However you may specify the git directory with the `-d` option as well:

    $ gitlog origin/master master -d ~/repos/gitlog


[travis]: https://travis-ci.org/kongo2002/gitlog/
