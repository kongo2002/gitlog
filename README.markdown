# gitlog - git log formatter

[![build](https://api.travis-ci.org/kongo2002/gitlog.svg)][travis]

> This tool is built to satisfy pretty specific requirements as it is primarily
> used at my work to generate automated changelogs. However this implementation
> may serve as an example on how to achieve similar tasks - at least as a
> reference for myself.


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


### JIRA

**gitlog** can be used to parse and extract references to [JIRA][jira] issues
mentioned in commit messages. Commit messages are expected to have a format
similar to the following:

~~~
Short description of the commit

On the following lines you may give a more detailled description
to the commited changes.

JIRA-250
~~~

In order for **gitlog** to be able to retrieve further information on the
respective [JIRA][jira] issues you have to pass the base URL to the [JIRA][jira]
web service and the *basic auth* credentials like this:

    $ gitlog -j https://jira.some.server/ -a user:pw dev origin/master


[travis]: https://travis-ci.org/kongo2002/gitlog/
[jira]: https://www.atlassian.com/en/software/jira
