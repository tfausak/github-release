# [GitHub Release][]

[![CI](https://github.com/tfausak/github-release/actions/workflows/ci.yml/badge.svg)](https://github.com/tfausak/github-release/actions/workflows/ci.yml)
[![Hackage](https://badgen.net/hackage/v/github-release)](https://hackage.haskell.org/package/github-release)

GitHub Release is a command-line utility for uploading files to GitHub
releases.

Once you've got it, run it like so:

``` sh
github-release upload \
  --token '...' \
  --owner 'someone' \
  --repo 'something' \
  --tag 'v1.2.3' \
  --file 'path/to/example.tgz' \
  --name 'example-1.2.3.tgz'
```

You can generate a token on the [personal access tokens][] page of your
personal settings. The `file` option is the path to the local file you want to
upload. The `name` option is what the file should be called on the GitHub
release.

GitHub Release is written in Haskell. If you want to build it yourself or use
it in your project, you'll want to get [Stack][]. Once you've done that, you
can install and use it from the command line.

``` sh
stack --resolver nightly install github-release
stack exec -- github-release upload # as above ...
```

Or you can use it from Haskell.

``` hs
import qualified GitHubRelease
GitHubRelease.upload
    "..."                 -- token
    "someone"             -- owner
    "something"           -- repo
    "1.2.3"               -- tag
    "path/to/example.tgz" -- file
    "example-1.2.3.tgz"   -- name
```

Inspired by <https://github.com/aktau/github-release>.

[GitHub Release]: https://github.com/tfausak/github-release
[the latest release]: https://github.com/tfausak/github-release/releases/latest
[personal access tokens]: https://github.com/settings/tokens
[Stack]: http://docs.haskellstack.org/en/stable/README/
