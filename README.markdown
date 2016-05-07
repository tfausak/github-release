# [GitHub Release][]

[![Version badge][]][version]
[![Build badge][]][build]
[![Windows build badge][]][windows build]

GitHub Release is a command-line utility for uploading files to GitHub
releases.

``` sh
github-release upload \
  --token '...' \
  --owner 'someone' \
  --repo 'something' \
  --tag '1.2.3' \
  --file 'path/to/example.tgz' \
  --name 'example-1.2.3.tgz'
```

Inspired by <https://github.com/aktau/github-release>.

[GitHub Release]: https://github.com/tfausak/github-release
[Version badge]: https://www.stackage.org/package/github-release/badge/nightly?label=version
[version]: https://www.stackage.org/package/github-release
[Build badge]: https://travis-ci.org/tfausak/github-release.svg
[build]: https://travis-ci.org/tfausak/github-release
[Windows build badge]: https://ci.appveyor.com/api/projects/status/github/tfausak/github-release?svg=true
[windows build]: https://ci.appveyor.com/project/TaylorFausak/github-release
