# [GitHub Release][]

[![Version badge][]][version]
[![Build badge][]][build]

GitHub Release is a command-line utility for uploading files to GitHub
releases.

``` sh
github-release upload \
  --token '...' \
  --owner 'someone' \
  --repo 'something' \
  --tag '1.2.3' \
  --path 'path/to/example.tgz' \
  --name 'example-1.2.3.tgz'
```

[GitHub Release]: https://github.com/tfausak/github-release
[Version badge]: https://www.stackage.org/package/github-release/badge/nightly?label=version
[version]: https://www.stackage.org/package/github-release
[Build badge]: https://travis-ci.org/tfausak/github-release.svg
[build]: https://travis-ci.org/tfausak/github-release
