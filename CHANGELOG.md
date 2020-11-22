## 0.1.2.0

- rename download option `--num-threads` to `--threads`
- remove download option `-a`
- download option `-g` can no longer accept comma-seperated lists, only a single element is allowed
- add download option `-f` for gallery list file input
- add timing for `download` command

## 0.1.1.1

- forgot to update cabal file

## 0.1.1.0

CHANGELOG.md added

- add `latest-gid` command
- use URI instead of Text
	- in APITag
- change logger format
- rename `download` options
	- `-I,--download-page-thumbnail` -> `-T,--thumbnails`
	- `-i,--download-page-image` -> `-I,--images`
- refactor code
	- use lens more often
	- use refined in tests
	- use exports
	- rename some datatypes
	- rename `url`s to `uri`s
- reduce `README.md` cause its annoying to manually change it everytime
- update ghc-options
- fix command descriptions
	- fix progDesc Of the `download` command
	- fix grammar in `num_threads_parser`
- fix logger to print to stderr instead of stdout

## 0.1.0.1

GitHub project got renamed.

- update github links (https://github.com/tensorknower69/hs-nhentai -> https://github.com/tensorknower69/nhentai)

## 0.1.0.0

Initial version.

CHANGELOG.md doesn't exist until `0.1.1.0`.

- add test
	- use tasty
- add library
	- parse comment api json
	- parse gallery api json
	- parse home page (for getting the latest gid)
- add cli
	- add logger (stdout, this is actually a bug, fixed in 0.1.1.0)
	- add log level filter `-l`
	- add `version` command
	- add `download` command
	- redownload on `503`
	- add download num threads option `-t`
	- add download page thumbnails option `-I`
	- add download page images option `-i`
	- add download gallery ids selection `-g` (can be comma-seperated)
	- add download all option `-a`
	- add download dest output dir option `-o`
	- add download second output dir format option `-2`
	- add download `--warn-least-size`
	- add download `--warn-most-duration`
