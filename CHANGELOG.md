## 0.1.1.0


## 0.1.0.1

GitHub project got renamed.

- update github links (https://github.com/tensorknower69/hs-nhentai -> https://github.com/tensorknower69/nhentai)

## 0.1.0.0

Initial version.

CHANGELOG.md didn't exist yet.

- test
	- use tasty

- library
	- parse comment api json
	- parse gallery api json
	- parse home page (for getting the latest gid)

- cli
	- add logger (stdout, this is actually a bug, fixed in 0.1.1.0)
	- add log level filter `-l`
	- add `version` command
	- add `download` command
	- redownload on `503`
	- add download num threads option `-t`
	- add download page thumbnails option `-I`
	- add download page images option `-i`
	- add download gallery ids selection `-g` (is comma-seperable)
	- add download all option `-a`
	- add download dest output dir option `-o`
	- add download second output dir format option `-2`
	- add download `--warn-least-size`
	- add download `--warn-most-duration`
