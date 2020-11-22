# nhentai
Scraping/downloading [nhentai](https://nhentai.net) galleries and JSONs.

## TODO

- add browsing/searching functionality

## Example

Download page images of [177013](https://nhentai.net/g/177013) to current directory (e.g. `./177013/<page>.<extension>`) with 10 threads and debug logging enabled:
```bash
nhentai -l debug download -I -o . -t 100 -g 177013
```

Download page images and page thumbnails from the latest gallery to the first gallery to `galleries/` using an alternative output format (see `nhentai --help`) with 100 threads and debug logging enabled:
```bash
nhentai -l debug download -I -T -o galleries/ -t 100 -2 -f <(seq `nhentai latest-gid` -1 1)
```

## Usage

```bash
$ nhentai --help
```

## Installation

### Linux

```bash
$ https://github.com/tensorknower69/nhentai
$ cd nhentai
$ stack install
```

## Uninstallation

### Linux
```bash
$ rm ~/.local/bin/nhentai
```

## Inspiration

- https://github.com/RicterZ/nhentai

## Why
I just want to write some Haskell.
