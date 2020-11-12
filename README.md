# hs-nhentai
Scraping/downloading [nhentai](https://nhentai.net) galleries and JSONs.

## Examples

Get help:
```bash
$ nhentai --help
```

Download thumbnail images(only that for now) from https://nhentai.net/g/177013 to `galleries/` with 3 threads:
```bash
$ nhentai --log-level debug download --gallery-ids 177013 --num-threads 3 --output-dir galleries
```

Download from the latest one to the beginning with another directory format:
```bash
$ nhentai --log-level info download --all --num-threads 8 --output-config-2 --output-dir another_dest
```

## Installation

### Linux

```bash
$ https://github.com/tensorknower69/hs-nhentai
$ cd hs-nhentai
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
