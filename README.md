# hs-nhentai
Scraping/downloading [nhentai](https://nhentai.net) galleries and JSONs.

## Usage

### Help
```bash
$ nhentai --help
Usage: nhentai [-l|--log-level LOG_LEVEL] COMMAND
  A scraper/downloader for nhentai.net

Available options:
  -l,--log-level LOG_LEVEL Set log level. Prints possible inputs on
                           error. (default: LevelDebug)
  -h,--help                Show this help text

Available commands:
  download                 Download thumbnails
  version                  Print version
```

### Download Help
```bash
$ nhentai download --help
Usage: nhentai download ((-g|--gallery-ids GALLERY_IDS) | (-a|--all))
                        [-t|--num-threads NUM_THREADS] [-2|--output-config-2]
                        [-o|--output-dir OUTPUT_DIR]
                        [--warn-least-size NUM_BYTES]
                        [--warn-most-duration DURATION]
                        [-I|--download-page-thumbnail]
                        [-i|--download-page-image]
  Download thumbnails

Available options:
  -g,--gallery-ids GALLERY_IDS
                           List of ids fo galleries to be downloaded, e.g.
                           177013 or 177013,166013
  -a,--all                 Download galleries from the latest one to the
                           beginning
  -t,--num-threads NUM_THREADS
                           Set the number of threads used in download
                           images (default: Refined 1)
  -2,--output-config-2     Use another directory format, instead of gid ->
                           dest_dir/<gid>/, the directory format will become gid
                           -> dest_dir/<div gid 1000>/<gid>
  -o,--output-dir OUTPUT_DIR
                           Set the output directory (default: "galleries")
  --warn-least-size NUM_BYTES
                           Set downloaded content's size threshold before
                           warning
  --warn-most-duration DURATION
                           Set download duration threshold before warning
  -I,--download-page-thumbnail
                           Download page thumbnails of a gallery
  -i,--download-page-image Download page images of a gallery
  -h,--help                Show this help text
```
## Examples

Download thumbnail page images and page thumbnails from https://nhentai.net/g/177013 to `galleries/` with 3 threads:
```bash
$ nhentai --log-level debug download --gallery-ids 177013 --download-page-image --download-page-thumbnail --num-threads 3 --output-dir galleries
```

Download from the latest one to the beginning with another directory format:
```bash
$ nhentai --log-level debug download --all --download-page-image --num-threads 8 --output-dir another_dest --output-config-2 --warn-most-duration 5 --warn-least-size 2000
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
