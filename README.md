# nhentai

API & Program for scraping/downloading [nhentai](https://nhentai.net) galleries and JSONs.

## Examples

Download the gallery info of [177013](https://nhentai.net/g/177013) (`-g 177013`) to current directory (`-o .`) with 100 threads (`-t 100`) and debug logging (`-l <level>`):
```bash
$ nhentai -l debug download -o . -t 100 -g 177013
$ file 177013/gallery.json
177013/gallery.json: JSON data
```

Download page images (`-I`) and page thumbnails (`-T`) starting from the latest gallery to the first gallery (`-f <list-file>`) to `galleries/` using an alternative output format (`-2`) with 100 threads and debug logging:
```bash
$ nhentai -l debug download -I -T -o galleries/ -t 100 -2 -f <(seq `nhentai latest-gid` -1 1)
$ tree galleries
galleries
├── ...
├── 360
│   └── ...
├── 361
│   └── ...
├── 362
│   ├── ...
│   ├── 362643
│   │   └── ...
│   ├── 362644
│   │   ├── 1.jpg          # page image
│   │   ├── 1t.jpg         # page thumbnail
│   │   ├── 2.jpg
│   │   ├── 2t.jpg
│   │   ├── ...
│   │   └── gallery.json   # gallery info
│   └── ...
└── ...
```

## Installation

### Linux

```bash
$ git clone https://github.com/tensorknower69/nhentai
$ cd nhentai/
$ stack install # build and copy the executable to ~/.local/bin/
$ nhentai --help # testing, $PATH must have ~/.local/bin or else the shell won't be able to find the executable
```

## Uninstallation

### Linux
```bash
$ rm ~/.local/bin/nhentai
```

## Inspiration

- https://github.com/RicterZ/nhentai
