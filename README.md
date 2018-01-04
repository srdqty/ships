# ships

## Command line utility to apply IPS patches

```
Usage: ships [-v|--version] (-p|--ips-filename IPS_FILENAME)
             [-i|--in-file INPUT_FILENAME] [-o|--out-file OUTPUT_FILENAME]
  ships -- apply IPS patch

Available options:
  -h,--help                Show this help text
  -v,--version             Show version
  -p,--ips-filename IPS_FILENAME
                           Name of IPS file to read and use for patching
  -i,--in-file INPUT_FILENAME
                           Optional input filename. Otherwise reads stdin
  -o,--out-file OUTPUT_FILENAME
                           Optional input filename. Otherwise reads stdin
```

## Correctness

I have only tested on two roms that I wanted to play (and it worked!).
See the examples folder for the IPS files and corresponding readme files.


## References

http://fileformats.archiveteam.org/wiki/IPS_(binary_patch_format)

https://zerosoft.zophar.net/ips.php

https://wiki.haskell.org/Dealing_with_binary_data

https://hackage.haskell.org/package/binary

https://hackage.haskell.org/package/bytestring
