#!/usr/bin/env python3.6
import os
import sys


HEADER_END = 0x14f


def main():
    if len(sys.argv) != 2:
        progname = os.path.basename(sys.argv[0])
        sys.stderr.write(f"usage: {progname} <rom_path>\n")
        return 1

    romname = os.path.abspath(sys.argv[1])
    if not os.path.isfile(romname):
        sys.stderr.write(
            f"given ROM path '{romname}' is not a file or does not exist\n")
        return 1

    with open(romname, 'rb') as fh:
        data = fh.read()

    basename = os.path.splitext(os.path.basename(romname))[0]
    newname = basename + ".dump"
    with open(newname, 'wb') as fh:
        fh.write(data[HEADER_END+1:])


if __name__ == '__main__':
    sys.exit(main())
