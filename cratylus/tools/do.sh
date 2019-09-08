#!/bin/bash
./s2cr.py _test.s 2>/dev/null && ./crc.py _test.crm 2>/dev/null && gcc -o _test.bin _test.c -lgmp && ./_test.bin
