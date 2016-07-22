#!/usr/bin/env python

import os, sys#, shutil

fn = sys.argv[1]
fnbase = os.path.basename(fn)
fnupper= fnbase.upper().replace('.','_')


f = open(fn, 'r')
first = f.readline()

if first.split() == [ '#ifndef', fnupper]:
    guarded = True
else:
    guarded = False

if not guarded:
    print "#ifndef %s\n#define %s\n"% (fnupper,fnupper)

print first,
for l in f:
    print l,

if not guarded:
    print "\n#endif /* %s */"% fnupper

f.close()
