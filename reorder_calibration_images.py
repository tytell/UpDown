from math import floor
import os, sys
import glob
import re
import shutil

# change these so that they have the right directory names
videodir = r'D:\UpAndDown\Calibration Images\allframes'
outdir = r'D:\UpAndDown\Calibration Images'

# take every 50th frame
step = 30

for f in glob.glob(os.path.join(videodir, '*.jpg')):
    _, fn = os.path.split(f)
    m = re.search('(.+)-(\d+)', fn)

    fr = int(m.group(2))
    if fr % step == 0:
        frout = floor(fr / step)

        outname = '{}-{:03d}.jpg'.format(m.group(1), frout)
        print(outname)

        shutil.copyfile(f, os.path.join(outdir, outname))