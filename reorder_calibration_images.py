from math import floor
import os, sys
import glob
import re
import shutil

videodir = '/Users/etytel01/Documents/2022/UpAndDown/rawdata/calibration_images0'
outdir = '/Users/etytel01/Documents/2022/UpAndDown/rawdata/calibration_images'

step = 50

for f in glob.glob(os.path.join(videodir, 'camera*.jpg')):
    m = re.search('camera-(\d)-(\d+)', f)

    fr = int(m.group(2))
    if fr % step == 0:
        frout = floor(fr / step)

        if m.group(1) == '1':
            outname = 'camera-{}-{:03d}.jpg'.format('rear', frout)
        else:
            outname = 'camera-{}-{:03d}.jpg'.format('lateral', frout)

        shutil.copyfile(f, os.path.join(outdir, outname))