# -*- coding: utf-8 -*-
"""
Created on Thu Sep  8 11:42:08 2016
Utilities for processing Salton soil core photos
@author: ybcheng
"""


import os
import fnmatch
from shutil import copyfile

import improc
import numpy as np


def copy_mosaic(mosaic_dir, final_dir, file_pattern='IID201609*.jpg'):
    """
    may be a handy little tool that copies soil core mosaic to "final" folder
    the program searches all the stitched photos in a dir (including sub-dir)
    and copies them to the "final" folder if they are not there already
    
    Parameters:
    ----
    mosaic_dir: str
        the folder where stitched photos are
    final_dir: str
        where the stitched photos should go
    file_pattern: str
        stitched photos should have the same name as soil cores
    """ 
        
    mosaics = []
    for root, dirnames, filenames in os.walk(mosaic_dir):
        for filename in fnmatch.filter(filenames, file_pattern):
            mosaics.append(os.path.join(root, filename))
            
    for m in mosaics:
        f = final_dir + os.path.basename(m)
        if not os.path.exists(f):
            copyfile(m, f)
            print('copied: %s' % f)


def rot_mosaic(filepath, file_pattern='IID201609*.jpg', k=1, replace=True):
    """
    may be a handy little tool that rotates soil core mosaic in "final" folder
    the program searches all the stitched photos in a dir (including sub-dir)
    and roates them
    
    Parameters:
    ----
    filepath: str
        the folder where stitched photos are
    file_pattern: str
        stitched photos should have the same name as soil cores
    k: int
        Number of times the array is rotated counter-clockwise 90 degrees
    replace: bool
        replace existing mosaic or not. if not, append "rot" ot filename
    """ 
        
    mosaics = []
    for root, dirnames, filenames in os.walk(filepath):
        for filename in fnmatch.filter(filenames, file_pattern):
            mosaics.append(os.path.join(root, filename))
            
    for m in mosaics:
        img = improc.imops.imio.imread(m)
        img = np.rot90(img, k=k)
        
        if replace:
            improc.imops.imio.imsave(m, img)
            print('generated: %s' % m)
        else:
            r = m.replace('.jpg', '_rot.jpg')
            improc.imops.imio.imsave(r, img)
            print('generated: %s' % r)    
