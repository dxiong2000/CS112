#! /usr/bin/env python
import os
from subprocess import call

for f in os.listdir("."):
    if (os.path.isdir(f)):
        print f
        call(["../prog3.py",f],stdout=open(f+"/grade.txt","w+"))
