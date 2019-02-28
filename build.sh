#!/bin/bash
if [ "$1" == "-a" ]
then
  time (fsharpc -a bitmap.fs && fsharpc -r bitmap.dll img.fs && mono img.exe)
else
  time (fsharpc -r bitmap.dll img.fs && mono img.exe)
fi

