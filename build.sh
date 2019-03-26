#!/bin/bash
if [ "$1" == "-a" ]
then
  time (fsharpc -a bitmap.fs -a img.fs && fsharpc -r bitmap.dll -r img.dll main.fs && mono main.exe)
else
  time (fsharpc -r bitmap.dll -r img.dll main.fs && mono main.exe)
fi
