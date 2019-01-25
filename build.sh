#!/bin/bash
time (fsharpc -a bitmap.fs && fsharpc -r bitmap.dll img.fs && mono img.exe)
