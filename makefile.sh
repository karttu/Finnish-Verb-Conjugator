#!/bin/sh
gcc -c -o wildcard.o wildcard.cpp
gcc -c -o conjtest.o conjtest.cpp
gcc -c -o conjugat.o conjugat.cpp
gcc -o conjugat.cgi conjugat.o conjtest.o wildcard.o
