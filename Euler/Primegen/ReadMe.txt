========================================================================
    DYNAMIC LINK LIBRARY : Primegen Project Overview
========================================================================

This library is base on:
primegen 0.97
19990304
Copyright 1999
D. J. Bernstein, djb@pobox.com
http://pobox.com/~djb/primegen.html


Primegen.vcproj
    primegen is a small, fast library to generate prime numbers in order.
    It generates the 50847534 primes up to 1000000000 in just 8 seconds on a
    Pentium II-350; it prints them in decimal in just 35 seconds.

    primegen can generate primes up to 1000000000000000, although it is not
    optimized for primes past 32 bits. It uses the Sieve of Atkin instead of
    the traditional Sieve of Eratosthenes.

Primegen.cpp
    This is the main DLL source file.

Primegen.h
    This file contains a class declaration.

AssemblyInfo.cpp
    Contains custom attributes for modifying assembly metadata.

/////////////////////////////////////////////////////////////////////////////
Other notes:

AppWizard uses "TODO:" to indicate parts of the source code you
should add to or customize.

/////////////////////////////////////////////////////////////////////////////
