#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# euler: Numeric conversions, number series, factorization, primes, coprimes
# Copyright: (c) 2013, Jared Suttles. All rights reserved.
# License: BSD, see LICENSE for details.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
from math import log10, log, factorial
from operator import mul
from fractions import gcd
from itertools import imap
from pyprimesieve import *


# General Math & Numeric Conversions
#-------------------------------------------------------------------------------
def digits(n, reverse=False, base=10, string=False):
    """
    List of the individual digits that comprise the given number. Providing a base will convert the number to that base.
    When string is True, a string is returned where alphabetic characters are used intead of integers (works up to and
    including base 36).
    """
    if not isinstance(base, int):
        raise TypeError('given base {} must be an int'.format(repr(base)))
    if base < 2:
        raise ValueError('base must be >= 2')
    if string and base > 36:
        raise ValueError('using alphabetic character replacements for digits requires a base of <= 36')
    if base != 10:
        ds = []
        while n >= base:
            n, r = divmod(n, base)
            ds.insert(0, r)
        if n > 0:
            ds.insert(0, n)
        if string:
            ds = ''.join([chr(d+87) if d > 9 else str(d) for d in ds])
        return ds[::-1] if reverse else ds
    return [int(d) for d in (str(n)[::-1] if reverse else str(n))]


def number(l):
    """
    Convert the given iterable of digits into a number composed of the digits in order.
    """
    return sum(d*10**i for i, d in enumerate(reversed(list(l))))


def number_digits(n):
    """
    Number of digits.
    """
    return n == 0 and 1 or int(log10(n))+1


def divisors(n):
    """
    Returns list of divisors of the given number.
    """
    div = [1]
    for p, r in factorize(n):
        div = [d * p**e for d in div for e in xrange(r+1)]
    return div


def number_divisors(n):
    """
    Number of divisors.
    """
    if n < 1:
        return 0
    if n == 1:
        return 1
    return reduce(mul, (e+1 for _, e in factorize(n)))


def is_palindrome(n):
    n = str(n)
    return n == n[::-1]


def lcm(*args):
    def lcm_(x, y):
        return x*y/gcd(x, y)
    return reduce(lcm_, args)


def pandigital(iterable):
    ds = {1, 2, 3, 4, 5, 6, 7, 8, 9}
    for ele in iterable:
        if ele in ds:
            ds.remove(ele)
        else:
            return False
    return not ds


# Number Series
#-------------------------------------------------------------------------------
def facs():
    """
    Factorial sequence iterator.
    """
    a, b = 1, 1
    while True:
        yield a
        a, b = a*b, b+1


def fibs(limit=None, step=2):
    """
    Fibonacci sequence iterator. Accepts step argument to specify the n-step Fibonacci sequence.
    """
    x = [0]*(step-1) + [1]
    while x[0] < limit or limit is None:
        yield x[0]
        x = x[1:] + [sum(x)]
fibonacci = fibs
tribonacci = fibs(step=3)
tetranacci = fibs(step=4)
pentanacci = fibs(step=5)
hexanacci = fibs(step=6)
heptanacci = fibs(step=7)


# Combinatorics
#-------------------------------------------------------------------------------
def permute(n, k):
    """
    The number of ways to permute n for k sized groups.
               n!
    n_P_k = --------
            (n - k)!
    """
    if k > n:
        return 0
    return factorial(n) / factorial(n-k)


def choose(n, k):
    """
    The number of k-sized combinations of n.
                n!
    n_C_k = ----------
            k!(n - k)!
    """
    if k > n:
        return 0
    return factorial(n) / (factorial(k) * factorial(n-k))


# Prime Numbers
#-------------------------------------------------------------------------------
def is_prime(n):
    """
    Primality test using modulo arithmetic.
    """
    if n == 2:
        return True
    if n < 2 or not n & 1:
        return False
    for i in xrange(3, int(n**0.5)+1, 2):
        if n % i == 0:
            return False
    return True


def pi(n):
    """
    Prime number theorem. Approximates.
    How many prime numbers are below the given number n?
    """
    return int(n/(log(n)-1))


def size_sieve_for_n_primes(n):
    """
    Prime number theorem. Approximates.
    What is the number where there are n prime numbers below it?
    """
    return int(n*log(n*log(n)))


# Relatively Prime
#-------------------------------------------------------------------------------
def coprimes(n):
    """
    List of coprimes below the given number.
    """
    return [m for m in xrange(n) if gcd(n, m) == 1]


def phi(n):
    """
    Euler's totient function.
    http://en.wikipedia.org/wiki/Euler's_totient_function
    """
    return int(reduce(mul, imap(lambda x: 1 - 1.0/x[0], factorize(n)), n))


def phis(n):
    """
    List of phi values for numbers below n.
    http://stackoverflow.com/questions/1019040/how-many-numbers-below-n-are-coprimes-to-n/1019389#1019389

      >>> import timeit
      >>> min(timeit.Timer('[phi(n) for n in xrange(10**3)]', setup='from demdigitsigits import phi').repeat(7, 1000))
      8.282382011413574
      >>> min(timeit.Timer('phis(10**3)', setup='from demdigits import phis').repeat(7, 1000))
      1.1944000720977783
    """
    phi_values = [0] + [1]*(n-1)
    for i in xrange(2, n):
        if phi_values[i] != 1:
            continue
        for j in xrange(i, n, i):
            phi_values[j] *= i - 1
            k = j / i
            while k % i == 0:
                phi_values[j] *= i
                k /= i
    return phi_values
