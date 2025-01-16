# frequent-substring

## Overview

`subs` is a tool for identifying frequent substrings in a text file, and substituting them for something else.
It is implemented in Haskell.

With the `-l` option, it can find the most frequent subsequence of a specified length.
With the `-r` option, it can find the longest subsequence that repeats at least the specified number of times.
With the `-s` option, in concert with the others, it can substitute the specified string for the subsequence found.

It works by hashing the subsequences that it's counting, to reduce memory consumption.
The motivating use-case was to aid in simplifying long mathematical expressions, too long for ordinary algebra systems (e.g. sage, mathematica) to simplify without exhausting memory.
It has successfully found a longest-substring on a >2.5M character sequence, which led to memory exhaustion on those ordinary algebra systems (on a system with 11Gi of ram).

## Help Doc

```
$ subs --help
frequent-substring help:

Usage: subs [-i|--filepath FilePath] [-l|--length INT] [-r|--repeats INT] 
            [-s|--substitute String] [-v|--verbose]

  find frequent subsequences in a text file

Available options:
  -i,--filepath FilePath   path to input file containing the text sequence
  -l,--length INT          find the most frequent subsequence of a specified
                           length
  -r,--repeats INT         find the longest subsequence which repeats this many
                           times
  -s,--substitute String   replace the subsequence found with a specified string
  -v,--verbose             whether to display counts, etc.
  -h,--help                Show this help text
```

## Typical Workflow

The longest-subsequence function (`-r`) uses a binary search.
A typical workflow on a long file might be to run with `-l` (and `-v`) a few times,
to get an idea of what number of repeats to specify.

```
$ subs -i res/formula -l 100 -v
Finding the most frequent subsequence of length 100 in the content of res/formula
content has length: 2500530
number of subsequences of exact length 100: 2500431
substring:
*d^2*e^2*g*h - 24*e^4*g*h + 192*c^2*g^3*h - 12*d^2*g^3*h - 60*e^2*g^3*h + 12*g^5*h - 24*c^4*h^2 - 72
length: 100
repeats: 189
```

Then use `-r` with that number of repeats, to find the longest substring that repeats at least that many times:

```
$ subs -i res/formula -r 189 > substring-that-repeats-at-least-189-times.txt
```

or, obtain the original formula but with a specified substitution applied:

```
$ subs -i res/formula -r 189 -s substituteTerm > original-with-substitution-applied.txt
```

## Examples

### Searching by length:

```
$ subs -i res/formula -l 17
*sqrt(e^2 - h^2)*
```

with verbose:

```
$ subs -i res/formula -l 17 -v
Finding the most frequent subsequence of length 17 in the content of res/formula
content has length: 13209
number of subsequences of exact length 17: 13193
substring:
*sqrt(e^2 - h^2)*
length: 17
repeats: 190
```

### Performing a substitution when searching by length:

```
$ subs -i res/formula -l 17 -s "*eMinusH*"
(16*c^6 + 24*c^4*d^2 + 12*c^2*d^4 + 2*d^6 - 24*c^4*e^2 + 24*c^2*d^2*e^2 + 18*d^4*e^2 + 12*c^2*e^4 - 18*d^2*e^4 - 2*e^6 - 84*c^4*g^2 - 30*c^2*d^2*g^2 + 6*d^4*g^2 + 66*c^2*e^2*g^2 - 42*d^2*e^2*g^2 - 12*e^4*g^2 + 66*c^2*g^4 + 6*d^2*g^4 + 12*e^2*g^4 + 2*g^6 - 96*c^4*g*h - 96*c^2*d^2*g*h - 24*d^4*g*h + 96*c^2*e^2*g*h - 48*d^2*e^2*g*h - 24*e^4*g*h + 192*c^2*g^3*h - 12*d^2*g^3*h - 60*e^2*g^3*h + 12*g^5*h - 24*c^4*h^2 - 72*c^2*d^2*h^2 - 30*d^4*h^2 + 24*c^2*e^2*h^2 + 12*d^2*e^2*h^2 - 6*e^4*h^2 + 222*c^2*g^2*h^2 + 132*d^2*g^2*h^2 - 102*e^2*g^2*h^2 - 54*g^4*h^2 + 96*c^2*g*h^3 + 144*d^2*g*h^3 - 48*e^2*g*h^3 - 116*g^3*h^3 + 12*c^2*h^4 + 30*d^2*h^4 - 6*e^2*h^4 - 90*g^2*h^4 - 24*g*h^5 - 2*h^6 + 48*eMinusH*c^4*d + 48*eMinusH*c^2*d^3 + 12*eMinusH*d^5 - 156*eMinusH*c^2*d*e^2 + 120*eMinusH*d^3*e^2 +
...
```

### Searching by number of repeats:

```
$ subs -i res/formula -r 150
*sqrt(e^2 - h^2)*
```

with verbose:

```
$ subs -i res/formula -r 150 -v
Finding the longest subsequence that repeats 150 times, in the content of res/formula
content has length: 13209
number of subsequences with length at least 0: 87258655
substring:
*sqrt(e^2 - h^2)*
length: 17
repeats: 190
```

### Performing a substitution when searching by number of repeats:

```
$ subs -i res/formula -r 150 -s "*eMinusH*"
(16*c^6 + 24*c^4*d^2 + 12*c^2*d^4 + 2*d^6 - 24*c^4*e^2 + 24*c^2*d^2*e^2 + 18*d^4*e^2 + 12*c^2*e^4 - 18*d^2*e^4 - 2*e^6 - 84*c^4*g^2 - 30*c^2*d^2*g^2 + 6*d^4*g^2 + 66*c^2*e^2*g^2 - 42*d^2*e^2*g^2 - 12*e^4*g^2 + 66*c^2*g^4 + 6*d^2*g^4 + 12*e^2*g^4 + 2*g^6 - 96*c^4*g*h - 96*c^2*d^2*g*h - 24*d^4*g*h + 96*c^2*e^2*g*h - 48*d^2*e^2*g*h - 24*e^4*g*h + 192*c^2*g^3*h - 12*d^2*g^3*h - 60*e^2*g^3*h + 12*g^5*h - 24*c^4*h^2 - 72*c^2*d^2*h^2 - 30*d^4*h^2 + 24*c^2*e^2*h^2 + 12*d^2*e^2*h^2 - 6*e^4*h^2 + 222*c^2*g^2*h^2 + 132*d^2*g^2*h^2 - 102*e^2*g^2*h^2 - 54*g^4*h^2 + 96*c^2*g*h^3 + 144*d^2*g*h^3 - 48*e^2*g*h^3 - 116*g^3*h^3 + 12*c^2*h^4 + 30*d^2*h^4 - 6*e^2*h^4 - 90*g^2*h^4 - 24*g*h^5 - 2*h^6 + 48*eMinusH*c^4*d + 48*eMinusH*c^2*d^3 + 12*eMinusH*d^5 - 156*eMinusH*c^2*d*e^2 + 120*eMinusH*d^3*e^2 +
```

### Reading from standard input:

```
$ cat res/formula | subs -l 17
*sqrt(e^2 - h^2)*
```

```
$ cat res/formula | subs -r 150
*sqrt(e^2 - h^2)*
```




