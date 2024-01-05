# Invistra: a portable and extensible Common Lisp FORMAT implementation

The invistra system provides a portable implementation of Common Lisp
FORMAT. It was initially part of [SICL][] but has been extracted and
the missing required features completed.

# Requirements

[ABCL][], [Clasp][], [ECL][], and [SBCL][] is the only current
implementations that this system has been tested on. 

In addition to a clone of this repository in a location that is
discoverable by ASDF you will also need a clone of
[nontrivial-gray-streams][], [Incless][] and [Inravina][].

# Usage

The core functionality is in the `invistra` package, but the Common
Lisp-like interface of `format` and `formmatter` is in the
`invistra-extrinsic` package and system. To call format try
the following in SBCL:

```
* (asdf:load-system :invistra-extrinsic)
T
* (invistra-extrinsic:format t "Wibble: ~a~%"
                             :quux)
Wibble: QUUX
NIL
```

Invistra can also replace an implementation's FORMAT
implementation. This is done with the `invistra-shim` system.

```
* (asdf:load-system :invistra-shim)
T
* (format t "Wibble: ~a~%" :quux)
Wibble: QUUX
NIL
```

[ABCL]: https://armedbear.common-lisp.dev/
[CLASP]: https://github.com/clasp-developers/clasp
[ECL]: https://ecl.common-lisp.dev/
[Incless]: https://github.com/s-expressionists/Incless
[Inravina]: https://github.com/s-expressionists/Inravina
[SBCL]: http://sbcl.org
[SICL]: https://github.com/robert-strandh/SICL
[nontrivial-gray-streams]: https://github.com/yitzchak/nontrivial-gray-streams
