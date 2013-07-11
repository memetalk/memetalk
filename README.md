Memetalk
========

This is a repository for the Memetalk programming system.

A rationale for this project may be found at the [wiki][1]

[1]: https://github.com/thiago-silva/memetalk/wiki

Installing & Building
--------------------

1. Make sure you have a working version of python (I'm using 2.7.5).
2. Install PyQt. If you use brew, you can use `brew install pyqt`.
3. Install greenlet python library.
3. Install python's qscintilla2 bindings. In debian-like system, `aptitude install python-qscintilla2`
4. Clone `pymeta` (https://github.com/thiago-silva/pymeta.git), and install it. You may need to run `sudo easy_install .` in the `pymeta` dir after building.
5. Run `python src/grammars/gen.py` to generate the parsers.
6. Run `python gen_core.py` to generate the core module.
7. Success!

Running
-------
For now, the entry-point for memetalk's interpreter is `i.py`. From the `memetalk` dir, run `python src/i.py {{source.mm}}`. For example:

```
python src/i.py tests/closures1.mm
```

The main (super alpha) programming environment can be executed with:

```
python i.py modules/ide-z-entry.mm
```
