Memetalk
========

This is a repository for the Memetalk programming system. 

A rationale for this project may be found at:
http://lists.memetalk.me/pipermail/memetalk-dev/2012-August/000001.html

Installing & Building
--------------------

1. Make sure you have a working version of python.
2. Install PyQt. If you use brew, you can use `brew install pyqt`.
3. Clone `pymeta` (https://github.com/thiago-silva/pymeta.git), and install it. You may need to run `sudo easy_install .` in the `pymeta` dir after building.
4. Run `python src/grammars/gen.py`.
5. Success!

Running
-------
For now, the entry-point for memetalk's interpreter is `i.py`. From the `memetalk` dir, run `python src/i.py {{source.mm}}`. For example:

```
python src/i.py tests/closures1.mm
```
