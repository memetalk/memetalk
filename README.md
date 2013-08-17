# Memetalk

This is a repository for the Memetalk programming system.

A rationale for this project may be found at the [wiki][1]

[1]: https://github.com/thiago-silva/memetalk/wiki

## List of dependencies
memetalk is currently developed in python, so it's recommended for you to create a
[virtualenv](http://docs.python-guide.org/en/latest/dev/virtualenvs.html#virtualenvwrapper)
to maintain your python dependencies.

 * Python (I'm using 2.7.5)
 * Python Greenlets
 * Python jinja2
 * QT (with python bindings)
 * QScintila2 (with python bindings)

### Debian-like systems
The line below will install all the dependencies recursively

* `# aptitude install python-qscintilla2 python-greenlet python-jinja2`

### Mac OS X
* `$ brew install python pyqt qscintilla2`
* `pip install greenlet`
* `pip install jinja2`

### All systems
 * Clone `pymeta` from https://github.com/thiago-silva/pymeta.git
 * Run `python setup.py install` in the `pymeta` dir after cloning

## Installing & Building
Inside of the `memetal/src` directory, run the following scripts:

1. Run `python grammars/gen.py` to generate the parsers.
2. Run `python gen_core.py` to generate the core module.
3. Success!

## Running
For now, the entry-point for memetalk's interpreter is `i.py`. From the `memetalk` dir, run `python src/i.py {{source.mm}}`. For example:

```
$ python src/i.py src/modules/memetest.mm
```

or preferably:

```
$ python src/i.py memetest.mm
```

as the module path is set to `src/modules` already.

Finally, the main (super alpha) programming environment can be executed with:

```
$ python src/i.py idez.mm
```
