# Memetalk

This is a repository for the Memetalk programming system.

A rationale for this project may be found at the [wiki][1]

[1]: https://github.com/thiago-silva/memetalk/wiki

## List of dependencies

Memetalk is currently developed in python, so it's recommended for you to
create a [virtualenv][2] to maintain your python dependencies.

 * Python (I'm using 2.7.5)
 * jinja2
 * Qt (with python bindings)
 * QScintila2 (with python bindings)
 * dshared library (and its dependencies)

[2]: http://docs.python-guide.org/en/latest/dev/virtualenvs.html#virtualenvwrapper

### Debian-like systems
The line below will install apt-available dependencies recursively:

* `# aptitude install python-qscintilla2 python-jinja2 libboost-all-dev python-dev`

### Mac OS X
* `$ brew install python pyqt qscintilla2`
* `$ pip install jinja2`

### All systems
 * Clone `pymeta` from https://github.com/thiago-silva/pymeta.git
 * `# python setup.py install` in the `pymeta` dir after cloning
 * Clone `dshared` from https://github.com/thiago-silva/dshared.git
 * `# python setup.py install` in the `dshared` dir after cloning

## Installing & Building
Inside of the `memetal/src` directory, run the following scripts:

1. Run `python grammars/gen.py` to generate the parsers.
2. Run `python gen_core.py` to generate the core module.
3. Success!

## Running
For now, the entry-point for memetalk's interpreter is `i.py`. From the
`memetalk` dir, run `python src/i.py {{source.mm}}`. For example:

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
