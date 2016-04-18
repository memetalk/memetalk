# Memetalk

This is a repository for the Memetalk programming system.

A rationale for this project may be found at the [wiki][1]

[1]: https://github.com/thiago-silva/memetalk/wiki

## List of dependencies

Memetalk's VM is currently developed in C++, with some python needs. To build
and use it, you will need:

 * Python (I'm using 2.7.5)
 * Qt 4
 * QScintila2
 * boost


* `$ brew install python pyqt qscintilla2`

Also, you will need:

 * Clone `pymeta` from https://github.com/thiago-silva/pymeta.git
 * `# python setup.py install` in the `pymeta` dir after cloning

## Building

Just run `make` Inside of the `memetalk/sugarfoot` directory.

## Compiling

Source files are in `sugarfoot/mm` directory, with extension `.mm`. The
compiler is written in python at the moment. To compile source code, run the
following from the `memetalk/sugarfoot` directory:

```
$  python -m pycompiler.compiler mm/hello_world.mm
```

A compiled `.mmc` file should have been generated on `mm` directory.


## Running

From `sugarfoot` directory, run:

```
$  MEME_PATH=./mm/ ./sf-vm hello_world
```
