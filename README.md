# Memetalk [![Build Status](https://secure.travis-ci.org/memetalk/memetalk.png?branch=master)](https://travis-ci.org/memetalk/memetalk)

This is a repository for the Memetalk programming system.

A rationale for this project may be found at the [wiki][1]

[1]: https://github.com/thiago-silva/memetalk/wiki

Current stage of development is **alpha**.

## List of dependencies

### C++

The Memetalk VM is currently developed in C++ and depends on:

* Boost (1.58.0.1)
* Google's [re2 (v. 2017-08-01)](https://github.com/google/re2)
* [Boehm GC (gc-7.6.0)](http://www.hboehm.info/gc/)

## Building

Run inside the repository directory:

```
$ make get_core
$ make
```

The VM binary `meme` should have been created in the current directory.

To be sure everything is minimally O.K., we also recommend running the tests:

```
$ make test
```

## Running

Copy meme.config.sample to ~/.meme.config. Edit the paths for cache directory
appropriately. Then, just run:

```
$ ./meme central/examples/hello_world.me
```
