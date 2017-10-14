# Memetalk [![Build Status](https://secure.travis-ci.org/thiago-silva/memetalk.png?branch=master)](https://travis-ci.org/thiago-silva/memetalk)

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

### Memetalk

MemeScript compiler is written in itself, so you need to install a previously
built binary version of it to perform the bootstraping:

* [memetalk (linux x86_64 tarballs)](https://github.com/thiago-silva/memetalk/releases)


## Building

Run inside the repository directory:

```
$ make
```

The VM binary `meme` should have been created in the current directory.

To be sure everything is minimally O.K., we also recommend running the tests:

```
$ make test
```

## Running

Copy meme.config.sample to ~/.meme.config. Edit the paths within
"override_to_local" key so they point to the correct directories. Then, from
the repository directory, just run:

```
$ ./meme central/examples/hello_world.me
```
