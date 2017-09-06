# Memetalk

This is a repository for the Memetalk programming system.

A rationale for this project may be found at the [wiki][1]

[1]: https://github.com/thiago-silva/memetalk/wiki

Current stage of development is **alpha**.

## List of dependencies

### C++

The Memetalk VM is currently developed in C++ and depends on:

* Boost (1.58.0.1)
* [Boehm GC (gc-7.6.0)](http://www.hboehm.info/gc/)

Memetalk also has unfortunate dependencies at the moment, such as:

 * Qt 4
 * QScintila2

These should be installed to be able to build the VM.

### Python

While MemeScript compiler is written in itself, we are currently bootstrapping
using a compiler written in python. Thus, to build, you will also need to have
the following installed:

 * Python (2.7.5)
 * clime (0.3.1)
 * pymeta (from https://github.com/thiago-silva/pymeta.git)

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
$ ./meme central/stdlib/hello_world.me
   ```
