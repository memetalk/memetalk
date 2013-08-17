.license
Copyright (c) 2012-2013 Thiago B. L. Silva <thiago@metareload.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
.endlicense

module memetest(io)
  io : memetalk/io/1.0();
{
  _test_files: fun() {
    <primitive "test_files">
  }

  _test_import: fun(filepath) {
    <primitive "test_import">
  }

  assert: fun(x,desc) {
    if (!x) {
      if (desc) {
        Exception.throw("assertion failed: '" + desc + "'");
      } else {
        Exception.throw("assertion failed");
      }
    }
  }

  main: fun() {
    _test_files().each(fun(path) {
      io.print("test:loading " + path);
      var m = _test_import(path);
      io.print("test:executing " + path);
      try {
        m.main();
      } catch(e) {
        io.print(e.value + " on " + path);
        Exception.throw("test:interrupted");
      }
    });
    return "ok";
  }

}
