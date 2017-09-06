meme foo
requires test

class Fail < Exception
end

class Fail2 < Fail
end

main: fun() {
  try {
    try {
      Exception.throw("msg1");
    } catch(Fail e) {
      test.assert(false, "uncaught");
    }
  } catch(e) {
    test.assert(true, "catch all 1");
  }

  try {
    try {
      Fail2.throw("msg2");
    } catch(Fail e) {
      test.assert(false, "uncaught");
    }
  } catch(e) {
    test.assert(true, "catch all 2");
  }

  try {
    Fail.throw("msg3");
  } catch(Fail e) {
    test.assert(true, "Caught 1");
  }

  try {
    Fail.throw("msg4");
  } catch(Exception e) {
    test.assert(true, "Caught 2");
  }

  try {
    Fail.throw("msg5");
  } catch(Exception e) {
    //
  }
}
