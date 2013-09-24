.license
.endlicense

.preamble()

.code

class Fail < Exception
end

class Fail2 < Fail
end

main: fun() {
  try {
    try {
      Exception.throw("msg1");
    } catch(Fail e) {
      assert(false, "uncaught");
    }
  } catch(e) {
    assert(true, "catch all 1");
  }

  try {
    try {
      Fail2.throw("msg2");
    } catch(Fail e) {
      assert(false, "uncaught");
    }
  } catch(e) {
    assert(true, "catch all 2");
  }

  try {
    Fail.throw("msg3");
  } catch(Fail e) {
    assert(true, "Caught 1");
  }

  try {
    Fail.throw("msg4");
  } catch(Exception e) {
    assert(true, "Caught 2");
  }

  try {
    Fail.throw("msg4");
  } catch(Exception e) {
    //
  }
}

.end
