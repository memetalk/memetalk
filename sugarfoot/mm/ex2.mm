.preamble(ex3)
  ex3: meme:ex3;
  [X] <= ex3;
.code

class Y
init new: fun() {
}
end

main: fun() {
   return ex3.foo()[:c];
}

.endcode
