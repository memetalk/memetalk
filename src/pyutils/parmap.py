from multiprocessing import Process, Pipe
from itertools import izip
import time

## from https://stackoverflow.com/questions/3288595/multiprocessing-how-to-use-pool-map-on-a-function-defined-in-a-class

def spawn(f):
    def fun(pipe,x):
        pipe.send(f(x))
        pipe.close()
    return fun

def parmap(f,X):
    # b = time.time()
    # print 'parmap', b
    pipe=[Pipe() for x in X]
    proc=[Process(target=spawn(f),args=(c,x)) for x,(p,c) in izip(X,pipe)]
    # print 'pipe created', time.time()-b
    [p.start() for p in proc]
    [p.join() for p in proc]
    return [p.recv() for (p,c) in pipe]

# if __name__ == '__main__':
#     print parmap(lambda x:x**x,range(1,5))
