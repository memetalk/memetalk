class Flag(object):
    def __init__(self):
        self.x = None
    def __call__(self, *args):
        if len(args) == 0:
            return self.x
        else:
            self.x = args[0]
