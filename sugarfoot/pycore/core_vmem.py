from pyutils import vmemory


class CoreVirtualMemory(vmemory.VirtualMemory):
    def index_for(self, name):
        return self.base + self.index[name]

    def addr_table(self):
        return [self.base + sum(self.cell_sizes[0:idx]) for idx,entry in enumerate(self.cells) if type(entry) == vmemory.PointerCell]

    def object_table(self):
        return reduce(lambda x,y: x+y, [e() for e in self.cells])
