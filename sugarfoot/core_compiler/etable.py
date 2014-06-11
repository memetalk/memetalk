class Cell(object):
    def index(self):
        return self.etable.base + self.etable.entries.index(self)

class IntCell(Cell):
    def __init__(self, etable, num):
        self.etable = etable
        self.num = num

    def __call__(self):
        return self.num

class StringCell(Cell):
    def __init__(self, etable, string):
        self.etable = etable
        self.string = string

    def __call__(self):
        chars = [c for c in self.string] + [0] # \0
        chars += (len(chars) % 8) * [0]        # padding
        return chars

class PointerCell(Cell):
    def __init__(self, etable, label=None, cell=None):
        self.etable = etable
        self.target_label = None
        self.target_cell = None

        if label:
            self.target_label = label
        if cell:
            self.target_cell = cell

        if label is None and cell is None:
            raise Exception('label or cell required')

    def __call__(self):
        if self.target_cell:
            return self.etable.base + self.etable.entries.index(self.target_cell)
        else:
            return self.etable.base + self.etable.index[self.target_label]

class VirtualEntryTable(object):
    def __init__(self):
        self.entries = []
        self.base = 0
        self.index = {}

    def append_int(self, num, label=None):
        cell = IntCell(self, num)
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    def append_string(self, string, label=None):
        cell = StringCell(self, string)
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    def append_pointer_to(self, cell, label=None):
        cell = PointerCell(self, cell=cell)
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    def append_label_ref(self, target_label, label=None):
        # if target_label == label:
        #     cell = PointerCell(self, label)
        #     cell = self.circular_cell(label)
        # else:
        #     print target_label
        #     br()
        cell = PointerCell(self, label=target_label)
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    # def circular_cell(self, label):
    #     cell = PointerCell(self, label)
    #     return cell

    def set_base(self, base):
        self.base = base

    def label_current(self, label):
        self.index[label] = len(self.entries)

    ###

    def addr_table(self):
        return [self.base + idx for idx, x in enumerate(self.entries) if type(x) == PointerCell]

    def object_table(self):
        ot = [x() for x in self.entries]
        # TODO: expand variable sized cells (eg. string) updating the references
        #  ie. some items in ot are not 32 bit values, but arrays of variable length.
        #      they should be flattened with ot and all further references from that point should be updated.
        #      for value in ot:
        #        if type(value) == list:
        #            ot = ot[:here] + value + ot[here:]
        #            update_refs(len(value) ot[here:])
        return ot

    def dump(self):
        for idx, x in enumerate(self.entries):
            print "[{}]: {}".format(self.base + idx, x())

if __name__ == '__main__':
    tb = VirtualEntryTable()
    c0 = tb.append_label_ref('Behavior', 'Behavior')
    c1 = tb.append_int(10, 'Object')
    c2 = tb.append_int(20)
    c3 = tb.append_label_ref('Object')
    c4 = tb.append_int(30)
    c5 = tb.append_pointer_to(c3)
    c5 = tb.append_label_ref('Foo') # forward reference
    c7 = tb.append_int(40, 'Foo')
    tb.set_base(100)
    tb.dump()
