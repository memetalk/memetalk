class Cell(object):
    def index(self):
        return self.etable.base + self.etable.entries.index(self)

class IntCell(Cell):
    def __init__(self, etable, num):
        self.etable = etable
        self.num = num

    def __call__(self, _):
        # tag int
        if self.num  < 0x80000000:
            return self.num | 0x80000000
        else:
            raise Exception('Unsupported big num')

class StringCell(Cell):
    def __init__(self, etable, string):
        self.etable = etable
        self.string = string

    def chunks32(self, ords):
        res = []
        curr = None
        for idx, x in enumerate(ords):
            if idx % 4 == 0:
                curr = []
                res.append(curr)
            curr.append(x)
        return res

    def __call__(self, _):
        chars = list(self.string)
        ords = map(ord, chars) + [0]  # \0
        chunks = self.chunks32(ords)
        data = []
        for chunk in chunks:
            value = 0
            bit_offset = 0
            for c in chunk:
                value += c << bit_offset
                bit_offset += 8
            data.append(value)
        return data

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

    def __call__(self, ptr_offset=0):
        if self.target_cell:
            return ptr_offset + self.etable.base + self.etable.entries.index(self.target_cell)
        else:
            return ptr_offset + self.etable.base + self.etable.index[self.target_label]

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
        # literal strings and arrays are expanded inline,
        # so we need to update upcoming references with the
        # amount of offset (= len(expanded_value))
        ot = []
        offset = 0
        for entry in self.entries:
            value = entry(offset)
            if type(value) == list:
                ot += value
                offset += len(value) - 1 # its already assume value occupies 1; add only extra space needed
            else:
                ot.append(value)
        return ot

    def dump(self):
        for idx, val in enumerate(self.object_table()):
            print "[{}]: {}".format(self.base + idx, val)

if __name__ == '__main__':
    tb = VirtualEntryTable()
    c0 = tb.append_label_ref('Behavior', 'Behavior')
    c1 = tb.append_int(10, 'Object')
    c2 = tb.append_int(20)
    c3 = tb.append_label_ref('Object')
    c4 = tb.append_int(30)
    tb.append_pointer_to(c3)
    tb.append_label_ref('Foo')

    tb.append_string("abcde")
    tb.append_label_ref('Foo') # forward reference
    tb.append_int(40, 'Foo')

    tb.set_base(100)
    tb.dump()
