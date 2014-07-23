from pdb import set_trace as br
import math
from pyutils import bits


class Cell(object):
    def index(self):
        return self.etable.offset + self.etable.entries.index(self)

class NullCell(Cell):
    def __init__(self, etable):
        self.etable = etable

    def __len__(self):
        return bits.WSIZE

    def __call__(self, *args):
        return bits.bytelist(0)

class IntCell(Cell):
    def __init__(self, etable, num):
        self.etable = etable
        self.num = num

    def __len__(self):
        return bits.WSIZE

    def __call__(self, *args):
        return bits.bytelist(self.num)

class TaggedIntCell(Cell):
    def __init__(self, etable, num):
        self.etable = etable
        self.num = num

    def __len__(self):
        return bits.WSIZE

    def __call__(self, *args):
        # tag int
        if self.num  < 0x80000000:
            return bits.bytelist_tag(self.num)
        else:
            raise Exception('Unsupported big num')

class StringCell(Cell):
    def __init__(self, etable, string):
        self.etable = etable

        string_with_term = string + "\0"
        # words_needed = int(math.ceil(len(string_with_term) / float(utils.WSIZE)))
        # to_fill = (words_needed * utils.WSIZE) - len(string_with_term)
        to_fill = bits.string_block_size(string_with_term) - len(string_with_term)
        # br()
        self.data = map(ord, string_with_term) + ([0] * to_fill)

    def __len__(self):
        return len(self.data)

    def __call__(self, *args):
        return self.data

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

    def __len__(self):
        return bits.WSIZE

    def __call__(self, offset=0):
        if self.target_cell:
            to = self.etable.cells.index(self.target_cell)
            return bits.bytelist(self.etable.base + sum(self.etable.cell_sizes[0:to]))
        else:
            return bits.bytelist(self.etable.base + self.etable.index[self.target_label])

class VirtualMemory(object):
    def __init__(self):
        self.cells = []
        self.cell_sizes = []
        self.index = {} # label -> memory pos
        self.base = 0
        self.cell_index = {} # label -> cells index

    def _append_cell(self, cell, label):
        if label is not None:
            self.label_current(label)
        self.cells.append(cell)
        self.cell_sizes.append(len(cell))

    def set_base(self, base):
        self.base = base

    def label_current(self, label):
        if label in self.index:
            raise Exception('Duplicated label ' + label)
        self.index[label] = sum(self.cell_sizes)
        self.cell_index[label] = len(self.cells)

    def get_pointer_to(self, label):
        return self.cells[self.cell_index[label]]

    def append_int(self, num, label=None):
        cell = IntCell(self, num)
        self._append_cell(cell, label)
        return cell

    def append_tagged_int(self, num, label=None):
        cell = TaggedIntCell(self, num)
        self._append_cell(cell, label)
        return cell

    def append_null(self, label=None):
        cell = NullCell(self)
        self._append_cell(cell, label)
        return cell

    def append_string(self, string, label=None):
        cell = StringCell(self, string)
        self._append_cell(cell, label)
        return cell

    def append_pointer_to(self, cell, label=None):
        cell = PointerCell(self, cell=cell)
        self._append_cell(cell, label)
        return cell

    def append_label_ref(self, target_label, label=None):
        cell = PointerCell(self, label=target_label)
        self._append_cell(cell, label)
        return cell

    ###

    def dump(self):
        address_offset = 4 # each value dumped is a 32 bit pack (ie. 4 1byte value, 4 addresses within)
        for idx, x in enumerate(bits.chunks(reduce(lambda x,y: x+y, [e() for e in self.cells]),4)):
            print '{} - {}'.format(self.base + (idx * address_offset), bits.atoword(x))

        # print '--'
        # print 'Object:', self.index_for('Object')
        # print 'Foo:', self.index_for('Foo')
        # print self.addr_table()

if __name__ == '__main__':
    tb = VirtualMemory()
    c0 = tb.append_label_ref('Behavior', 'Behavior')   # 100 -> 100
    c1 = tb.append_int(14, 'Object')                   # 104 = 14
    c2 = tb.append_int(18)                             # 108 = 18
    c3 = tb.append_label_ref('Object')                 # 112 -> 104
    c4 = tb.append_int(16)                             # 116 = 16
    tb.append_pointer_to(c3)                           # 120 -> 112
    tb.append_label_ref('Foo')                         # 124 -> 144 -- forward reference

    tb.append_string("abceghijkl")                     # 128-136
    tb.append_label_ref('Foo')                         # 140 -> 144 -- forward reference
    c9 = tb.append_int(21, 'Foo')                      # 144 = 21
    tb.append_pointer_to(c9)                           # 148 -> 144

    tb.set_base(100)
    tb.dump()
