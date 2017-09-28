from pdb import set_trace as br
import math
from pyutils import bits
import pyutils
import time
from pyutils.parmap import parmap


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

    def is_target_reachable(self):
        # assumes `target_cell` always exists in etable
        return self.target_cell or self.target_label in self.etable.index

    def __call__(self, offset=0):
        if self.target_cell:
            return bits.bytelist(self.etable.base + self.etable.opt_physical_addresses[self.target_cell])
        else:
            return bits.bytelist(self.etable.base + self.etable.index[self.target_label])

class VirtualMemory(object):
    def __init__(self):
        self.cells = []
        self.index = {} # label -> memory pos
        self.base = 0
        self.cell_index = {} # label -> cells index
        self.symb_references = []

        # opt
        self.total_size = 0
        self.opt_cell_indexes = {}
        self.opt_ptr_cells = []
        self.opt_physical_addresses = {}
        self.opt_object_table = []
        self.opt_pending_ptr = {}

    def _append_cell(self, cell, label):
        if label is not None:
            self.label_current(label)
        self.cells.append(cell)

        # opt
        self.opt_physical_addresses[cell] = self.total_size
        self.total_size += len(cell)
        self.opt_cell_indexes[cell] = len(self.cells) -1 # subst self.cells.index(cell)
        if type(cell) == PointerCell:
            self.opt_ptr_cells.append(cell)
            if cell.is_target_reachable():
                self.opt_object_table += cell()
            else:
                if cell.target_label not in self.opt_pending_ptr:
                    self.opt_pending_ptr[cell.target_label] = []
                self.opt_pending_ptr[cell.target_label].append((cell, len(self.opt_object_table)))
                self.opt_object_table += bits.bytelist(0xDDDDDDDD)
        else:
            self.opt_object_table += cell()

    def set_base(self, base):
        self.base = base

    def label_current(self, label):
        if label in self.index:
            raise Exception('Duplicated label ' + label)
        self.index[label] = self.total_size
        self.cell_index[label] = len(self.cells)
        if label in self.opt_pending_ptr:
            for cell, ot_addr in self.opt_pending_ptr[label]:
                new_val = cell()
                self.opt_object_table[ot_addr:ot_addr+len(new_val)] = new_val
            del self.opt_pending_ptr[label]

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

    def append_pointer_to(self, target_cell, label=None):
        cell = PointerCell(self, cell=target_cell)
        self._append_cell(cell, label)
        if target_cell in self.symb_table.keys():
            #self.symb_references.append((self.symb_table[target_cell], cell))
            self.symb_references.append((self.symb_table[target_cell], self.physical_address(cell)))
        return cell

    def append_label_ref(self, target_label, label=None):
        cell = PointerCell(self, label=target_label)
        self._append_cell(cell, label)
        return cell


    ###
    def physical_address(self, cell):
        return self.base + self.opt_physical_addresses[cell]

    def object_table(self):
        # return reduce(lambda x,y: x+y, [e() for e in self.cells])
        return self.opt_object_table
        # # b = time.time()
        # # print 'begin obj table' #, self.total_size)

        # # res = reduce(lambda x,y: x+y, [e() for e in self.cells])
        # res = [None] * self.total_size
        # i = 0
        # for e in self.cells:
        #     sub = e()
        #     for v in sub:
        #         res[i] = v
        #         i += 1
        # # print 'end obj table', time.time()-b
        # return res

    def reloc_table(self):
        # print 'reloc table xx'
        # b = time.time()
        res = [self.physical_address(entry) for entry in self.opt_ptr_cells]
        # print 'end reloc table', time.time()-b
        return res

    def symbols_references(self):
        return self.symb_references
        # # assert(sorted([(x[0], self.physical_address(x[1])) for x in self.symb_references]) == sorted(sr))

        # sr = []
        # # print 'symb references'
        # # b = time.time()
        # for ptr, text in self.symb_table.items():
        #     for referer in [x for x in self.opt_ptr_cells if x.target_cell == ptr]:
        #         sr.append((text, self.physical_address(referer)))
        # # print 'end symb references', time.time()-b
        # assert sorted(self.symb_references) == sorted(sr)
        # return sr

    def append_int_to_int_dict(self, pydict):
        pairs_oop = []
        for key, val in iter(sorted(pydict.items())):
            pairs_oop.append((key, val))
        return self.append_dict_with_pairs(pairs_oop)

    def append_int_to_int_list(self, pydict):
        pairs_oop = []
        for key, val in iter(sorted(pydict.items())):
            val_oop = self.append_list_of_ints(val)
            pairs_oop.append((key, val_oop))
        return self.append_dict_with_pairs(pairs_oop)


    def append_symbol_to_int_dict(self, pydict):
        pairs_oop = []
        for key, val in iter(sorted(pydict.items())):
            key_oop = self.append_symbol_instance(key)
            pairs_oop.append((key_oop, val))
        return self.append_dict_with_pairs(pairs_oop)

    def append_symbol_dict(self, pydict):
        pairs_oop = []
        for key, val in iter(sorted(pydict.items())):
            key_oop = self.append_symbol_instance(key)
            val_oop = self.append_string_instance(val)
            pairs_oop.append((key_oop, val_oop))
        return self.append_dict_with_pairs(pairs_oop)

    def append_dict_with_pairs(self, pairs):
        frame_oop = self._append_dict_frame(pairs)
        oop = self.append_dict_prologue(len(pairs), frame_oop)
        return oop

    def _append_dict_frame(self, pairs):
        self.append_int(pyutils.FRAME_TYPE_ELEMENTS)
        self.append_int(len(pairs) * 2 * bits.WSIZE)

        oops = []
        for key, val in pairs:
            if isinstance(key, (int, long)):
                oops.append(self.append_tagged_int(key))
            else:
                oops.append(self.append_pointer_to(key))

            if isinstance(val, (int, long)):
                self.append_tagged_int(val)
            else:
                self.append_pointer_to(val)

        if len(oops) > 0:
            return oops[0]
        else:
            return None

    def append_empty_dict(self):
        return self.append_dict_prologue(0, None)

    # def append_dict_emiting_entries(self, entries_pydict):
    #     pairs_oop = []
    #     for key, entry, in iter(sorted(entries_pydict.items())):
    #         key_oop = self.append_string_instance(key)
    #         val_oop = entry.fill(self)
    #         pairs_oop.append((key_oop, val_oop))
    #     return self.append_dict_with_pairs(pairs_oop)

    def append_sym_dict_emiting_entries(self, entries_pydict):
        pairs_oop = []
        for key, entry, in iter(sorted(entries_pydict.items())):
            key_oop = self.append_symbol_instance(key)
            val_oop = entry.fill(self)
            pairs_oop.append((key_oop, val_oop))
        return self.append_dict_with_pairs(pairs_oop)
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
