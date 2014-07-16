import pyutils
from pyutils import bits
from pyutils import vmemory
from pdb import set_trace as br


class CompVirtualMemory(vmemory.VirtualMemory):
    def __init__(self):
        super(CompVirtualMemory, self).__init__()
        self.ext_ref_table = []
        self.symb_table = []
        self.string_table = {}
        self.exception_types = []

    def _physical_address(self, cell):
        return self.base + sum(self.cell_sizes[0:self.cells.index(cell)])

    def object_table(self):
        return reduce(lambda x,y: x+y, [e() for e in self.cells])

    def external_references(self):
        return [(x[0], self._physical_address(x[1])) for x in self.ext_ref_table]

    def external_names(self):
        return sorted(set([x[0] for x in self.ext_ref_table] + [x[0] for x in self.symb_table]))

    def reloc_table(self):
        return [self._physical_address(entry) for entry in self.cells if type(entry) == vmemory.PointerCell]

    def symbols_references(self):
        sr = []
        for text, ptr in self.symb_table:
             for referer in [x for x in self.cells if type(x) == vmemory.PointerCell and x.target_cell == ptr]:
                 sr.append((text, self.base + sum(self.cell_sizes[0:self.cells.index(referer)])))
        return sr

    def exception_table_types(self):
        return [self._physical_address(x) for x in self.exception_types]

    def add_exception_type(self, type_oop):
        self.exception_types.append(type_oop)

    def append_external_ref(self, name, label=None):
        oop = self.append_int(0xAAAA, label)
        self.ext_ref_table.append((name, oop))
        return oop

    def append_object_instance(self):
        self.append_int(pyutils.FRAME_TYPE_OBJECT)
        self.append_int(2 * bits.WSIZE)

        oop = self.append_external_ref('Object')
        self.append_null()             # delegate
        return oop

    def append_symbol_instance(self, string):
        oop = self.append_int(0xBBBB)
        self.symb_table.append((string, oop))
        return oop

        # if string in self.symb_table:
        #     return self.symb_table[string]
        # else:
        #     delegate = self.append_object_instance()
        #     oop = self.append_external_ref('Symbol')
        #     self.append_pointer_to(delegate)        # delegate
        #     self.append_int(len(string))
        #     self.append_string(string)
        #     self.symb_table[string] = oop
        #     return oop

    def append_string_instance(self, string):
        if string in self.string_table:
            return self.string_table[string]
        else:
            delegate = self.append_object_instance()

            self.append_int(pyutils.FRAME_TYPE_BVAR_OBJECT)
            self.append_int((3 * bits.WSIZE) + bits.string_block_size(string + "\0"))

            oop = self.append_external_ref('String')
            self.append_pointer_to(delegate)        # delegate
            self.append_int(len(string))
            self.append_string(string)

            self.string_table[string] = oop
            return oop

    def _append_dict_prologue(self, size):
        delegate = self.append_object_instance()

        self.append_int(pyutils.FRAME_TYPE_DVAR_OBJECT)
        self.append_int((3 * bits.WSIZE) + (size * 2 * bits.WSIZE))

        oop = self.append_external_ref('Dictionary')  # vt
        self.append_pointer_to(delegate)              # delegate
        self.append_int(size)                         # dict length
        return oop

    def _append_dict_pairs(self, pairs):
        for key, val in pairs:
            self.append_pointer_to(key)
            self.append_pointer_to(val)

    def append_empty_dict(self):
        return self._append_dict_prologue(0)

    def append_dict_emiting_entries(self, entries_pydict):
        pairs_oop = []
        for key, entry, in entries_pydict.iteritems():
            key_oop = self.append_string_instance(key)
            val_oop = entry.fill(self)
            pairs_oop.append((key_oop, val_oop))
        return self.append_dict_with_pairs(pairs_oop)

    def append_dict_with_pairs(self, pairs):
        oop = self._append_dict_prologue(len(pairs))
        self._append_dict_pairs(pairs)
        return oop


    def append_empty_list(self):
        delegate = self.append_object_instance()

        self.append_int(pyutils.FRAME_TYPE_LVAR_OBJECT)
        self.append_int(3 * bits.WSIZE)

        oop = self.append_external_ref('List')         # vt
        self.append_pointer_to(delegate)               # delegate
        self.append_int(0)                             # len
        return oop


    # used internally to create class fields list, etc.
    def append_list_of_strings(self, lst):
        oops_elements = [self.append_string_instance(string) for string in lst]
        delegate = self.append_object_instance()

        self.append_int(pyutils.FRAME_TYPE_LVAR_OBJECT)
        self.append_int((3 + len(lst)) * bits.WSIZE)

        oop = self.append_external_ref('List')      # vt
        self.append_pointer_to(delegate)            # delegate
        self.append_int(len(lst))                   # len
        for oop_element in oops_elements:           # .. elements
            self.append_pointer_to(oop_element)
        return oop
