from pyutils import vmemory

class CompVirtualMemory(vmemory.VirtualMemory):
    def __init__(self):
        super(CompVirtualMemory, self).__init__()
        self.ext_ref_table = []
        self.string_table = {}

    def external_references(self):
        return [(x[0], self.base + sum(self.cell_sizes[0:self.cells.index(x[1])])) for x in self.ext_ref_table]

    def append_external_ref(self, name, label=None):
        oop = self.append_int(999, label)
        self.ext_ref_table.append((name, oop))
        return oop

    def append_object_instance(self):
        oop = self.append_external_ref('Object')
        self.append_null()             # delegate
        return oop

    def append_string_instance(self, string):
        if string in self.string_table:
            return self.string_table[string]
        else:
            delegate = self.append_object_instance()
            oop = self.append_external_ref('String')
            self.append_pointer_to(delegate)        # delegate
            self.append_int(len(string))
            self.append_string(string)
            self.string_table[string] = oop
            return oop

    def _append_dict_prologue(self, size):
        delegate = self.append_object_instance()
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
        oop = self.append_external_ref('List')         # vt
        self.append_pointer_to(delegate)               # delegate
        self.append_int(0)                             # len
        return oop


    # used internally to create class fields list, etc.
    def append_list_of_strings(self, lst):
        oops_elements = [self.append_string_instance(string) for string in lst]
        delegate = self.append_object_instance()
        oop = self.append_external_ref('List')      # vt
        self.append_pointer_to(delegate)            # delegate
        self.append_int(len(lst))                   # len
        for oop in oops_elements:                   # .. elements
            self.append_pointer_to(oop)
        return oop
