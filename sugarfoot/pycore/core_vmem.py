from pyutils import vmemory
from . import utils

class CoreVirtualMemory(vmemory.VirtualMemory):
    def __init__(self):
        self.string_table = {}
        super(CoreVirtualMemory, self).__init__()

    def index_for(self, name):
        return self.base + self.index[name]

    def addr_table(self):
        return [self.base + sum(self.cell_sizes[0:idx]) for idx,entry in enumerate(self.cells) if type(entry) == vmemory.PointerCell]

    def object_table(self):
        return reduce(lambda x,y: x+y, [e() for e in self.cells])

    def append_object_instance(self):
        oop = self.append_label_ref('Object') # vt
        self.append_null()                    # delegate: end of chain of delegation
        return oop

    def append_string_instance(self, string):
        if string in self.string_table:
            return self.string_table[string]
        else:
            delegate = self.append_object_instance() # Assumed to be object! if source change, this breaks
            oop = self.append_label_ref(utils.class_label('String'))   # vt
            self.append_pointer_to(delegate)        # delegate
            self.append_int(len(string))
            self.append_string(string)
            self.string_table[string] = oop
            return oop

    def _append_dict_prologue(self, size):
        delegate = self.append_object_instance()        # Assumed to be object! if source change, this breaks
        oop = self.append_label_ref(utils.class_label('Dictionary')) # vt
        self.append_pointer_to(delegate)          # delegate
        self.append_int(size)                     # dict length
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
        delegate = self.append_object_instance()                 # Assumed to be object! if source change, this breaks
        oop = self.append_label_ref(utils.class_label('List'))  # vt
        self.append_pointer_to(delegate)                        # delegate
        self.append_int(0)                                      # len
        return oop


    # used internally to create class fields list, etc.
    def append_list_of_strings(self, lst):
        oops_elements = [self.append_string_instance(string) for string in lst]
        delegate = self.append_object_instance()    # Assumed to be object! if source change, this breaks
        oop = self.append_label_ref('List')       # vt
        self.append_pointer_to(delegate)          # delegate
        self.append_int(len(lst))                 # len
        for oop in oops_elements:                   # .. elements
            self.append_pointer_to(oop)
        return oop
