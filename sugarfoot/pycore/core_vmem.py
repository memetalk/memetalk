import pyutils
from pyutils import vmemory
from pyutils import bits
from . import utils
from pdb import set_trace as br

class CoreVirtualMemory(vmemory.VirtualMemory):
    def __init__(self):
        self.string_table = {}
        self.symb_table = []
        super(CoreVirtualMemory, self).__init__()

    def index_for(self, name):
        return self.base + self.index[name]

    def reloc_table(self):
        return [self.base + sum(self.cell_sizes[0:idx]) for idx,entry in enumerate(self.cells) if type(entry) == vmemory.PointerCell]

    def object_table(self):
        return reduce(lambda x,y: x+y, [e() for e in self.cells])

    def external_names(self):
        return sorted(set([x[0] for x in self.symb_table]))

    def symbols_references(self):
        sr = []
        for text, ptr in self.symb_table:
             for referer in [x for x in self.cells if type(x) == vmemory.PointerCell and x.target_cell == ptr]:
                 sr.append((text, self.base + sum(self.cell_sizes[0:self.cells.index(referer)])))
        return sr

    def append_external_ref(self, name, label):
        return self.append_label_ref(name, label) # core is self contained and does not require external names

    def append_object_instance(self):
        self.append_int(pyutils.FRAME_TYPE_OBJECT)
        self.append_int(2 * bits.WSIZE)

        oop = self.append_label_ref('Object') # vt
        self.append_null()                    # delegate: end of chain of delegation
        return oop

    def append_string_instance(self, string):
        if string in self.string_table:
            return self.string_table[string]
        else:
            delegate = self.append_object_instance() # Assumed to be object! if source change, this breaks

            self.append_int(pyutils.FRAME_TYPE_BVAR_OBJECT)
            self.append_int((3 * bits.WSIZE) + bits.string_block_size(string + "\0"))

            oop = self.append_label_ref(utils.class_label('String'))   # vt
            self.append_pointer_to(delegate)        # delegate
            self.append_int(len(string))
            self.append_string(string)

            self.string_table[string] = oop
            return oop

    def append_symbol_instance(self, string):
        # We only need a dummy/word-sized placeholder here.
        # (actually, not event this. We only need to return a dummy cell. All we need
        #  its the PointerCell's pointing to our dummy to construct the symbols_reference())
        # In any case, the full data is only used by dump to display the text of the symbol
        delegate = self.append_object_instance() # Assumed to be object! if source change, this breaks

        self.append_int(pyutils.FRAME_TYPE_BVAR_OBJECT)
        self.append_int((3 * bits.WSIZE) + bits.string_block_size(string + "\0"))

        oop = self.append_label_ref(utils.class_label('Symbol'))   # vt
        self.append_pointer_to(delegate)                           # delegate
        self.append_int(len(string))
        self.append_string(string)

        self.symb_table.append((string, oop))
        return oop

    def _append_dict_prologue(self, size):
        delegate = self.append_object_instance()        # Assumed to be object! if source change, this breaks

        self.append_int(pyutils.FRAME_TYPE_DVAR_OBJECT)
        self.append_int((3 * bits.WSIZE) + (size * 2 * bits.WSIZE))

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

    def append_string_dict(self, pydict):
        pairs_oop = []
        for key, val in pydict.iteritems():
            key_oop = self.append_string_instance(key)
            val_oop = self.append_string_instance(val)
            pairs_oop.append((key_oop, val_oop))
        return self.append_dict_with_pairs(pairs_oop)

    def append_sym_dict_emiting_entries(self, entries_pydict):
        pairs_oop = []
        for key, entry, in entries_pydict.iteritems():
            key_oop = self.append_symbol_instance(key)
            val_oop = entry.fill(self)
            pairs_oop.append((key_oop, val_oop))
        return self.append_dict_with_pairs(pairs_oop)

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

        self.append_int(pyutils.FRAME_TYPE_LIST_OBJECT)
        self.append_int(3 * bits.WSIZE)

        oop = self.append_label_ref(utils.class_label('List'))  # vt
        self.append_pointer_to(delegate)                        # delegate
        self.append_int(0)                                      # len
        return oop


    # used internally to create class fields list, etc.
    def append_list_of_strings(self, lst):
        oops_elements = [self.append_string_instance(string) for string in lst]
        delegate = self.append_object_instance()  # Assumed to be object! if source change, this breaks

        if len(lst) > 0:
            self.append_int(pyutils.FRAME_TYPE_ELEMENTS)
            self.append_int(len(lst) * bits.WSIZE)
            oops = []
            for oop_element in oops_elements:         # .. elements
                oops.append(self.append_pointer_to(oop_element))
            frame_size = 4 * bits.WSIZE
        else:
            frame_size = 3 * bits.WSIZE

        self.append_int(pyutils.FRAME_TYPE_LIST_OBJECT)
        self.append_int(frame_size)

        oop = self.append_label_ref('List')       # vt
        self.append_pointer_to(delegate)          # delegate
        self.append_int(len(lst))                 # len
        if len(lst) > 0:
            self.append_pointer_to(oops[0])
        return oop
