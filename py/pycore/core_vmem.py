import pyutils
from pycompiler import comp_vmemory
from pyutils import bits
from . import utils
from pdb import set_trace as br

class CoreVirtualMemory(comp_vmemory.CompVirtualMemory):
    def index_for(self, name):
        return self.base + self.index[name]

    def external_names(self):
        return sorted(set([x[0] for x in self.symb_table]))

    def append_external_ref(self, name, label=None):
        return self.append_label_ref(name, label) # core is self contained and does not require external names
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

