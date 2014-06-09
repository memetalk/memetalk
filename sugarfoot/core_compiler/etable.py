

class Cell(object):
    def index(self):
        return self.etable.base + self.etable.entries.index(self)

class ValueCell(Cell):
    def __init__(self, etable, num):
        self.etable = etable
        self.num = num

    def __call__(self):
        return self.num

class PointerCell(Cell):
    def __init__(self, etable, target_cell):
        self.etable = etable
        self.target_cell = target_cell

    def __call__(self):
        return self.etable.base + self.etable.entries.index(self.target_cell)

class VirtualEntryTable(object):
    def __init__(self):
        self.entries = []
        self.base = 0
        self.index = {}

    def index_size(self):
        return len(self.index)

    def append_int(self, num, label=None):
        cell = ValueCell(self, num)
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    def append_pointer_to(self, cell, label=None):
        cell = PointerCell(self, cell)
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    def append_label_ref(self, target_label, label=None):
        if target_label == label:
            cell = self.circular_cell(label)
        else:
            cell = PointerCell(self, self.entries[self.index[target_label]])
        if label is not None:
            self.label_current(label)
        self.entries.append(cell)
        return cell

    def circular_cell(self, label):
        cell = PointerCell(self, None)
        cell.target_cell = cell
        return cell

    def set_base(self, base):
        self.base = base

    def label_current(self, label):
        self.index[label] = len(self.entries)

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
    tb.set_base(100)
    tb.dump()
