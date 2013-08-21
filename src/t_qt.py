from PyQt4.QtGui import QApplication
from PyQt4.QtGui import QAction
import sys
import threading

_qt_lock = threading.Semaphore(0)

_qt_action = None
_qt_qapp = None

_the_function = None
_the_result = None
_proc_locks = []

_exiting = False

def to_qt(proc, fn):
    global _qt_action, _the_function, _the_result, _proc_locks
    _the_function = fn
    _proc_locks.append(proc.lock)
    _qt_action.trigger()          # notify qt thread we have something for it
    _proc_locks[-1].acquire()   # make proc wait for result
    _proc_locks.pop()
    return _the_result

def slot_incoming(*rest):
    global _the_function, _the_result, _proc_locks
    _the_result = _the_function()
    _proc_locks[-1].release()

_exit_code = None
def exit(proc, code):
    global _exiting, _qt_qapp, _exit_code
    if _qt_qapp:
        _qt_qapp.exit(code)
    else:
        _exit_code = code
        _exiting = True
        _qt_lock.release()


# this is the main python thread
def qt_main():
    global _qt_action, _proc_lock, _qt_qapp, _exit_code

    _qt_lock.acquire() # wait 'til someone wants Qt

    if _exiting:
        sys.exit(_exit_code)
        return

    l = _proc_locks[-1]

    print 'qt_main: creating QApplication...'
    _qt_qapp = QApplication(sys.argv)

    print "qt_main: Setting up action..."
    _qt_action = QAction(None)
    _qt_action.triggered.connect(slot_incoming)

    print "qt_main: release main"
    l.release()   # let caller thread go
    _qt_qapp.exec_()  # enter the qt loop in the main thread


def start_qt(proc_lock):
    global _proc_locks
    _proc_locks.append(proc_lock)
    _qt_lock.release()
    proc_lock.acquire()
    _proc_locks.pop()

    assert _qt_qapp != None, "start_qt should have _qt_qapp"
    assert _qt_action != None, "start_qt should have _qt_action"

# our decorator for Qt actions
def on_qt_thread(fn):
    def wrapp(proc):
        return to_qt(proc, lambda: fn(proc))
    return wrapp
