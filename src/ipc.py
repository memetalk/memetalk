import multiprocessing
import sys

# 1) We can't put a Manager.Queue() into a dshared container
#    -That means we can't put it on an instance (eg. Process) that
#     will live in a dshared container.
#
# 2) We can't put a Manager.Queue() into a Manager.dict() into another
#    Manager.dict().
#    -It also means we can't create a class with Queues
#     and put these instances in a Manager.dict() and so on.
#
# 3) It seems we can't have a python dicts mapping procids -> Manager.dicts
#    because the simple minded dicts are not shared, and changes on them won't
#    be visible (or I'm too sleepy to think about why it wasn't working).
#
# 4) BUT we can have a Manager.dict() containing Queues on a single nesting
#    level.
#
# All we need is the channels to be set up in a Manager.dict before
# forking. After that, changing the channels (say, to store the dbg channel on
# the target process dbg slot) will work. So, instead of having Manager.dict
# mapping procids to other manager.dict will be creating these guys as module
# attributes.

_manager             = multiprocessing.Manager()
_interpreter_channel = _manager.Queue()
_this_mod             = sys.modules[__name__]

def create_channels_for_proc(procid, target_process):
    global _manager
    global _interpreter_channel
    global _this_mod

    entry_name = 'channel_' + str(procid)
    setattr(_this_mod, entry_name, multiprocessing.Manager().dict())
    channels = getattr(_this_mod, entry_name)

    channels['my'] = multiprocessing.Manager().Queue()

    # channel where I receive commands from outside (ie. a debugger)
    channels['my'] = _manager.Queue()

    # channel where I command a target process (ie. I'm a debugger)
    if target_process != None:
        channels['target'] = proc_channel(target_process.procid, 'my')
    else:
        channels['target'] = None

    # channel where I send info to my debugger  (ie. I'm a target process)
    channels['dbg'] = None

    # channel where I receive info from my target (ie. I'm a debugger)
    channels['target_incoming'] = _manager.Queue()

def interpreter_channel():
    return _interpreter_channel

def proc_channel(procid, name):
    global _this_mod
    entry_name = 'channel_' + str(procid)
    channels = getattr(_this_mod, entry_name)
    return channels[name]

def set_proc_channel(procid, name, channel):
    global _this_mod
    entry_name = 'channel_' + str(procid)
    channels = getattr(_this_mod, entry_name)
    channels[name] = channel
