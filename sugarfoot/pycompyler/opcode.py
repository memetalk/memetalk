# bytecode
#
# word: 32 bits
#
#  - [op: 8 bits] [args: 24 bits]
#
# Groups:
#
# 1 pushes: 8
# 2 pops: 5
# 3 returns: 6
# 4 calls: 4
# 5 jumps: 3
# 6 exits: 1

WORD_SIZE = 4

opcode_mapping = {
    # push
    "push_param": 1,
    "push_local": 2,
    "push_literal": 3,
    "push_field": 4,
    "push_env": 5,
    "push_this": 6,
    "push_module": 7,
    "push_ntf": 8,
    # pop
    "pop_param": 20,
    "pop_local": 21,
    "pop_field": 22,
    "pop_env": 23,
    "pop": 24,
    # ret
    "ret_this":  30,
    "ret_top":  31,
    "ret_ntf":  32,
    "ret_param":  33,
    "ret_local":  34,
    "ret_field":  35,
    # calls
    "send":  40,
    "call":  41,
    "super_send":  42,
    "super_ctor_send":  43,
    #
    # jumps
    "jump_on_false": 50,
    "jump": 51,
    "jump_back": 52,
    # ...
    "exit": 60
    }

def bytecode_for(key, arg):
    global opcode_mapping
    for k,v in opcode_mapping.iteritems():
        if k == key:
            return encode(v, arg)
    raise Exception("Opcode not found for",key)

def encode(op, arg):
    return (op << 24) + arg
