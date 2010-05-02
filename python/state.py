


def lift_state (v): return lambda s: (s, v)

def bind_state (m, f):
    def bind_run_state(s):
        s0, a = m (s)
        return f (a, s0)
    return bind_run_state


def get_state (s): return (s,s)

def put_state (v): lambda s: (v, None)


    
    
