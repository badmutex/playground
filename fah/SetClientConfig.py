"""
Runs a FaH client in CRC. Used for makeflow to run on SGE, WorkQueue, Condor
"""

import sys
import ConfigParser as conf

def write_config(cfg, path, mode):
    try:
        fd = open(path, mode)
        cfg.write(fd)
    finally: fd.close()

def set(template, path, section, option, value, mode='w'):
    cfg = conf.RawConfigParser()
    cfg.read(template)
    cfg.set(section, option, value)
    write_config(cfg, path, mode)

def read_config(path):
    cfg = conf.RawConfigParser()
    cfg.read(path)
    return cfg


def main(**kwargs):
    template = kwargs.get('template', 'client.cfg.template')
    username = kwargs.get('username', 'nd_bot')
    machineid = kwargs.get('machineid', '1')
    config   = kwargs.get('config', 'client.cfg')

    usersection = kwargs.get('usersection', 'settings')
    machineidsection = kwargs.get('usersection', 'settings')

    cfg = read_config(template)
    cfg.set(usersection, 'username', username)
    cfg.set(machineidsection, 'machineid', machineid)

    write_config(cfg, config, 'w')


def usage():
    return '%s <username> <machineid>' % sys.argv[0]

if __name__ == '__main__':
    if len(sys.argv[1:]) is not 2:
        print usage()
        sys.exit(1)
    else:
        user = sys.argv[1]
        machine = sys.argv[2]
        main(username = user, machineid = machine)
