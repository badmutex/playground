
from optparse import OptionParser

if __name__ == '__main__':
    parser = OptionParser()
    parser.add_option( '-f', '--file', dest='filename',
                       help='output file', metavar='FILE')

    opts, args = parser.parse_args()
    print opts, args
