import sys, os;
from cvxopt import solvers;
from cvxopt import matrix;
sys.path.append('/bio/lib/python2.7/site-packages');
sys.path.append('/bio/lib/site-python');
import numpy as np;

def getMatrix(file):
    with open(file, 'r') as f:
        (row, col) = [int(i) for i in f.readline()[:-1].split('\t')];
        m = np.zeros( (row, col), dtype = np.float );
        for c in xrange(col):
            for r in xrange(row):
                try:
                    m[r, c] = float(f.readline()[:-1]);
                except IndexError:
                    print "col = {0}, row = {1}".format(col, row)
                    print "c = {0}, r = {1}".format(c, r)
                    raise;
    return m;

def main(argv):
    try:
        infileP = argv[1];
        infileq = argv[2];
        outfile = argv[3];
    except IndexError:
        print 'usage: {prog} <file P> <file q> <out>'.format(prog = argv[0]);
    else:
        P = matrix(getMatrix(infileP), tc = 'd');
        print P;
        q = matrix(getMatrix(infileq), tc = 'd');
        print q;
        sol = solvers.qp(P, q);
        print  sol['x'];
        print sol['primal objective'];
        np.savetxt(outfile, sol['x']);

if __name__ == "__main__":
    main(sys.argv);
