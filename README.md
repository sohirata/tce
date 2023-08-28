# tce
Tensor Contraction Engine

# author
So Hirata (sohirata@illinois.edu)

# references
S. Hirata
J. Phys. Chem. A <b>107</b>, 9887-9897 (2003).

# how to compile
There is no compilation needed. Copy ccc.py, oce.py, tce.py in a directory.

# how to execute using GUI
A GUI will start by

     python ccc.py &

Choose appropriate options and press the bar.

# how to execute command-line
The CCSD T2 amplitude equation can be derived by oce.py as follows:

     python
     >>> import oce
     >>> a = oce.readfromfile("input/ccsd_t2.in")
     >>> a = a.performfullcontraction()
     >>> a = a.deletedisconnected()
     >>> a = a.simplify()
     >>> print(a)
     >>> a.writetofile("output/ccsd_t2.out")

The CCSD T2 amplitude equation can then be transformed to a Fortran77 code to be called by NWChem:

     python
     >>> import tce
     >>> b = tce.readfromfile("output/ccsd_t2.out")
     >>> b = b.breakdown()
     >>> b = b.fullyfactorize()
     >>> print(b)
     >>> c = b.fortran77("ccsd_t2",excitation=['t','t1','t2','t3','x','c'],deexcitation=['y'], \
                         general=['d','e','f','v'],intermediate=['i','j'],active=1,program="NWCHEM")
     >>> print(c)
     >>> c.writetofile("fortran/ccsd_t2")
     
