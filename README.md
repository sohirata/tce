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
     
