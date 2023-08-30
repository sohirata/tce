# tce
Tensor Contraction Engine (TCE) is a symbol algebra code written in Python, which derives working equations of <i>ab initio</i> electron-correlated theories and synthesizes the corresponding computer programs that execute in parallel as a part of NWCHEM or UTCHEM quantum chemistry software.

# authors
TCE has been developed by So Hirata at Pacific Northwest National Laboratory, University of Florida, and University of Illinois at Urbana-Champaign with contributions from Dr. Muneaki Kamiya (ionization and electron attachment) and Dr. Peng-Dong Fan (perturbation corrections) at University of Florida. TCE generates codes for NWCHEM developed by Pacific Northwest National Laboratory and distributed as open-source under the Educational Community License version 2.0. So Hirata thanks Dr. Robert J. Harrison, Dr. Marcel Nooijen, Dr. Alexander A. Auer, Dr. David E. Bernholdt, Dr. Venkatesh Choppella, Dr. P. Sadayappan, Dr. Gerald Baumgartner, Dr. Daniel Cociorva, Dr. Russell Pitzer, Dr. J. Ramanujam, Dr. Jarek Nieplocha, Dr. Theresa L. Windus, Dr. Michel Dupuis, Dr. Takeshi Yanai, and Dr. Kimihiko Hirao. A newer, more advanced version of TCE has also been developed by Dr. Toru Shiozaki, Dr. Muneaki Kamiya, Dr. Edward Valeev, and So Hirata for explicitly correlated methods.

# references
S. Hirata, J. Phys. Chem. A <b>107</b>, 9887-9897 (2003). </br>
S. Hirata, J. Chem. Phys. <b>121</b>, 51-59 (2004). </br>
S. Hirata, Theor. Chem. Acc. <b>116</b>, 2-17 (2006).

# how to compile
There is no compilation needed. Copy ccc.py, oce.py, tce.py in a directory.

# how to execute using GUI
A GUI will start by

     python ccc.py &

The CCSD <i>T</i><sub>2</sub> amplitude equation will be derived and the corresponding codes will be generated by the following choice of options:
![TCE_GUI_CCSD_T2](https://github.com/sohirata/tce/assets/57192472/35875bf1-843a-47b6-8925-5134b18b2bff)


# how to execute command-line
The CCSD <i>T</i><sub>2</sub> amplitude equation can be derived by oce.py as follows:

     python
     >>> import oce
     >>> a = oce.readfromfile("input/ccsd_t2.in")
     >>> a = a.performfullcontraction()
     >>> a = a.deletedisconnected()
     >>> a = a.simplify()
     >>> print(a)
     >>> a.writetofile("output/ccsd_t2.out")

The CCSD <i>T</i><sub>2</sub> amplitude equation can then be transformed to a parallel Fortran77 code to be a part of NWCHEM.

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


     
