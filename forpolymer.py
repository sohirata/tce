import sys
import types
import string
import locale
import tce

print("Select the algorithm: (1) Permutation, (2) Nonpermutation")
codes = ['permutation','nopermutation']
number = sys.stdin.readline()
number = locale.atoi(number[0:len(number)-1])
code = codes[number-1]
print("Select the algorithm: (1) No mapping, (2) Mapping")
mappingvals = ['n','y']
number = sys.stdin.readline()
number = locale.atoi(number[0:len(number)-1])
mappingval = mappingvals[number-1]
if ((code == 'permutation') and (mappingval == 'y')):
  prefix = 'p_m_'
elif ((code == 'nopermutation') and (mappingval == 'y')):
  prefix = 'np_m_'
elif ((code == 'permutation') and (mappingval == 'n')):
  prefix = 'p_'
elif ((code == 'nopermutation') and (mappingval == 'n')):
  prefix = 'np_'
print(code)

list0 = ['ccs_e','ccs_t1','ccd_e','ccd_t2','ccsd_e','ccsd_t1','ccsd_t2','ccsdt_e','ccsdt_t1','ccsdt_t2','ccsdt_t3']

list1 = ['ccs_t1_disconnected','ccd_t2_disconnected','ccsd_t1_disconnected','ccsd_t2_disconnected',\
         'ccsdt_t1_disconnected','ccsdt_t2_disconnected','ccsdt_t3_disconnected',\
         'ccsd_t2_overlap','ccsdt_t3_overlap','lccsd_e','lccsd_t1','lccsd_t2']

list2 = ['lccd_e','lccd_t2','ccd_e','ccd_t2','lccsd_e','lccsd_t1','lccsd_t2','ccsd_e','ccsd_t1','ccsd_t2',\
         'ccsdt_e','ccsdt_t1','ccsdt_t2','ccsdt_t3','ccsdtq_e','ccsdtq_t1','ccsdtq_t2','ccsdtq_t3','ccsdtq_t4',\
         'qcisd_e','qcisd_t1','qcisd_t2','cisd_e','cisd_c1','cisd_c2','cisdt_e','cisdt_c1','cisdt_c2','cisdt_c3',\
         'cisdtq_e','cisdtq_c1','cisdtq_c2','cisdtq_c3','cisdtq_c4','mbpt1_t1','mbpt1_t2','mbpt2_e',\
         'mbpt2_t1','mbpt2_t2','mbpt2_t3','mbpt2_t4','mbpt3_e','mbpt3_t1','mbpt3_t2','mbpt4_e','mbpt2_t2_b',\
         'ccsd_lambda1','ccsd_lambda2','ccsd_density1','ccsdt_lambda1','ccsdt_lambda2','ccsdt_lambda3',\
         'ccsdt_density1','ccsdtq_lambda1','ccsdtq_lambda2','ccsdtq_lambda3','ccsdtq_lambda4','ccsdtq_density1',\
         'eomccsd_x1','eomccsd_x2','eomccsd_y1','eomccsd_y2','eomccsdt_x1','eomccsdt_x2','eomccsdt_x3',\
         'eomccsdt_y1','eomccsdt_y2','eomccsdt_y3','eomccsdtq_x1','eomccsdtq_x2','eomccsdtq_x3','eomccsdtq_x4',\
         'eomccsdtq_y1','eomccsdtq_y2','eomccsdtq_y3','eomccsdtq_y4','eomccsd_denominator','eomccsd_density1',\
         'eomccsdt_denominator','eomccsdt_density1','eomccsdtq_denominator','eomccsdtq_density1',\
         'cis_denominator','cis_density1','cis_x1','ipeomccsd_x1','ipeomccsd_x2','ipeomccsdt_x1',\
         'ipeomccsdt_x2','ipeomccsdt_x3','ipeomccsdtq_x1','ipeomccsdtq_x2','ipeomccsdtq_x3','ipeomccsdtq_x4',\
         'eaeomccsd_x1','eaeomccsd_x2','eaeomccsdt_x1','eaeomccsdt_x2','eaeomccsdt_x3','eaeomccsdtq_x1',\
         'eaeomccsdtq_x2','eaeomccsdtq_x3','eaeomccsdtq_x4']

for model in list0:
   print("Entering ",model)
   tce.readfromfile("output/"+model+'.out').breakdown().fullyfactorize() \
     .fortran90(prefix+model,mode=code,mapping=mappingval,excitation=['t','t1','t2','t3','x','c'],deexcitation=['y'], \
     general=['d','e','f','v'],intermediate=['i','j']).writetofile("forpolymerout/"+prefix+model)
