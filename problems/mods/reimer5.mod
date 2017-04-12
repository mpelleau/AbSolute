var t;
var u;
var x;
var y;
var z;

subject to

cons1: -1 + 2*x^2 - 2*y^2 + 2*z^2 - 2*t^2 + 2*u^2 = 0;
cons2: -1 + 2*x^3 - 2*y^3 + 2*z^3 - 2*t^3 + 2*u^3 = 0;
cons3: -1 + 2*x^4 - 2*y^4 + 2*z^4 - 2*t^4 + 2*u^4 = 0;
cons4: -1 + 2*x^5 - 2*y^5 + 2*z^5 - 2*t^5 + 2*u^5 = 0;
cons5: -1 + 2*x^6 - 2*y^6 + 2*z^6 - 2*t^6 + 2*u^6 = 0;

#solve;
#display t, u, x, y, z;

# TITLE : The 5-dimensional system of Reimer.

# ROOT COUNTS :

# total degree : 720
# mixed volume : 720

# REFERENCES :

# See the PoSSo test suite.
# For general dimension n, the system looks like

# -1/2 + \sum_{i=1}^{n}(-1)^(i+1)x_{i}^k = 0, k=2..n+1 

# GENERATORS OF SYMMETRY GROUP :

# z y x t u
# u y z t x
# x t z y u

# NOTE :

# The system has 12 roots with all components positive,
# but any fine mixed subdivision has only one mixed cell.

# THE GENERATING SOLUTIONS :

# 12 5
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -1.25705460701026E-01   4.80449609667978E-01
#  y : -3.96732207927890E-01  -2.01295133009924E-01
#  z :  3.39026130046387E-01   4.97108436261838E-01
#  t : -9.17817062157943E-02  -4.55073462282180E-01
#  u :  8.75016837867993E-01   1.54169279604962E-02
# == err :  5.257E-16 = rco :  2.886E-02 = res :  4.381E-16 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x :  1.65179856275237E-01   4.01306224781909E-53
#  y :  3.71809860927029E-01   6.21535250576860E-53
#  z :  6.00580975840425E-01   6.78631664671847E-53
#  t :  8.06590703105641E-01   4.86135182865890E-53
#  u :  9.49130782115133E-01   1.47635013588467E-53
# == err :  4.081E-15 = rco :  1.931E-04 = res :  3.400E-16 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x :  3.28353058900760E-01   3.83293571654000E-01
#  y : -3.65037597430609E-01  -1.77941422105434E-01
#  z :  8.83954216708990E-01   7.47522218167706E-19
#  t : -3.65037597430609E-01   1.77941422105434E-01
#  u :  3.28353058900760E-01  -3.83293571654000E-01
# == err :  4.158E-16 = rco :  1.470E-02 = res :  3.643E-16 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -8.20501169693851E-01   4.81804127164467E-02
#  y :  1.92649800357694E-02  -8.15105711153907E-01
#  z :  2.83453963159985E-02   8.39642248705884E-01
#  t :  6.63305484227383E-01  -6.19401872141988E-01
#  u : -6.12614994436549E-01   6.70606441286687E-01
# == err :  3.890E-16 = rco :  1.109E-01 = res :  7.022E-16 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -2.89250316112716E-01   1.93720295400755E-02
#  y :  1.25951647109466E-01  -3.11152873990998E-01
#  z :  9.17357766838520E-01  -3.21236642103152E-03
#  t :  6.77654385257208E-01  -1.36742906837876E-02
#  u : -1.51240072064018E-01   2.63861320151929E-01
# == err :  4.402E-16 = rco :  6.008E-03 = res :  3.334E-16 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x :  2.92426652768470E-02   1.98441472348304E-01
#  y : -1.15079341958084E-01  -9.12609179052688E-02
#  z :  4.88543274566396E-01   1.87476161310206E-02
#  t :  7.55781391744456E-01   8.67380109394292E-03
#  u :  9.36082697242564E-01   2.23886684350489E-03
# == err :  5.112E-16 = rco :  1.351E-03 = res :  2.624E-16 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -8.23715781042161E-01  -2.47741481660290E-19
#  y :  5.91859519233860E-01  -5.00136991500666E-01
#  z : -5.37294659901288E-01   5.27047166093312E-01
#  t :  5.91859519233860E-01   5.00136991500666E-01
#  u : -5.37294659901288E-01  -5.27047166093312E-01
# == err :  4.104E-16 = rco :  1.415E-01 = res :  7.706E-16 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x :  8.75016837867993E-01  -1.54169279604962E-02
#  y : -9.17817062157942E-02   4.55073462282180E-01
#  z :  3.39026130046387E-01  -4.97108436261838E-01
#  t : -3.96732207927890E-01   2.01295133009924E-01
#  u : -1.25705460701026E-01  -4.80449609667978E-01
# == err :  5.296E-16 = rco :  2.849E-02 = res :  6.661E-16 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -1.51240072064018E-01  -2.63861320151929E-01
#  y :  1.25951647109466E-01   3.11152873990998E-01
#  z :  9.17357766838520E-01   3.21236642103152E-03
#  t :  6.77654385257208E-01   1.36742906837876E-02
#  u : -2.89250316112716E-01  -1.93720295400755E-02
# == err :  4.648E-16 = rco :  4.638E-03 = res :  3.268E-16 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -6.12614994436549E-01  -6.70606441286687E-01
#  y :  6.63305484227383E-01   6.19401872141988E-01
#  z :  2.83453963159985E-02  -8.39642248705884E-01
#  t :  1.92649800357694E-02   8.15105711153907E-01
#  u : -8.20501169693851E-01  -4.81804127164468E-02
# == err :  4.093E-16 = rco :  9.277E-02 = res :  1.423E-15 ==
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x :  2.92426652768470E-02  -1.98441472348305E-01
#  y :  7.55781391744457E-01  -8.67380109394305E-03
#  z :  9.36082697242564E-01  -2.23886684350493E-03
#  t : -1.15079341958084E-01   9.12609179052692E-02
#  u :  4.88543274566397E-01  -1.87476161310209E-02
# == err :  1.018E-15 = rco :  1.359E-03 = res :  3.501E-16 ==
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 12
# the solution for t :
#  x : -8.90691982819378E-02   8.72390401374093E-48
#  y :  3.04390884130488E-01  -2.35203294488113E-48
#  z :  5.59254489061997E-01   2.56585412168851E-49
#  t :  7.86835443685312E-01   4.49024471295489E-49
#  u :  9.43962351030283E-01   1.71056941445901E-49
# == err :  2.854E-15 = rco :  5.084E-04 = res :  4.003E-16 ==
