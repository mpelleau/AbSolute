var t;
var u;
var v;
var x;
var y;
var z;

subject to

cons1:  2*x^2+2*y^2+2*z^2+2*t^2+2*u^2+v^2-v = 0;
cons2:  x*y+y*z+2*z*t+2*t*u+2*u*v-u = 0;
cons3:  2*x*z+2*y*t+2*z*u+u^2+2*t*v-t = 0;
cons4:  2*x*t+2*y*u+2*t*u+2*z*v-z = 0;
cons5:  t^2+2*x*v+2*y*v+2*z*v-y = 0;
cons6:  2*x+2*y+2*z+2*t+2*u+v-1 = 0;

#solve;
#display t, u, v, x, y, z;

# TITLE : a problem of magnetism in physics

# ROOT COUNTS :

# total degree: 32
# mixed volume : 32

# REFERENCES :

# From the PoSSo test suite.

# Shigotoshi Katsura: "Users posing problems to PoSSO",
# in the PoSSo Newsletter, no. 2, July 1994,
# edited by L. Gonzalez-Vega and T. Recio.
# Available at http://janet.dm.unipi.it/

# S. Katsura, W. Fukuda, S. Inawashiro, N.M. Fujiki and R. Gebauer,
# Cell Biophysics, Vol 11, pages 309--319, 1987.

# W. Boege, R. Gebauer, and H. Kredel:
# "Some examples for solving systems of algebraic equations by
#  calculating Groebner bases", J. Symbolic Computation, 2:83-98, 1986.

# S. Katsura, in "New Trends in Magnetism", edited
# by M.D. Coutinho-Filho and S.M. Resende, 
# World Scientific, 1990.

# NOTE (excerpt from the PoSSo Newsletter) :

# The general formulation of the equations is

# \sum_{i=-N}^N u(l)*u(m-l) = u(m)
# \sum_{i=-N}^N u(l) = 1

# with m in {-N+1,-N,..,N-1}, u(l) = u(-l),
#  and u(l) = 0, for |l| > N,

# The number of solutions for a given N is 2^N.
# Among them, physically meaningful solutions are restricted
# to those for which u(l) is real and 0 <= u(l) <= 1,
# since u(l) is a probability.

# THE SOLUTIONS :

# 32 6
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -7.34679413715196E-02  -1.05421979432305E-81
#  y :  8.96500882133478E-02   7.90664845742289E-82
#  z :  3.22926736021011E-02   1.44955221719420E-81
#  t : -1.54099162732096E-01  -1.18599726861343E-81
#  u :  2.65738935518866E-01  -1.84488464006534E-81
#  v :  6.79770813538602E-01   3.68976928013068E-81
# == err :  4.235E-16 = rco :  6.845E-02 = res :  5.551E-17 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.23989867532776E-01   6.69182486630844E-83
#  y :  1.15327526760843E-02  -5.45641104483611E-83
#  z :  8.58389785887035E-02  -6.17706910736164E-84
#  t :  1.62143145160524E-01  -4.94165528588931E-83
#  u :  2.25869805737949E-01   2.05902303578721E-83
#  v :  2.77210370739033E-01   5.76526450020419E-83
# == err :  4.821E-16 = rco :  7.475E-02 = res :  8.327E-17 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -2.07358972809625E-01   1.44890865261227E-70
#  y :  9.35088892161247E-02  -1.01876389636801E-70
#  z :  2.19257911144064E-01  -7.24454326306137E-71
#  t :  2.25457387686581E-02  -9.05567907882671E-71
#  u :  1.51473206244495E-01   1.06121262558422E-70
#  v :  4.41146454872566E-01   9.05567907882671E-71
# == err :  2.406E-16 = rco :  1.067E-01 = res :  5.551E-17 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.57025351946495E+00   5.76457486699560E+00
#  y :  4.61777915035574E+00  -1.32891381878462E+00
#  z : -2.85008572379927E+00  -2.96262478568672E+00
#  t :  3.67679932837071E+00   1.95277814810887E+00
#  u : -6.44863384847331E-01  -3.92345674167498E+00
#  v : -5.45875170122980E+00   9.95284662083709E-01
# == err :  5.652E-15 = rco :  1.365E-03 = res :  2.010E-14 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.21538696343695E-01  -1.45427136994283E-01
#  y :  5.41432115293970E-03   1.92234187205295E-01
#  z : -1.29474720447878E-01   7.44121358558796E-02
#  t :  4.21087159135082E-01  -4.00121205095676E-02
#  u :  9.38435315354770E-02   3.49228636012240E-02
#  v :  4.61336809936150E-01  -2.32259858317095E-01
# == err :  4.096E-16 = rco :  3.034E-02 = res :  7.850E-17 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.27009396586336E-02   1.51426720098643E-01
#  y :  4.31073671633152E-01   9.79652306994441E-03
#  z :  4.97255031475553E-03  -2.20062605132790E-01
#  t : -1.04214329254954E-01  -1.50525567764471E-02
#  u : -6.21735758680806E-02   3.61797763381580E-02
#  v :  4.86085245667523E-01   7.54242848049839E-02
# == err :  5.026E-16 = rco :  3.855E-02 = res :  6.592E-17 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -5.83959866995207E-01   8.77442593123539E-01
#  y :  1.18070251828669E+00   1.16300260901468E-01
#  z : -3.02272354151035E-01  -7.53820937749179E-01
#  t : -5.40199676757081E-01  -2.65819846833774E-01
#  u :  3.27158857148399E-01   5.12015123631312E-01
#  v :  8.37141044936462E-01  -9.72234386146732E-01
# == err :  3.718E-15 = rco :  1.048E-02 = res :  6.661E-16 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  5.94183853341865E-02   1.95697375121334E-01
#  y :  3.99365516038540E-01  -4.47735495014597E-02
#  z :  3.56463693311711E-02  -1.98749596752736E-01
#  t : -2.07078028915640E-01  -4.06794736634173E-02
#  u :  3.02537558042511E-02   1.02020125595299E-01
#  v :  3.64788004814983E-01  -2.70297615980414E-02
# == err :  4.100E-16 = rco :  2.704E-02 = res :  2.695E-16 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.32387153995653E-02   0.00000000000000E+00
#  y : -8.73756842370348E-02  -5.52714787526044E-76
#  z : -6.41927643960892E-02   1.24360827193360E-75
#  t :  3.27425164118805E-01  -5.52714787526044E-76
#  u :  4.22144271367303E-02  -8.29072181289067E-76
#  v :  5.90335145554309E-01   1.65814436257813E-75
# == err :  4.302E-16 = rco :  3.254E-02 = res :  3.816E-17 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.81646358312146E-01  -3.05135743680520E-01
#  y : -2.45661127185011E-01   1.26402934892260E+00
#  z :  1.28100343359660E+00   1.95756286562680E-02
#  t :  2.69789837319146E-01  -6.92748789616714E-01
#  u : -8.81520435955827E-01  -4.69599332750762E-01
#  v :  5.16069301074466E-01   3.67757776938245E-01
# == err :  5.229E-15 = rco :  2.422E-02 = res :  5.620E-16 ==
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.21538696343695E-01   1.45427136994283E-01
#  y :  5.41432115293969E-03  -1.92234187205295E-01
#  z : -1.29474720447878E-01  -7.44121358558796E-02
#  t :  4.21087159135082E-01   4.00121205095676E-02
#  u :  9.38435315354771E-02  -3.49228636012240E-02
#  v :  4.61336809936150E-01   2.32259858317095E-01
# == err :  4.336E-16 = rco :  3.034E-02 = res :  6.206E-17 ==
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.27009396586336E-02  -1.51426720098643E-01
#  y :  4.31073671633152E-01  -9.79652306994440E-03
#  z :  4.97255031475552E-03   2.20062605132790E-01
#  t : -1.04214329254954E-01   1.50525567764471E-02
#  u : -6.21735758680806E-02  -3.61797763381580E-02
#  v :  4.86085245667523E-01  -7.54242848049839E-02
# == err :  5.075E-16 = rco :  3.855E-02 = res :  8.100E-17 ==
# solution 13 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  4.22327061275954E-01  -3.86856051726076E-01
#  y : -5.39101147413182E-01  -3.67652911266504E-02
#  z : -7.13331690209261E-02   1.11472099190827E-01
#  t :  1.25961423554398E-01   2.27162877399557E-01
#  u :  3.28700380323012E-01   3.47451620579309E-01
#  v :  4.66890902561487E-01  -5.24930508633935E-01
# == err :  5.281E-16 = rco :  3.983E-02 = res :  1.943E-16 ==
# solution 14 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.57025351946497E+00  -5.76457486699559E+00
#  y :  4.61777915035575E+00   1.32891381878461E+00
#  z : -2.85008572379926E+00   2.96262478568673E+00
#  t :  3.67679932837070E+00  -1.95277814810888E+00
#  u : -6.44863384847322E-01   3.92345674167498E+00
#  v : -5.45875170122980E+00  -9.95284662083694E-01
# == err :  1.842E-14 = rco :  1.365E-03 = res :  1.446E-14 ==
# solution 15 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -1.81646358312146E-01   3.05135743680519E-01
#  y : -2.45661127185011E-01  -1.26402934892261E+00
#  z :  1.28100343359660E+00  -1.95756286562676E-02
#  t :  2.69789837319146E-01   6.92748789616714E-01
#  u : -8.81520435955827E-01   4.69599332750762E-01
#  v :  5.16069301074466E-01  -3.67757776938245E-01
# == err :  5.120E-15 = rco :  2.422E-02 = res :  1.054E-15 ==
# solution 16 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -4.53090455165408E-01  -2.65159314313777E-02
#  y :  4.72369413963952E-01  -1.72189347661563E-01
#  z :  7.61392026176293E-02  -2.59841065181448E-02
#  t :  3.90830532173818E-02  -3.10677427065331E-02
#  u :  1.66041794189728E-02  -1.21033708197555E-01
#  v :  6.97789211894943E-01   7.53581673030348E-01
# == err :  5.479E-16 = rco :  2.561E-02 = res :  9.021E-17 ==
# solution 17 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -4.53090455165408E-01   2.65159314313777E-02
#  y :  4.72369413963952E-01   1.72189347661563E-01
#  z :  7.61392026176293E-02   2.59841065181447E-02
#  t :  3.90830532173818E-02   3.10677427065331E-02
#  u :  1.66041794189728E-02   1.21033708197555E-01
#  v :  6.97789211894943E-01  -7.53581673030348E-01
# == err :  6.846E-16 = rco :  2.561E-02 = res :  1.804E-16 ==
# solution 18 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  2.19202174282559E-01  -8.29072181289067E-76
#  y :  1.86196233915891E-01   3.21265470249513E-75
#  z : -2.33164321950951E-02  -1.10542957505209E-75
#  t : -6.22139522220197E-02  -3.05720366850343E-75
#  u :  6.08345024330872E-02   5.52714787526044E-76
#  v :  2.38594947571154E-01   2.59085056652833E-75
# == err :  4.761E-16 = rco :  3.716E-02 = res :  5.551E-17 ==
# solution 19 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  1.77720242269329E-01  -2.69880267346701E-79
#  y :  2.52054104063177E-01  -2.15904213877361E-78
#  z : -1.26609638729233E-01   4.93380435074275E-79
#  t :  6.57264116780968E-02   1.07952106938681E-78
#  u : -7.31811773405752E-02  -2.69880267346701E-79
#  v :  4.08580116118410E-01   1.61928160408021E-78
# == err :  5.089E-16 = rco :  4.527E-02 = res :  5.551E-17 ==
# solution 20 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  9.16958134062345E-02  -3.86900351268231E-74
#  y : -8.43879405799624E-02   1.10542957505209E-75
#  z : -1.02027573161162E-01   1.76868732008334E-74
#  t :  5.52705192655442E-02   1.98977323509376E-74
#  u :  3.08655664592089E-01   8.29072181289067E-75
#  v :  4.61587032954515E-01  -1.54760140507292E-74
# == err :  5.099E-16 = rco :  6.751E-02 = res :  2.776E-17 ==
# solution 21 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -3.71434890128193E+00  -2.26721676217087E+00
#  y : -1.30211184710235E+00   4.55637566992507E+00
#  z :  4.36802284527241E+00  -3.22491009143098E+00
#  t : -2.53969151312992E+00  -2.00811793381777E+00
#  u :  2.20426421471743E+00   2.90935925832586E+00
#  v :  2.96773040304869E+00   6.90197183373700E-02
# == err :  9.879E-15 = rco :  2.934E-03 = res :  1.465E-14 ==
# solution 22 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  1.40862400739638E-01  -1.38178696881511E-76
#  y :  1.92885306946496E-01  -2.76357393763022E-76
#  z : -5.91323323306972E-02   5.52714787526044E-76
#  t :  1.80509696917380E-01  -4.14536090644533E-76
#  u : -1.01057866510769E-01   5.52714787526044E-76
#  v :  2.91865588475904E-01  -5.52714787526044E-76
# == err :  2.834E-16 = rco :  5.475E-02 = res :  2.220E-16 ==
# solution 23 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  5.94183853341865E-02  -1.95697375121334E-01
#  y :  3.99365516038540E-01   4.47735495014597E-02
#  z :  3.56463693311711E-02   1.98749596752736E-01
#  t : -2.07078028915640E-01   4.06794736634173E-02
#  u :  3.02537558042511E-02  -1.02020125595299E-01
#  v :  3.64788004814983E-01   2.70297615980414E-02
# == err :  4.389E-16 = rco :  2.704E-02 = res :  8.777E-17 ==
# solution 24 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -3.17327431527865E-01   1.79678795707145E-02
#  y :  2.51074725113845E-01   1.35840540037664E+00
#  z :  1.60334509980202E+00  -4.17460309000618E-01
#  t : -4.52423320845073E-01  -1.14940404120786E+00
#  u : -7.68030460085420E-01   2.53099446870810E-01
#  v :  3.66722775084977E-01  -1.25216753219381E-01
# == err :  4.568E-15 = rco :  2.054E-02 = res :  1.153E-15 ==
# solution 25 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  2.10588109558726E-01   5.52714787526044E-76
#  y :  9.63978681241064E-02  -9.39615138794276E-75
#  z :  4.03916112981482E-02   8.84343660041671E-75
#  t :  4.17053473397812E-02   7.73800702536462E-75
#  u :  4.27934029063517E-02  -3.59264611891929E-75
#  v :  1.36247321545774E-01  -6.63257745031253E-75
# == err :  5.003E-16 = rco :  4.736E-02 = res :  5.551E-17 ==
# solution 26 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  4.22327061275954E-01   3.86856051726075E-01
#  y : -5.39101147413182E-01   3.67652911266506E-02
#  z : -7.13331690209261E-02  -1.11472099190827E-01
#  t :  1.25961423554398E-01  -2.27162877399557E-01
#  u :  3.28700380323012E-01  -3.47451620579309E-01
#  v :  4.66890902561488E-01   5.24930508633935E-01
# == err :  5.897E-16 = rco :  3.983E-02 = res :  3.664E-16 ==
# solution 27 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  0.00000000000000E+00   0.00000000000000E+00
#  y :  0.00000000000000E+00   0.00000000000000E+00
#  z :  1.36566297568363E-158   0.00000000000000E+00
#  t :  2.42427351708672E-191   0.00000000000000E+00
#  u :  0.00000000000000E+00   0.00000000000000E+00
#  v :  1.00000000000000E+00   0.00000000000000E+00
# == err :  1.215E-63 = rco :  6.598E-02 = res :  2.731E-158 ==
# solution 28 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x :  1.39125672609318E-01  -5.43582081447824E-82
#  y : -1.45563258086646E-01   2.71791040723912E-82
#  z : -1.14364175483778E-01   2.80027132867061E-82
#  t :  1.90920510948048E-01   6.09470818593015E-82
#  u :  5.32023462057030E-02  -2.96499317153359E-82
#  v :  7.53357807614709E-01  -5.92998634306717E-82
# == err :  4.485E-16 = rco :  4.737E-02 = res :  5.551E-17 ==
# solution 29 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -5.83959866995208E-01  -8.77442593123539E-01
#  y :  1.18070251828669E+00  -1.16300260901468E-01
#  z : -3.02272354151035E-01   7.53820937749179E-01
#  t : -5.40199676757081E-01   2.65819846833775E-01
#  u :  3.27158857148399E-01  -5.12015123631312E-01
#  v :  8.37141044936462E-01   9.72234386146732E-01
# == err :  4.110E-15 = rco :  1.048E-02 = res :  4.441E-16 ==
# solution 30 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -3.71434890128193E+00   2.26721676217088E+00
#  y : -1.30211184710235E+00  -4.55637566992507E+00
#  z :  4.36802284527241E+00   3.22491009143098E+00
#  t : -2.53969151312992E+00   2.00811793381777E+00
#  u :  2.20426421471743E+00  -2.90935925832586E+00
#  v :  2.96773040304869E+00  -6.90197183373696E-02
# == err :  6.334E-15 = rco :  2.934E-03 = res :  9.441E-15 ==
# solution 31 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -3.17327431527865E-01  -1.79678795707145E-02
#  y :  2.51074725113845E-01  -1.35840540037664E+00
#  z :  1.60334509980202E+00   4.17460309000618E-01
#  t : -4.52423320845073E-01   1.14940404120786E+00
#  u : -7.68030460085420E-01  -2.53099446870810E-01
#  v :  3.66722775084977E-01   1.25216753219381E-01
# == err :  4.622E-15 = rco :  2.054E-02 = res :  1.096E-15 ==
# solution 32 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x : -2.07926968038444E-01   1.34940133673351E-78
#  y :  1.09542324450522E-01  -1.55181153724353E-78
#  z :  1.63551292586952E-01   1.34940133673351E-79
#  t :  1.21988131355882E-01   1.24819623647849E-78
#  u : -5.03053401613212E-02  -7.42170735203429E-79
#  v :  7.26301119612819E-01  -1.21446120306016E-78
# == err :  5.627E-16 = rco :  4.755E-02 = res :  5.551E-17 ==
