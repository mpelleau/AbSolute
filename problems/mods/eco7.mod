var x1;
var x2;
var x3;
var x4;
var x5;
var x6;
var x7;

subject to

cons1:  (x1 + x1*x2 + x2*x3 + x3*x4 + x4*x5 + x5*x6)*x7 - 1 = 0;
cons2:  (x2 + x1*x3 + x2*x4 + x3*x5 + x4*x6)*x7 - 2 = 0;
cons3:  (x3 + x1*x4 + x2*x5 + x3*x6)*x7 - 3 = 0;
cons4:  (x4 + x1*x5 + x2*x6)*x7 - 4 = 0;
cons5:  (x5 + x1*x6)*x7 - 5 = 0;
cons6:  x6*x7 - 6 = 0;
cons7:  x1 + x2 + x3 + x4 + x5 + x6 + 1 = 0;

#solve;
#display x1, x2, x3, x4, x5, x6, x7;

# TITLE : 7-dimensional economics problem

# ROOT COUNTS :

# total degree : 486
# 3-homogeneous Bezout number : 112
#   with partition : {x1 x2 x3 x4 x5 }{x6 }{x7 }
# generalized Bezout number : 80
#   based on the set structure :
#      {x1 x3 x5 }{x2 x4 x6 }{x7 }
#      {x1 x2 x5 x6 }{x3 x4 }{x7 }
#      {x1 x2 x3 }{x4 x5 x6 }{x7 }
#      {x1 x2 x4 }{x5 x6 }{x7 }
#      {x1 x5 }{x6 }{x7 }
#      {x6 }{x7 }
#      {x1 x2 x3 x4 x5 x6 }
# mixed volume : 32

# REFERENCE :

# Alexander Morgan:
# `Solving polynomial systems using continuation for engineering
#  and scientific problems', Prentice-Hall, Englewood Cliffs, New Jersey, 1987,
# (p 148).

# NOTE :

# Transform u = 1/x7 and the total degree equals the number of solutions.
# See the reduced economics problem, in the file redeco7.

# THE SOLUTIONS :

# 32 7
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  5.93369639826478E-03   1.28636083334685E-01
#  x2 :  5.95495251474475E-01   2.08845973813888E+00
#  x3 : -6.99468204461881E-01  -4.29895386284063E-01
#  x4 : -1.32701859744045E+00  -6.35115643463731E-01
#  x5 :  1.49441734602828E-01  -5.41035850350458E-01
#  x6 :  2.75616119426760E-01  -6.11048941375316E-01
#  x7 :  3.68023794115138E+00   8.15919439917735E+00
# == err :  4.597E-15 = rco :  1.056E-02 = res :  7.332E-15 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  5.93369639826586E-03  -3.71479583210633E+00
#  x2 : -6.29608516467496E+00   2.89485362873523E-01
#  x3 :  6.85454641806504E-01   6.41622249799803E+00
#  x4 :  4.94031447364828E+00  -2.36147158475214E-01
#  x5 :  2.97245117069881E-01  -2.53379026615599E+00
#  x6 : -6.32862764247967E-01  -2.20974604134012E-01
#  x7 : -8.45046930263530E+00   2.95062249572451E+00
# == err :  1.027E-14 = rco :  1.203E-03 = res :  1.484E-13 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  2.63424603497105E-01  -1.79307987438582E+00
#  x2 : -1.42230137654078E+00   7.27270787149925E-01
#  x3 :  6.48059125615115E-01   7.41546225335957E-01
#  x4 : -1.21625875444223E-01  -4.44285120925777E-01
#  x5 : -5.08590494417704E-01   4.40081292362660E-01
#  x6 :  1.41034017290485E-01   3.28466690463057E-01
#  x7 :  6.62230193448536E+00  -1.54232690910824E+01
# == err :  4.346E-14 = rco :  2.736E-03 = res :  8.981E-15 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -2.51557210700575E-01   1.79307987438582E+00
#  x2 : -5.18994747263476E-01  -1.65067431386248E+00
#  x3 :  2.54014532482806E+00   1.49593314929199E+00
#  x4 : -1.73247661935806E+00   1.65911587822018E+00
#  x5 : -1.53306651761352E+00  -2.14239297415820E+00
#  x6 :  4.95949770107570E-01  -1.15506161387731E+00
#  x7 :  1.88319443182410E+00   4.38593932445156E+00
# == err :  4.379E-15 = rco :  1.345E-02 = res :  8.894E-15 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.27399636935069E-01  -3.71479583210633E+00
#  x2 : -6.84186578113854E+00  -2.76209288924990E+00
#  x3 : -4.12071064151692E+00   8.69959556204953E+00
#  x4 :  8.72511873116428E+00   3.50394294906873E+00
#  x5 :  1.81846960499532E+00  -5.23487937380794E+00
#  x6 : -1.40841155043922E+00  -4.91770415954087E-01
#  x7 : -3.79717658548780E+00   1.32584762480407E+00
# == err :  8.966E-15 = rco :  2.797E-03 = res :  7.105E-14 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  5.93369639826475E-03  -1.28636083334685E-01
#  x2 :  5.95495251474475E-01  -2.08845973813888E+00
#  x3 : -6.99468204461880E-01   4.29895386284063E-01
#  x4 : -1.32701859744045E+00   6.35115643463731E-01
#  x5 :  1.49441734602828E-01   5.41035850350458E-01
#  x6 :  2.75616119426760E-01   6.11048941375316E-01
#  x7 :  3.68023794115138E+00  -8.15919439917735E+00
# == err :  4.866E-15 = rco :  1.056E-02 = res :  3.878E-15 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  3.97308366771297E-01   1.92171595772051E+00
#  x2 : -2.38300160166170E+00   1.65159813713614E+00
#  x3 : -1.17635241772214E+00  -2.17748014148655E+00
#  x4 :  5.01732358554671E-01  -1.22966143182879E+00
#  x5 :  1.19054568318791E+00  -6.79107470333444E-01
#  x6 :  4.69767610869959E-01   5.12934948792143E-01
#  x7 :  5.82616709754244E+00  -6.36153845578694E+00
# == err :  4.678E-15 = rco :  5.913E-03 = res :  1.465E-14 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  2.63424603497105E-01   1.79307987438582E+00
#  x2 : -1.42230137654078E+00  -7.27270787149925E-01
#  x3 :  6.48059125615115E-01  -7.41546225335957E-01
#  x4 : -1.21625875444223E-01   4.44285120925777E-01
#  x5 : -5.08590494417704E-01  -4.40081292362661E-01
#  x6 :  1.41034017290485E-01  -3.28466690463057E-01
#  x7 :  6.62230193448536E+00   1.54232690910824E+01
# == err :  4.305E-14 = rco :  2.736E-03 = res :  8.882E-15 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.08489054403391E+00  -1.79307987438582E+00
#  x2 : -1.75656198282473E+00  -7.45683258320039E-01
#  x3 : -2.96247415748369E-01   2.32634270785216E+00
#  x4 :  1.04373156113984E+00  -8.97470024637929E-01
#  x5 : -1.38967844523800E+00   3.78900562083038E-01
#  x6 :  3.13865738637349E-01   7.30989887408594E-01
#  x7 :  2.97569862064542E+00  -6.93036968326386E+00
# == err :  4.644E-15 = rco :  8.877E-03 = res :  1.421E-14 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  6.93515873660876E-01  -4.74778387287990E-66
#  x2 :  7.22177850183674E-01  -1.42433516186397E-65
#  x3 : -1.65871370380082E+00  -7.59645419660784E-65
#  x4 :  1.16901800599298E-01  -4.74778387287990E-66
#  x5 : -1.07196055981947E-01   0.00000000000000E+00
#  x6 : -7.66685764661078E-01   4.50070940743944E-65
#  x7 : -7.82589201020625E+00  -4.59406805440176E-64
# == err :  2.368E-15 = rco :  2.397E-02 = res :  1.776E-15 ==
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.27399636935069E-01   1.28636083334685E-01
#  x2 :  4.97146350108961E-02   2.19412989932238E+00
#  x3 :  1.55565100551911E-01   1.21486906126276E+00
#  x4 : -2.47476413001305E+00  -2.09079838123884E+00
#  x5 : -1.71288232676718E-01  -8.69709294847598E-02
#  x6 :  6.13372990191896E-01  -1.35986573319623E+00
#  x7 :  1.65369671656056E+00   3.66629364825190E+00
# == err :  5.475E-15 = rco :  2.262E-02 = res :  1.093E-14 ==
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  3.97308366771298E-01  -1.92171595772051E+00
#  x2 : -2.38300160166170E+00  -1.65159813713614E+00
#  x3 : -1.17635241772214E+00   2.17748014148655E+00
#  x4 :  5.01732358554671E-01   1.22966143182879E+00
#  x5 :  1.19054568318791E+00   6.79107470333445E-01
#  x6 :  4.69767610869959E-01  -5.12934948792143E-01
#  x7 :  5.82616709754244E+00   6.36153845578694E+00
# == err :  4.728E-15 = rco :  5.913E-03 = res :  1.375E-14 ==
# solution 13 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -1.66666666666667E-01  -2.49628917422963E-52
#  x2 : -1.66666666666667E-01  -8.35238971903811E-53
#  x3 : -1.66666666666667E-01   5.01143383142287E-52
#  x4 : -1.66666666666667E-01   4.17619485951906E-52
#  x5 : -1.66666666666667E-01   5.84667280332668E-52
#  x6 : -1.66666666666667E-01   5.01143383142287E-52
#  x7 : -3.60000000000000E+01   6.41463530422127E-50
# == err :  6.281E-15 = rco :  2.220E-03 = res :  1.776E-15 ==
# solution 14 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.27399636935068E-01   3.71479583210633E+00
#  x2 : -6.84186578113855E+00   2.76209288924990E+00
#  x3 : -4.12071064151691E+00  -8.69959556204954E+00
#  x4 :  8.72511873116429E+00  -3.50394294906871E+00
#  x5 :  1.81846960499532E+00   5.23487937380794E+00
#  x6 : -1.40841155043922E+00   4.91770415954085E-01
#  x7 : -3.79717658548780E+00  -1.32584762480407E+00
# == err :  2.537E-14 = rco :  2.797E-03 = res :  3.553E-14 ==
# solution 15 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.24157573765506E-01  -1.92171595772051E+00
#  x2 : -1.48391565543637E+00  -7.29739304826774E-02
#  x3 : -5.60052371456211E-01   1.17922334308334E+00
#  x4 :  5.48670634671490E-01   9.30133142907219E-01
#  x5 :  7.08367213492227E-01   1.15818184498564E-01
#  x6 :  2.11087752494370E-01  -2.30484782285939E-01
#  x7 :  1.29659090383023E+01   1.41573572436994E+01
# == err :  3.463E-14 = rco :  1.367E-03 = res :  1.489E-14 ==
# solution 16 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  4.36024966562036E-01   1.92171595772051E+00
#  x2 : -8.16560321165552E-01   1.72600044478268E+00
#  x3 : -3.45095823467105E+00   8.67158668830608E-01
#  x4 : -5.62843410349120E-01  -4.13034050901962E+00
#  x5 :  2.42330915003328E+00  -1.44479097161200E+00
#  x6 :  9.71027849590408E-01   1.06025640929782E+00
#  x7 :  2.81860566521976E+00  -3.07760969275286E+00
# == err :  6.365E-15 = rco :  9.009E-03 = res :  2.512E-14 ==
# solution 17 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.51498181419768E+00   1.18504685467082E-62
#  x2 :  7.41222573661469E-01   1.21543267145725E-63
#  x3 : -1.07823548534975E+00   1.27620430503012E-62
#  x4 : -1.63478816493804E+00  -7.29259602874353E-63
#  x5 :  1.16304871296224E+00   1.09388940431153E-62
#  x6 : -1.70622945053361E+00  -1.52792069897868E-62
#  x7 : -3.51652586826676E+00   3.14903289293149E-62
# == err :  5.155E-15 = rco :  3.603E-02 = res :  3.553E-15 ==
# solution 18 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -6.81648480864347E-01  -3.03858167864314E-64
#  x2 :  9.58129145190158E-01   7.21663148677745E-64
#  x3 :  6.31940496052593E-01   7.59645419660784E-65
#  x4 : -4.34421392403970E-01  -4.55787251796470E-64
#  x5 : -8.87912123263308E-01  -1.80415787169436E-64
#  x6 : -5.86087644711127E-01   1.89911354915196E-64
#  x7 : -1.02373767032016E+01   8.07127252475485E-64
# == err :  2.856E-14 = rco :  1.259E-02 = res :  3.553E-15 ==
# solution 19 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.25749090709884E+00  -1.92171595772051E+00
#  x2 : -1.00903560786738E+00  -3.30462465143614E+00
#  x3 : -3.99271237385452E+00  -1.22680685072913E+00
#  x4 : -3.03453290093010E+00   3.65908289972281E+00
#  x5 :  3.61780513583611E+00   5.15362410077954E+00
#  x6 :  2.16098483971706E+00  -2.35955954061657E+00
#  x7 :  1.26652651496622E+00   1.38290869371563E+00
# == err :  9.623E-15 = rco :  1.153E-02 = res :  1.066E-14 ==
# solution 20 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.39817459672457E-01  -3.20475411419393E-65
#  x2 : -1.52476811214796E-01  -4.46083501852424E-65
#  x3 :  2.16348572029908E+00   1.85163571042316E-64
#  x4 : -9.41947644616723E-01  -6.64689742203186E-65
#  x5 : -9.04563389105643E-01  -6.88428661567585E-65
#  x6 : -1.30431533503437E+00   5.45995145381188E-65
#  x7 : -4.60011458796647E+00   9.49556774575980E-65
# == err :  4.665E-15 = rco :  6.219E-02 = res :  2.887E-15 ==
# solution 21 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  5.69908729836229E-01  -1.79307987438582E+00
#  x2 : -1.27629537390668E+00   1.77720268392520E-01
#  x3 :  2.62145169634245E+00   8.47407879790140E-01
#  x4 :  2.99663982501565E-01  -4.45879059994048E+00
#  x5 : -4.31844602385446E+00   2.65619747762992E+00
#  x6 :  1.10371698908089E+00   2.57054484851373E+00
#  x7 :  8.46204103742907E-01  -1.97080014277834E+00
# == err :  4.554E-15 = rco :  2.347E-02 = res :  6.169E-15 ==
# solution 22 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.00000000000000E+00  -6.68191177523049E-52
#  x2 :  1.00000000000000E+00  -1.78184314006146E-51
#  x3 :  1.00000000000000E+00   5.34552942018439E-51
#  x4 :  1.00000000000000E+00   5.34552942018439E-51
#  x5 :  1.00000000000000E+00   5.34552942018439E-51
#  x6 : -6.00000000000000E+00  -1.06910588403688E-50
#  x7 : -1.00000000000000E+00   6.68191177523049E-52
# == err :  4.815E-35 = rco :  4.762E-02 = res :  2.717E-50 ==
# solution 23 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.25749090709884E+00   1.92171595772051E+00
#  x2 : -1.00903560786738E+00   3.30462465143614E+00
#  x3 : -3.99271237385452E+00   1.22680685072913E+00
#  x4 : -3.03453290093010E+00  -3.65908289972281E+00
#  x5 :  3.61780513583611E+00  -5.15362410077954E+00
#  x6 :  2.16098483971706E+00   2.35955954061657E+00
#  x7 :  1.26652651496622E+00  -1.38290869371563E+00
# == err :  8.689E-15 = rco :  1.153E-02 = res :  1.422E-14 ==
# solution 24 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.08489054403391E+00   1.79307987438582E+00
#  x2 : -1.75656198282473E+00   7.45683258320039E-01
#  x3 : -2.96247415748368E-01  -2.32634270785216E+00
#  x4 :  1.04373156113984E+00   8.97470024637929E-01
#  x5 : -1.38967844523800E+00  -3.78900562083039E-01
#  x6 :  3.13865738637349E-01  -7.30989887408594E-01
#  x7 :  2.97569862064542E+00   6.93036968326386E+00
# == err :  4.402E-15 = rco :  8.877E-03 = res :  1.589E-14 ==
# solution 25 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.78534059463196E-01  -1.03311777073867E-62
#  x2 :  1.40399529688145E+00   1.94469227433161E-62
#  x3 : -4.24145311642535E-01   1.21543267145725E-62
#  x4 :  2.30307117019040E+00  -9.72346137165803E-63
#  x5 : -1.76538489062995E+00   5.34790375441192E-62
#  x6 : -2.69607032426256E+00  -6.32024989157772E-62
#  x7 : -2.22546123741826E+00  -3.88938454866321E-62
# == err :  4.397E-15 = rco :  7.427E-02 = res :  1.221E-15 ==
# solution 26 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  5.93369639826430E-03   3.71479583210633E+00
#  x2 : -6.29608516467496E+00  -2.89485362873527E-01
#  x3 :  6.85454641806506E-01  -6.41622249799802E+00
#  x4 :  4.94031447364827E+00   2.36147158475214E-01
#  x5 :  2.97245117069882E-01   2.53379026615599E+00
#  x6 : -6.32862764247966E-01   2.20974604134012E-01
#  x7 : -8.45046930263531E+00  -2.95062249572452E+00
# == err :  9.137E-15 = rco :  1.203E-03 = res :  8.527E-14 ==
# solution 27 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -2.51557210700575E-01  -1.79307987438582E+00
#  x2 : -5.18994747263476E-01   1.65067431386248E+00
#  x3 :  2.54014532482806E+00  -1.49593314929199E+00
#  x4 : -1.73247661935806E+00  -1.65911587822018E+00
#  x5 : -1.53306651761352E+00   2.14239297415820E+00
#  x6 :  4.95949770107570E-01   1.15506161387731E+00
#  x7 :  1.88319443182410E+00  -4.38593932445156E+00
# == err :  4.379E-15 = rco :  1.345E-02 = res :  8.894E-15 ==
# solution 28 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  4.36024966562036E-01  -1.92171595772051E+00
#  x2 : -8.16560321165553E-01  -1.72600044478268E+00
#  x3 : -3.45095823467105E+00  -8.67158668830608E-01
#  x4 : -5.62843410349120E-01   4.13034050901962E+00
#  x5 :  2.42330915003328E+00   1.44479097161200E+00
#  x6 :  9.71027849590408E-01  -1.06025640929782E+00
#  x7 :  2.81860566521976E+00   3.07760969275286E+00
# == err :  5.695E-15 = rco :  9.009E-03 = res :  1.962E-14 ==
# solution 29 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.27399636935069E-01  -1.28636083334685E-01
#  x2 :  4.97146350108961E-02  -2.19412989932238E+00
#  x3 :  1.55565100551911E-01  -1.21486906126276E+00
#  x4 : -2.47476413001305E+00   2.09079838123884E+00
#  x5 : -1.71288232676718E-01   8.69709294847598E-02
#  x6 :  6.13372990191896E-01   1.35986573319623E+00
#  x7 :  1.65369671656056E+00  -3.66629364825190E+00
# == err :  5.425E-15 = rco :  2.262E-02 = res :  1.084E-14 ==
# solution 30 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  5.69908729836229E-01   1.79307987438582E+00
#  x2 : -1.27629537390668E+00  -1.77720268392520E-01
#  x3 :  2.62145169634245E+00  -8.47407879790140E-01
#  x4 :  2.99663982501565E-01   4.45879059994048E+00
#  x5 : -4.31844602385446E+00  -2.65619747762992E+00
#  x6 :  1.10371698908089E+00  -2.57054484851373E+00
#  x7 :  8.46204103742907E-01   1.97080014277834E+00
# == err :  4.554E-15 = rco :  2.347E-02 = res :  6.169E-15 ==
# solution 31 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.24157573765507E-01   1.92171595772051E+00
#  x2 : -1.48391565543637E+00   7.29739304826770E-02
#  x3 : -5.60052371456211E-01  -1.17922334308334E+00
#  x4 :  5.48670634671490E-01  -9.30133142907219E-01
#  x5 :  7.08367213492227E-01  -1.15818184498564E-01
#  x6 :  2.11087752494370E-01   2.30484782285939E-01
#  x7 :  1.29659090383023E+01  -1.41573572436994E+01
# == err :  4.016E-14 = rco :  1.367E-03 = res :  1.676E-14 ==
# solution 32 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  6.54799273870137E-01  -1.51929083932157E-63
#  x2 : -8.54232602712374E-01   6.07716335728627E-64
#  x3 :  1.57319825015525E-01   0.00000000000000E+00
#  x4 : -5.20756185121201E-01   7.59645419660784E-64
#  x5 : -6.62201048157115E-02   3.03858167864314E-64
#  x6 : -3.70910206236376E-01   1.13946812949118E-64
#  x7 : -1.61764219455754E+01  -1.52687825261831E-62
# == err :  2.186E-14 = rco :  6.932E-03 = res :  1.776E-15 ==