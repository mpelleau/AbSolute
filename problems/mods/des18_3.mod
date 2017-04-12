var a10;
var a20;
var a21;
var a22;
var a30;
var a31;
var a32;
var a33;

subject to

cons1:   6*a33*a10*a20 + 10*a22*a10*a31 + 8*a32*a10*a21 - 162*a10^2*a21
+ 16*a21*a30 + 14*a31*a20 + 48*a10*a30 = 0;

cons2:  15*a33*a10*a21 - 162*a10^2*a22 - 312*a10*a20 + 24*a10*a30 + 27*a31*a21
+ 24*a32*a20 + 18*a22*a10*a32 + 30*a22*a30 + 84*a31*a10 = 0; 

cons3: -240*a10 + 420*a33 - 64*a22 + 112*a32 = 0;

cons4:  180*a33*a10 - 284*a22*a10 - 162*a10^2 + 60*a22*a32 + 50*a32*a10 + 70*a30
+ 55*a33*a21 + 260*a31 - 112*a20 = 0;

cons5:  66*a33*a10 + 336*a32 + 90*a31 + 78*a22*a33 - 1056*a10 - 90*a21 = 0;

cons6: 136*a33 - 136 = 0;

cons7:  4*a22*a10*a30 + 2*a32*a10*a20 + 6*a20*a30 - 162*a10^2*a20 + 3*a31*a21*a10 = 0;

cons8:  28*a22*a10*a33 + 192*a30 + 128*a32*a10 + 36*a31*a20 + 36*a33*a20 
- 300*a10*a21 + 40*a32*a21 - 648*a10^2 + 44*a22*a31 = 0;

#solve;
#display a10, a20, a21, a22, a30, a31, a32, a33;

# TITLE : a "dessin d'enfant", called des18_3

# ROOT COUNTS :

# total degree : 324
# 2-homogeneous Bezout number : 108
#   with partition : {a33 }{a10 a20 a22 a31 a32 a21 a30 }
# mixed volume : 46

# REFERENCES :

# Raphael Nauheim:
#  "Systems of Algebraic Equations with Bad Reduction"
# Universitaet Heidelberg, Interdisziplinaeres Zentrum
# fuer wissenschaftliches Rechnen, Preprint 95-46, Dezember 1995.

# Birch, B: "Noncongruence Subgroups, Covers and Drawings",
# In "The Grothendieck Theory of Dessins d'Enfants",
# editor: Schneps, L., London Mathematical Society Lecture Series 200,
# Cambridge University Press, pages 25-46, 1994.

# There are six real and forty complex conjugated solutions.

# THE SOLUTIONS :

# 46 8
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   3.22092406769751E-31
#  a10 : -5.54426674204252E-01  -3.53198380037265E-01
#  a20 : -4.17822975619532E-01  -3.53594895643123E-01
#  a22 :  5.52498520158377E+00   6.50715246552986E-01
#  a31 : -1.61440960737313E+00  -2.43233070643676E+00
#  a32 : -1.78092275810410E+00  -3.85016387763862E-01
#  a21 :  2.62382601999093E+00   5.79410173333995E-01
#  a30 :  1.23028437311259E+00   2.66195765643575E+00
# == err :  1.812E-14 = rco :  8.638E-04 = res :  2.079E-13 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -3.09120151102865E-29
#  a10 : -1.74157771399634E+00   3.40355275593061E+00
#  a20 :  4.91348169799353E+01   3.96086942250424E+01
#  a22 : -1.66417016057400E+01  -1.57081029697567E+01
#  a31 :  6.84488775932716E+01  -6.34639241522368E-01
#  a32 : -1.69914960189864E+01  -1.68273150572399E+00
#  a21 :  9.74850591804066E+00  -5.79696064185844E+01
#  a30 : -1.09041583991505E+02  -2.57282006833764E+02
# == err :  5.640E-13 = rco :  2.343E-06 = res :  3.597E-11 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -4.50777566960056E-31
#  a10 : -2.63884000934243E-01   5.36119882957316E-02
#  a20 : -4.05589651447332E+00  -3.00221834077863E+00
#  a22 :  4.10204355848378E+00  -9.73991786392433E-01
#  a31 : -8.27725842520557E-01  -4.42434138731594E-02
#  a32 : -1.97144082572550E+00  -4.41683903019108E-01
#  a21 : -1.72994316426648E+00  -3.12705473793765E+00
#  a30 :  1.50931473222725E+00  -5.51212501552006E-01
# == err :  7.457E-15 = rco :  1.272E-03 = res :  8.527E-14 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   7.94150952558633E-31
#  a10 : -3.74218849336509E-02   1.74522270949688E-01
#  a20 :  3.55361604057689E-01  -1.66112057309503E+00
#  a22 :  5.30429944044650E+00  -1.52312637107046E+00
#  a31 :  9.27223890018023E-01   1.43552628981904E+00
#  a32 : -7.99161501745538E-01  -4.96381631433789E-01
#  a21 :  2.95238786615847E+00  -3.65741963624141E+00
#  a30 : -6.94785461561970E-01  -3.08200024225707E-01
# == err :  6.911E-15 = rco :  9.817E-04 = res :  2.842E-14 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.93065581370859E-29
#  a10 :  3.66697071175811E-01  -1.26968351968060E+00
#  a20 :  1.41090490214364E+02   2.19984715394215E+01
#  a22 : -2.55146395389989E+01  -2.14876391175427E+00
#  a31 : -1.98355442765332E+01   1.10713187298381E+01
#  a32 : -1.75440145840512E+01  -3.94861549174659E+00
#  a21 : -1.11479554107057E+02   8.43407755361708E+00
#  a30 : -3.46132825573911E+01  -1.67491797617033E+01
# == err :  4.377E-13 = rco :  2.921E-06 = res :  3.094E-11 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -1.02140693168661E-30
#  a10 :  1.11742669633946E-03  -1.84690464419679E+00
#  a20 : -1.88025938222224E+00  -1.08579763561905E+00
#  a22 :  4.10461992958911E+00  -7.52649644065811E+00
#  a31 :  5.47316919455685E+00   7.25530993529782E-01
#  a32 : -1.40210841159978E+00  -8.25850791794061E+00
#  a21 :  3.78367670323515E+00  -1.63132443958542E+01
#  a30 : -2.14790328348906E+01   5.01362779788523E-01
# == err :  4.105E-14 = rco :  5.878E-05 = res :  2.874E-12 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -1.84457951941295E-30
#  a10 : -1.07747633122764E+00  -2.05266250102671E+00
#  a20 : -5.74565759407296E-01  -5.45366422079856E-01
#  a22 :  3.03275895426104E+00  -1.49684623267144E+00
#  a31 :  3.87893930977835E-01  -4.86999205958773E+00
#  a32 : -4.32587273591006E+00  -5.25390320658377E+00
#  a21 : -1.28140021255613E+00  -3.20254325452192E+00
#  a30 : -9.01029523452842E+00   1.42556192187152E+01
# == err :  9.146E-14 = rco :  1.180E-04 = res :  4.590E-13 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -7.56393327989766E-31
#  a10 : -5.54426674204252E-01   3.53198380037266E-01
#  a20 : -4.17822975619532E-01   3.53594895643124E-01
#  a22 :  5.52498520158377E+00  -6.50715246552987E-01
#  a31 : -1.61440960737313E+00   2.43233070643676E+00
#  a32 : -1.78092275810410E+00   3.85016387763862E-01
#  a21 :  2.62382601999093E+00  -5.79410173333995E-01
#  a30 :  1.23028437311259E+00  -2.66195765643575E+00
# == err :  1.995E-14 = rco :  8.638E-04 = res :  4.143E-13 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   5.94761105584188E-30
#  a10 : -8.41608786378188E+00  -5.98709263304501E+00
#  a20 : -1.52078998501707E+01   1.01772043510331E+01
#  a22 :  4.18459490973777E+00  -1.87533188950896E+01
#  a31 : -4.09877429653650E+01   2.01786262301772E+01
#  a32 : -1.93932769025396E+01  -2.35456664394333E+01
#  a21 : -1.71856946448060E+01  -1.81200525559564E+01
#  a30 :  7.77560113694935E+01   2.91123025982788E+02
# == err :  7.474E-13 = rco :  6.125E-06 = res :  7.745E-11 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -3.13431796291135E-32
#  a10 : -1.61591655372893E-02   3.79934253110678E-02
#  a20 :  7.73157603400808E-01  -3.97531297270607E-02
#  a22 :  5.82415463252854E+00  -1.09283247748605E-02
#  a31 :  9.59293405718147E-02   1.13653777604448E-01
#  a32 : -4.56538421849311E-01   7.51697257952250E-02
#  a21 :  3.61687073476930E+00  -3.31114726533374E-02
#  a30 : -2.72486634454952E-02  -2.80676116099737E-02
# == err :  3.517E-15 = rco :  1.020E-04 = res :  5.689E-14 ==
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.93719976393706E-55
#  a10 :  1.11518069895478E+02   3.26265223399926E-55
#  a20 :  1.90866270035488E+02  -4.43720703823900E-53
#  a22 :  4.51548424377688E+01  -3.42578484569923E-54
#  a31 :  2.84394247689901E+02   1.36868261216269E-52
#  a32 :  2.61020059740463E+02  -6.52530446799852E-55
#  a21 :  7.13045653167747E+01  -4.07831529249908E-54
#  a30 :  1.72231426440665E+04   1.83752573818838E-51
# == err :  4.997E-11 = rco :  1.679E-09 = res :  4.470E-08 ==
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   5.91028432977675E-32
#  a10 :  2.90964894603069E-01  -4.19102380684238E-01
#  a20 : -9.84531343849711E+00  -5.09872418732703E+00
#  a22 :  7.26664757504249E+00  -3.74410104261106E+00
#  a31 : -2.99791577750170E+00   2.61772984004309E+00
#  a32 :  1.02586624560228E+00  -3.03756284010111E+00
#  a21 :  3.92913226381656E+00  -7.35726614573737E+00
#  a30 : -2.39677577873105E+00  -5.21247190296407E+00
# == err :  1.083E-14 = rco :  4.219E-04 = res :  4.778E-13 ==
# solution 13 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   2.31024066709371E-31
#  a10 : -1.78899091686156E-01  -1.84622565376735E-01
#  a20 : -1.33334913544351E-01  -1.38797226480544E-01
#  a22 :  5.17933182447055E+00  -6.05097237155145E-02
#  a31 : -1.00238036533596E-02  -5.60236409002329E-01
#  a32 : -1.17373701105859E+00  -4.30196767930441E-01
#  a21 :  2.06466894481676E+00  -1.87897884018666E-01
#  a30 : -7.93805092581808E-03   4.37641882841095E-01
# == err :  2.550E-15 = rco :  2.366E-04 = res :  1.157E-13 ==
# solution 14 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -7.52525580216640E-30
#  a10 : -5.57356465955718E+00   4.55750871639050E+00
#  a20 : -3.43014209413819E-01  -1.67390178823015E+00
#  a22 : -2.51488849973176E+00   3.25346216025777E+00
#  a31 :  1.15350837630467E-02   1.09027284234543E+01
#  a32 : -1.71304319846121E+01   1.16252113409841E+01
#  a21 : -4.81243643676069E+00   6.99058875505608E+00
#  a30 :  3.19307939020137E+00  -1.23152196675656E+02
# == err :  6.497E-13 = rco :  1.388E-05 = res :  5.543E-12 ==
# solution 15 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   9.62494721489119E-31
#  a10 : -1.64073255722099E+00   2.64286316731288E+00
#  a20 :  7.43568915776749E-01  -2.15763149273222E+00
#  a22 : -5.48697725277948E+00   2.20226322279694E+00
#  a31 :  2.56168390011989E+00   7.46340458802628E+00
#  a32 : -1.04012710527761E+01   6.92171434298299E+00
#  a21 : -2.29770501865556E+01   6.14160475447846E+00
#  a30 : -1.80518157990855E+01  -5.64245167866385E+01
# == err :  1.480E-13 = rco :  1.612E-05 = res :  5.682E-12 ==
# solution 16 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.54203888499115E-29
#  a10 : -3.63312191610396E+00  -1.29558498888172E-01
#  a20 :  2.95654921086464E+00   1.53471148917313E+00
#  a22 : -7.17676970868325E+00  -2.90513979191750E+00
#  a31 : -8.86955569466596E+00  -1.40245694108306E+00
#  a32 : -1.56362725108989E+01  -1.93770523585608E+00
#  a21 : -3.35004990724038E+01  -9.72920082017105E+00
#  a30 :  7.63467865550755E+01   7.08373623877234E+00
# == err :  6.216E-13 = rco :  3.887E-05 = res :  1.878E-11 ==
# solution 17 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.72058919755877E-31
#  a10 :  1.13579649773337E+00  -1.69153563677396E-01
#  a20 : -2.21892610494372E+01  -1.67242306102555E+01
#  a22 :  2.10445774895358E+01   9.91262922499520E+00
#  a31 : -3.53190240984121E+01  -4.39810366491285E+01
#  a32 :  1.07093224891634E+01   5.30188763497427E+00
#  a21 :  1.04073188769951E+01  -1.37356882831107E+01
#  a30 :  3.38235457345909E+01  -1.14629299201937E+01
# == err :  6.900E-14 = rco :  4.336E-05 = res :  9.205E-12 ==
# solution 18 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   7.43062589678967E-30
#  a10 : -8.41608786378189E+00   5.98709263304502E+00
#  a20 : -1.52078998501707E+01  -1.01772043510331E+01
#  a22 :  4.18459490973776E+00   1.87533188950896E+01
#  a31 : -4.09877429653650E+01  -2.01786262301772E+01
#  a32 : -1.93932769025396E+01   2.35456664394334E+01
#  a21 : -1.71856946448061E+01   1.81200525559564E+01
#  a30 :  7.77560113694937E+01  -2.91123025982788E+02
# == err :  4.393E-13 = rco :  6.125E-06 = res :  1.163E-10 ==
# solution 19 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -1.05338616588948E-29
#  a10 :  3.66697071175811E-01   1.26968351968060E+00
#  a20 :  1.41090490214364E+02  -2.19984715394215E+01
#  a22 : -2.55146395389989E+01   2.14876391175427E+00
#  a31 : -1.98355442765332E+01  -1.10713187298381E+01
#  a32 : -1.75440145840512E+01   3.94861549174659E+00
#  a21 : -1.11479554107057E+02  -8.43407755361709E+00
#  a30 : -3.46132825573911E+01   1.67491797617033E+01
# == err :  4.476E-13 = rco :  2.921E-06 = res :  3.820E-11 ==
# solution 20 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -2.91703841149741E-62
#  a10 : -6.96812563416184E-02  -1.62867977975272E-61
#  a20 :  1.46149368079807E+00   9.02337215289866E-60
#  a22 :  6.21012535918065E+00   4.20053531255627E-60
#  a31 : -2.87638109546105E-01  -3.36431763459368E-60
#  a32 : -3.50673915485953E-01   2.48920611114446E-60
#  a21 :  4.55178173702070E+00   1.13570028820966E-59
#  a30 :  1.14303650490072E-01   1.08902767362570E-60
# == err :  3.398E-15 = rco :  7.703E-05 = res :  5.684E-14 ==
# solution 21 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   3.94946717491797E-31
#  a10 :  1.13579649773337E+00   1.69153563677396E-01
#  a20 : -2.21892610494372E+01   1.67242306102555E+01
#  a22 :  2.10445774895358E+01  -9.91262922499520E+00
#  a31 : -3.53190240984121E+01   4.39810366491285E+01
#  a32 :  1.07093224891634E+01  -5.30188763497427E+00
#  a21 :  1.04073188769951E+01   1.37356882831107E+01
#  a30 :  3.38235457345908E+01   1.14629299201937E+01
# == err :  6.245E-14 = rco :  4.336E-05 = res :  3.311E-12 ==
# solution 22 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   7.60042448718532E-31
#  a10 : -3.74218849336510E-02  -1.74522270949689E-01
#  a20 :  3.55361604057689E-01   1.66112057309503E+00
#  a22 :  5.30429944044650E+00   1.52312637107046E+00
#  a31 :  9.27223890018022E-01  -1.43552628981904E+00
#  a32 : -7.99161501745538E-01   4.96381631433789E-01
#  a21 :  2.95238786615848E+00   3.65741963624142E+00
#  a30 : -6.94785461561970E-01   3.08200024225708E-01
# == err :  6.465E-15 = rco :  9.817E-04 = res :  8.527E-14 ==
# solution 23 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   6.07716335728627E-63
#  a10 : -1.43946955307287E+00   6.27163258471943E-61
#  a20 : -5.15200331458865E-01   2.11485284833562E-61
#  a22 :  3.20357759910987E+00   7.77876909732643E-61
#  a31 : -2.04515751033731E+00   1.45851920574871E-60
#  a32 : -5.00396184280766E+00   1.76237737361302E-60
#  a21 : -2.11601605378910E+00   1.84745766061503E-60
#  a30 :  6.81756372345637E+00  -6.22301527786114E-60
# == err :  3.169E-14 = rco :  9.072E-04 = res :  3.979E-13 ==
# solution 24 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.51929083932157E-63
#  a10 :  2.39059161735694E+01  -8.69034360091937E-61
#  a20 :  9.20967834736090E+01  -6.05771643454296E-60
#  a22 :  4.44669593854581E+01  -1.34730711631037E-60
#  a31 : -1.17569336988949E+01  -2.69826053063510E-61
#  a32 :  7.28866543064819E+01  -2.63262716637641E-60
#  a21 :  3.59261959367712E+01  -1.24946478625806E-60
#  a30 :  1.71413050540115E+03  -1.16992687223789E-58
# == err :  2.029E-11 = rco :  9.983E-08 = res :  2.910E-09 ==
# solution 25 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   3.99118544112854E-29
#  a10 : -5.13505136216113E+00   1.40762912240562E+01
#  a20 : -1.16758861628740E+02  -1.35074247707284E+02
#  a22 :  5.23996846257515E+01   1.05824592417430E+02
#  a31 : -4.68410100351749E+01  -6.81764137435109E+02
#  a32 :  1.51889954386556E+01   9.06346768615092E+01
#  a21 :  1.11763197928563E+02  -4.06519233854987E+02
#  a30 :  8.85736286156920E+02  -2.21618652632072E+03
# == err :  3.916E-12 = rco :  2.949E-08 = res :  3.952E-09 ==
# solution 26 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   2.25342923227654E-29
#  a10 : -1.74157771399634E+00  -3.40355275593060E+00
#  a20 :  4.91348169799353E+01  -3.96086942250424E+01
#  a22 : -1.66417016057400E+01   1.57081029697567E+01
#  a31 :  6.84488775932716E+01   6.34639241522329E-01
#  a32 : -1.69914960189864E+01   1.68273150572399E+00
#  a21 :  9.74850591804068E+00   5.79696064185844E+01
#  a30 : -1.09041583991505E+02   2.57282006833763E+02
# == err :  4.621E-13 = rco :  2.343E-06 = res :  5.866E-11 ==
# solution 27 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -2.31082359616357E-30
#  a10 : -1.64073255722099E+00  -2.64286316731288E+00
#  a20 :  7.43568915776753E-01   2.15763149273222E+00
#  a22 : -5.48697725277949E+00  -2.20226322279694E+00
#  a31 :  2.56168390011988E+00  -7.46340458802628E+00
#  a32 : -1.04012710527761E+01  -6.92171434298298E+00
#  a21 : -2.29770501865556E+01  -6.14160475447845E+00
#  a30 : -1.80518157990854E+01   5.64245167866385E+01
# == err :  1.039E-13 = rco :  1.612E-05 = res :  2.917E-12 ==
# solution 28 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -3.37100717296503E-30
#  a10 : -5.57356465955717E+00  -4.55750871639049E+00
#  a20 : -3.43014209413809E-01   1.67390178823013E+00
#  a22 : -2.51488849973175E+00  -3.25346216025775E+00
#  a31 :  1.15350837630596E-02  -1.09027284234543E+01
#  a32 : -1.71304319846121E+01  -1.16252113409840E+01
#  a21 : -4.81243643676069E+00  -6.99058875505606E+00
#  a30 :  3.19307939020158E+00   1.23152196675655E+02
# == err :  3.181E-13 = rco :  1.388E-05 = res :  1.490E-11 ==
# solution 29 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.69041439750991E-31
#  a10 : -6.67245758085761E-02  -2.34822140829434E-01
#  a20 : -3.90543112888623E+00  -1.27700478508603E+00
#  a22 :  2.76338858003241E+00  -3.84609078364775E-01
#  a31 :  8.48762422588007E-01  -8.38568729661347E-01
#  a32 : -2.31390204528557E+00  -7.22966917985802E-01
#  a21 : -4.66089810988903E+00  -1.28792954226737E+00
#  a30 : -1.06926302914714E+00   7.58242694786569E-01
# == err :  6.926E-15 = rco :  9.580E-04 = res :  1.422E-13 ==
# solution 30 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   3.28972407030479E-30
#  a10 : -1.07747633122763E+00   2.05266250102670E+00
#  a20 : -5.74565759407294E-01   5.45366422079856E-01
#  a22 :  3.03275895426105E+00   1.49684623267143E+00
#  a31 :  3.87893930977836E-01   4.86999205958770E+00
#  a32 : -4.32587273591004E+00   5.25390320658374E+00
#  a21 : -1.28140021255612E+00   3.20254325452191E+00
#  a30 : -9.01029523452835E+00  -1.42556192187151E+01
# == err :  1.704E-13 = rco :  1.180E-04 = res :  7.390E-13 ==
# solution 31 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   4.28073850696345E-31
#  a10 : -6.01573949972425E-02   1.72715984551965E-01
#  a20 : -6.99151674885414E-01   1.81019877076219E+00
#  a22 :  4.53222562187609E+00   1.15753842803441E+00
#  a31 :  8.62781580143381E-01   1.97937798273999E-01
#  a32 : -1.28906549106490E+00   1.03155621148816E+00
#  a21 :  6.39930630763385E-01   3.15240512872134E+00
#  a30 : -5.46254839168637E-01  -4.39640734216265E-01
# == err :  5.213E-15 = rco :  1.142E-03 = res :  9.871E-14 ==
# solution 32 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -8.70348283971388E-29
#  a10 : -5.13505136216113E+00  -1.40762912240562E+01
#  a20 : -1.16758861628740E+02   1.35074247707284E+02
#  a22 :  5.23996846257515E+01  -1.05824592417430E+02
#  a31 : -4.68410100351747E+01   6.81764137435109E+02
#  a32 :  1.51889954386556E+01  -9.06346768615092E+01
#  a21 :  1.11763197928563E+02   4.06519233854987E+02
#  a30 :  8.85736286156921E+02   2.21618652632072E+03
# == err :  5.097E-12 = rco :  2.949E-08 = res :  3.145E-09 ==
# solution 33 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -2.51664194801923E-31
#  a10 :  2.90964894603069E-01   4.19102380684238E-01
#  a20 : -9.84531343849711E+00   5.09872418732703E+00
#  a22 :  7.26664757504249E+00   3.74410104261106E+00
#  a31 : -2.99791577750170E+00  -2.61772984004309E+00
#  a32 :  1.02586624560228E+00   3.03756284010111E+00
#  a21 :  3.92913226381657E+00   7.35726614573737E+00
#  a30 : -2.39677577873106E+00   5.21247190296407E+00
# == err :  6.076E-15 = rco :  4.219E-04 = res :  6.653E-13 ==
# solution 34 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -5.57582168896358E-58
#  a10 :  3.78205552886912E+00  -9.55855146679472E-58
#  a20 :  1.07299037764053E+02  -3.68800777427163E-56
#  a22 :  1.01363081259447E+01  -1.73248745335654E-57
#  a31 : -1.51879803754348E+01   1.27447352890596E-57
#  a32 :  1.01465807766879E+01  -2.07101948447219E-57
#  a21 : -1.01252225842081E+01   4.77927573339736E-58
#  a30 :  2.99392733328254E+02  -1.17251564659348E-55
# == err :  4.550E-14 = rco :  6.116E-06 = res :  4.161E-11 ==
# solution 35 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   8.63586338217863E-32
#  a10 : -1.61591655372893E-02  -3.79934253110678E-02
#  a20 :  7.73157603400807E-01   3.97531297270605E-02
#  a22 :  5.82415463252854E+00   1.09283247748603E-02
#  a31 :  9.59293405718148E-02  -1.13653777604447E-01
#  a32 : -4.56538421849311E-01  -7.51697257952251E-02
#  a21 :  3.61687073476930E+00   3.31114726533369E-02
#  a30 : -2.72486634454952E-02   2.80676116099737E-02
# == err :  3.347E-15 = rco :  1.020E-04 = res :  5.702E-14 ==
# solution 36 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.14616071604603E-28
#  a10 :  4.59970428869386E+00  -1.18879900199898E+01
#  a20 :  1.43691837368076E+01  -6.38540307264143E+01
#  a22 :  3.14935789575631E+01  -7.61210298749095E+00
#  a31 :  1.21637840303081E+02   2.59447637493818E+01
#  a32 :  2.41028400229515E+01  -2.98240374642586E+01
#  a21 :  1.88319464309689E+02   3.87724248468782E+01
#  a30 : -9.28113792501200E+02  -8.47551516425481E+02
# == err :  2.438E-12 = rco :  4.150E-07 = res :  3.905E-10 ==
# solution 37 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -3.34516610159445E-30
#  a10 : -5.81886740236537E-01  -1.03148939531588E+00
#  a20 :  6.20483592664957E+00  -6.16790153067882E+00
#  a22 :  4.08648154459807E+00   1.98599217576612E+00
#  a31 :  7.14821620614031E+00  -1.12791885597748E+01
#  a32 : -2.66176784645083E+00  -1.07548174666767E+00
#  a21 :  7.15332106064413E+00  -2.22674384652886E+00
#  a30 : -1.65832042225268E+01   2.33055668615934E+01
# == err :  4.603E-14 = rco :  1.906E-04 = res :  1.849E-12 ==
# solution 38 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -6.12731154525910E-30
#  a10 : -5.81886740236537E-01   1.03148939531588E+00
#  a20 :  6.20483592664957E+00   6.16790153067882E+00
#  a22 :  4.08648154459807E+00  -1.98599217576612E+00
#  a31 :  7.14821620614031E+00   1.12791885597748E+01
#  a32 : -2.66176784645083E+00   1.07548174666767E+00
#  a21 :  7.15332106064413E+00   2.22674384652887E+00
#  a30 : -1.65832042225268E+01  -2.33055668615934E+01
# == err :  4.432E-14 = rco :  1.906E-04 = res :  1.025E-12 ==
# solution 39 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -4.10843000056870E-29
#  a10 :  4.59970428869387E+00   1.18879900199897E+01
#  a20 :  1.43691837368076E+01   6.38540307264143E+01
#  a22 :  3.14935789575631E+01   7.61210298749093E+00
#  a31 :  1.21637840303081E+02  -2.59447637493819E+01
#  a32 :  2.41028400229515E+01   2.98240374642586E+01
#  a21 :  1.88319464309689E+02  -3.87724248468783E+01
#  a30 : -9.28113792501200E+02   8.47551516425481E+02
# == err :  2.395E-12 = rco :  4.150E-07 = res :  2.064E-09 ==
# solution 40 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -2.98079050533065E-31
#  a10 :  1.11742669633835E-03   1.84690464419679E+00
#  a20 : -1.88025938222225E+00   1.08579763561904E+00
#  a22 :  4.10461992958910E+00   7.52649644065811E+00
#  a31 :  5.47316919455685E+00  -7.25530993529774E-01
#  a32 : -1.40210841159979E+00   8.25850791794062E+00
#  a21 :  3.78367670323514E+00   1.63132443958542E+01
#  a30 : -2.14790328348906E+01  -5.01362779788552E-01
# == err :  7.428E-15 = rco :  5.878E-05 = res :  2.379E-12 ==
# solution 41 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -3.38082657347544E-32
#  a10 : -1.78899091686156E-01   1.84622565376735E-01
#  a20 : -1.33334913544351E-01   1.38797226480544E-01
#  a22 :  5.17933182447055E+00   6.05097237155139E-02
#  a31 : -1.00238036533603E-02   5.60236409002329E-01
#  a32 : -1.17373701105859E+00   4.30196767930440E-01
#  a21 :  2.06466894481676E+00   1.87897884018665E-01
#  a30 : -7.93805092581755E-03  -4.37641882841095E-01
# == err :  2.341E-15 = rco :  2.366E-04 = res :  1.798E-13 ==
# solution 42 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   1.50259219921837E-32
#  a10 : -2.63884000934243E-01  -5.36119882957316E-02
#  a20 : -4.05589651447332E+00   3.00221834077863E+00
#  a22 :  4.10204355848378E+00   9.73991786392433E-01
#  a31 : -8.27725842520558E-01   4.42434138731594E-02
#  a32 : -1.97144082572550E+00   4.41683903019108E-01
#  a21 : -1.72994316426648E+00   3.12705473793765E+00
#  a30 :  1.50931473222725E+00   5.51212501552007E-01
# == err :  6.154E-15 = rco :  1.272E-03 = res :  1.608E-13 ==
# solution 43 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   3.98272977783113E-59
#  a10 :  3.66215698318787E-01  -4.72949161117447E-59
#  a20 :  3.02178218554036E+01  -8.92131470234173E-57
#  a22 :  1.33734293432174E+01  -2.86756544003841E-57
#  a31 : -2.20730390859508E+00   8.36373253344538E-58
#  a32 :  4.67670754966447E+00  -1.67274650668907E-57
#  a21 :  2.28143370261007E+01  -8.28407793788875E-57
#  a30 :  3.02814098281021E+00  -1.03550974223609E-57
# == err :  3.000E-14 = rco :  8.879E-05 = res :  4.547E-13 ==
# solution 44 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00   6.46219597323654E-30
#  a10 : -3.63312191610397E+00   1.29558498888167E-01
#  a20 :  2.95654921086467E+00  -1.53471148917314E+00
#  a22 : -7.17676970868328E+00   2.90513979191751E+00
#  a31 : -8.86955569466602E+00   1.40245694108304E+00
#  a32 : -1.56362725108990E+01   1.93770523585608E+00
#  a21 : -3.35004990724040E+01   9.72920082017106E+00
#  a30 :  7.63467865550760E+01  -7.08373623877218E+00
# == err :  2.216E-13 = rco :  3.887E-05 = res :  1.985E-11 ==
# solution 45 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -4.50783247852683E-32
#  a10 : -6.67245758085762E-02   2.34822140829434E-01
#  a20 : -3.90543112888623E+00   1.27700478508603E+00
#  a22 :  2.76338858003241E+00   3.84609078364775E-01
#  a31 :  8.48762422588007E-01   8.38568729661347E-01
#  a32 : -2.31390204528557E+00   7.22966917985802E-01
#  a21 : -4.66089810988903E+00   1.28792954226737E+00
#  a30 : -1.06926302914714E+00  -7.58242694786570E-01
# == err :  6.627E-15 = rco :  9.580E-04 = res :  1.310E-13 ==
# solution 46 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a33 :  1.00000000000000E+00  -2.75773772029008E-31
#  a10 : -6.01573949972424E-02  -1.72715984551965E-01
#  a20 : -6.99151674885412E-01  -1.81019877076219E+00
#  a22 :  4.53222562187609E+00  -1.15753842803441E+00
#  a31 :  8.62781580143381E-01  -1.97937798273998E-01
#  a32 : -1.28906549106489E+00  -1.03155621148816E+00
#  a21 :  6.39930630763387E-01  -3.15240512872134E+00
#  a30 : -5.46254839168637E-01   4.39640734216265E-01
# == err :  4.931E-15 = rco :  1.142E-03 = res :  1.137E-13 ==
