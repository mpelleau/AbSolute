var x1;
var x10;
var x2;
var x3;
var x4;
var x5;
var x6;
var x7;
var x8;
var x9;

subject to

cons1:  x2 + 2*x6 + x9 + 2*x10 - 1.0E-5 = 0;
cons2:  x3 + x8 - 3.0E-5 = 0;
cons3:  x1 + x3 + 2*x5 + 2*x8 + x9 + x10 - 5.0E-5 = 0;
cons4:  x4 + 2*x7 - 1.0E-5 = 0;
cons5:  0.5140437E-7 * x5 - x1^2 = 0;
cons6:  0.1006932E-6 * x6 - x2^2 = 0;
cons7:  0.7816278E-15 * x7 - x4^2 = 0;
cons8:  0.1496236E-6 * x8 - x1*x3 = 0;
cons9:  0.6194411E-7 * x9 - x1*x2 = 0;
cons10:  0.2089296E-14 * x10 - x1*x2^2 = 0;

#solve;
#display x1, x10, x2, x3, x4, x5, x6, x7, x8, x9;

# TITLE : Model A combustion chemistry example for a temparature of 3000 degrees

# ROOT COUNTS :

# total degree : 96

# 5-homogeneous Bezout number : 44
#   with partition : {x2 x6 x9 x10 }{x3 x8 }{x1 }{x5 }{x4 x7 }

# mixed volume : 16

# REFERENCES :

# A.P. Morgan:
# `Solving Polynomial Systems Using Continuation for Engineering and
#  Scientific Problems', Prentice-Hall, Englewood Cliffs, N.J., 1987.
# Chapter 9.

# NOTE :

# The system should be scaled properly.  See the file comb3000s.

# THE SOLUTIONS :

# 16 10
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 :  2.44157718332959E-07  -2.69135029358109E-68
#  x6 :  5.92025990052519E-07  -1.39548002247951E-67
#  x9 :  5.53814405529183E-07   1.66914276780934E-67
#  x10 :  4.00898794801641E-06   6.95476153253892E-68
#  x3 :  1.54714076656939E-05   9.27301537671856E-67
#  x8 :  1.45285923343061E-05  -9.08755506918418E-67
#  x1 :  1.40505656302463E-07   5.30735744001309E-68
#  x5 :  3.84049827922917E-07   2.90136321227487E-67
#  x4 :  6.25149147668167E-11  -8.25298368527951E-67
#  x7 :  4.99996874254262E-06   4.12163150278652E-67
# == err :  4.098E-19 = rco :  1.302E-15 = res :  2.753E-21 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -8.91664127240689E-08  -9.91828751827260E-08
#  x6 : -1.87360573646801E-08   1.75657962676780E-07
#  x9 : -7.91642740880272E-07   1.14477345706777E-06
#  x10 :  5.45914063416685E-06  -6.98453253619300E-07
#  x3 :  4.73669630616887E-10   7.13749004225551E-06
#  x8 :  2.99995263303694E-05  -7.13749004225551E-06
#  x1 : -1.49581864476755E-07  -6.28891662707340E-07
#  x5 : -7.25872117958961E-06   3.66003075075719E-06
#  x4 :  6.25149147668167E-11  -1.65059673705590E-66
#  x7 :  4.99996874254262E-06   8.24326300557305E-67
# == err :  8.070E-19 = rco :  1.843E-15 = res :  1.388E-20 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 :  2.44157718332959E-07   6.52530446799852E-55
#  x6 :  5.92025990052519E-07   4.24144790419904E-54
#  x9 :  5.53814405529183E-07  -3.75205006909915E-54
#  x10 :  4.00898794801641E-06  -2.61012178719941E-54
#  x3 :  1.54714076656939E-05  -2.54486874251943E-53
#  x8 :  1.45285923343061E-05   2.54486874251943E-53
#  x1 :  1.40505656302463E-07  -8.97229364349797E-55
#  x5 :  3.84049827922917E-07  -8.93151049057298E-54
#  x4 : -6.25153055807167E-11   1.14702617601537E-55
#  x7 :  5.00003125765279E-06  -5.70326904185418E-56
# == err :  4.098E-19 = rco :  1.302E-15 = res :  2.753E-21 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -8.91664127240689E-08  -9.91828751827260E-08
#  x6 : -1.87360573646801E-08   1.75657962676780E-07
#  x9 : -7.91642740880272E-07   1.14477345706777E-06
#  x10 :  5.45914063416685E-06  -6.98453253619300E-07
#  x3 :  4.73669630616975E-10   7.13749004225551E-06
#  x8 :  2.99995263303694E-05  -7.13749004225551E-06
#  x1 : -1.49581864476755E-07  -6.28891662707340E-07
#  x5 : -7.25872117958961E-06   3.66003075075719E-06
#  x4 : -6.25153055807167E-11  -4.63650768835928E-69
#  x7 :  5.00003125765279E-06   2.31825384417964E-69
# == err :  8.068E-19 = rco :  1.843E-15 = res :  1.395E-20 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 :  7.04569390653156E-08  -9.69102131998154E-08
#  x6 : -4.39692964369002E-08  -1.35619823110719E-07
#  x9 :  8.31555564214474E-07   9.93970971497191E-07
#  x10 :  4.59296304479701E-06  -3.12910556037969E-07
#  x3 : -1.40318385913825E-07  -6.90383625579641E-06
#  x8 :  3.01403183859138E-05   6.90383625579641E-06
#  x1 : -1.62832772240636E-07   6.49907434653423E-07
#  x5 : -7.70100211134234E-06  -4.11740205295453E-06
#  x4 :  6.25149147668167E-11   0.00000000000000E+00
#  x7 :  4.99996874254262E-06   0.00000000000000E+00
# == err :  8.466E-19 = rco :  2.446E-15 = res :  8.375E-21 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -8.91664127240689E-08   9.91828751827260E-08
#  x6 : -1.87360573646801E-08  -1.75657962676780E-07
#  x9 : -7.91642740880272E-07  -1.14477345706777E-06
#  x10 :  5.45914063416685E-06   6.98453253619300E-07
#  x3 :  4.73669630616939E-10  -7.13749004225551E-06
#  x8 :  2.99995263303694E-05   7.13749004225551E-06
#  x1 : -1.49581864476755E-07   6.28891662707340E-07
#  x5 : -7.25872117958961E-06  -3.66003075075719E-06
#  x4 :  6.25149147668167E-11  -1.42581726046354E-56
#  x7 :  4.99996874254262E-06   7.09033777052028E-57
# == err :  8.069E-19 = rco :  1.843E-15 = res :  1.388E-20 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 :  7.04569390653156E-08  -9.69102131998154E-08
#  x6 : -4.39692964369002E-08  -1.35619823110719E-07
#  x9 :  8.31555564214474E-07   9.93970971497191E-07
#  x10 :  4.59296304479701E-06  -3.12910556037969E-07
#  x3 : -1.40318385913825E-07  -6.90383625579641E-06
#  x8 :  3.01403183859138E-05   6.90383625579641E-06
#  x1 : -1.62832772240636E-07   6.49907434653423E-07
#  x5 : -7.70100211134234E-06  -4.11740205295453E-06
#  x4 : -6.25153055807167E-11   0.00000000000000E+00
#  x7 :  5.00003125765279E-06   0.00000000000000E+00
# == err :  8.468E-19 = rco :  2.446E-15 = res :  8.565E-21 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -8.91664127240689E-08   9.91828751827260E-08
#  x6 : -1.87360573646801E-08  -1.75657962676780E-07
#  x9 : -7.91642740880272E-07  -1.14477345706777E-06
#  x10 :  5.45914063416685E-06   6.98453253619300E-07
#  x3 :  4.73669630616637E-10  -7.13749004225551E-06
#  x8 :  2.99995263303694E-05   7.13749004225551E-06
#  x1 : -1.49581864476755E-07   6.28891662707340E-07
#  x5 : -7.25872117958961E-06  -3.66003075075719E-06
#  x4 : -6.25153055807167E-11  -4.63650768835928E-69
#  x7 :  5.00003125765279E-06   2.31825384417964E-69
# == err :  9.068E-19 = rco :  1.843E-15 = res :  1.474E-20 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 :  7.04569390653156E-08   9.69102131998154E-08
#  x6 : -4.39692964369002E-08   1.35619823110719E-07
#  x9 :  8.31555564214474E-07  -9.93970971497191E-07
#  x10 :  4.59296304479701E-06   3.12910556037969E-07
#  x3 : -1.40318385913825E-07   6.90383625579641E-06
#  x8 :  3.01403183859138E-05  -6.90383625579641E-06
#  x1 : -1.62832772240636E-07  -6.49907434653423E-07
#  x5 : -7.70100211134234E-06   4.11740205295453E-06
#  x4 : -6.25153055807167E-11   0.00000000000000E+00
#  x7 :  5.00003125765279E-06   0.00000000000000E+00
# == err :  8.467E-19 = rco :  2.446E-15 = res :  1.525E-20 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -2.60313982923272E-07  -1.28338532813785E-65
#  x6 :  6.72968678176653E-07   4.36944484550978E-65
#  x9 : -6.17521153048185E-07  -8.60535826959482E-66
#  x10 :  4.76594888980908E-06  -3.29377506181043E-65
#  x3 :  1.51354874557977E-05  -1.54302975868597E-65
#  x8 :  1.48645125442023E-05   1.54302975868597E-65
#  x1 :  1.46944846381988E-07   8.25298368527951E-67
#  x5 :  4.20057436327419E-07   1.28338532813785E-65
#  x4 : -6.25153055807167E-11  -1.66914276780934E-66
#  x7 :  5.00003125765279E-06   8.34571383904670E-67
# == err :  3.791E-19 = rco :  1.325E-15 = res :  1.482E-21 ==
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 :  7.04569390653156E-08   9.69102131998154E-08
#  x6 : -4.39692964369002E-08   1.35619823110719E-07
#  x9 :  8.31555564214474E-07  -9.93970971497191E-07
#  x10 :  4.59296304479701E-06   3.12910556037969E-07
#  x3 : -1.40318385913825E-07   6.90383625579641E-06
#  x8 :  3.01403183859138E-05  -6.90383625579641E-06
#  x1 : -1.62832772240636E-07  -6.49907434653423E-07
#  x5 : -7.70100211134234E-06   4.11740205295453E-06
#  x4 :  6.25149147668167E-11  -3.56454315115886E-57
#  x7 :  4.99996874254262E-06   1.75240110224570E-57
# == err :  8.468E-19 = rco :  2.446E-15 = res :  8.565E-21 ==
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -2.60313982923272E-07  -1.03487250547164E-54
#  x6 :  6.72968678176653E-07   3.54813430447420E-54
#  x9 : -6.17521153048185E-07  -3.97635741018660E-55
#  x10 :  4.76594888980908E-06  -2.85482070474935E-54
#  x3 :  1.51354874557977E-05  -3.28304381046176E-54
#  x8 :  1.48645125442023E-05   3.28304381046176E-54
#  x1 :  1.46944846381988E-07   3.13839106493093E-56
#  x5 :  4.20057436327419E-07  -4.39693367472557E-56
#  x4 :  6.25149147668167E-11   3.54462950226971E-57
#  x7 :  4.99996874254262E-06  -1.77231475113485E-57
# == err :  3.791E-19 = rco :  1.325E-15 = res :  1.482E-21 ==
# solution 13 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -2.94190576551449E-08  -1.46096250884933E-06
#  x6 : -2.11885804732594E-05   8.53685060755712E-07
#  x9 : -2.35403168406488E-09   6.04925907316275E-07
#  x10 :  2.62044670179290E-05  -4.25666759989186E-07
#  x3 :  3.62020323688239E-05   1.79872021664988E-07
#  x8 : -6.20203236882388E-06  -1.79872021664988E-07
#  x1 : -2.56361634369072E-08  -6.16039194939472E-10
#  x5 :  1.27777730079002E-08   6.14456766419239E-10
#  x4 : -6.25153055807167E-11   1.53683970830301E-50
#  x7 :  5.00003125765279E-06  -7.68419854151506E-51
# == err :  4.634E-19 = rco :  6.286E-16 = res :  1.211E-20 ==
# solution 14 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -2.94190576551449E-08   1.46096250884933E-06
#  x6 : -2.11885804732594E-05  -8.53685060755712E-07
#  x9 : -2.35403168406488E-09  -6.04925907316275E-07
#  x10 :  2.62044670179290E-05   4.25666759989186E-07
#  x3 :  3.62020323688239E-05  -1.79872021664988E-07
#  x8 : -6.20203236882388E-06   1.79872021664988E-07
#  x1 : -2.56361634369072E-08   6.16039194939473E-10
#  x5 :  1.27777730079003E-08  -6.14456766419239E-10
#  x4 : -6.25153055807167E-11   1.05638191171578E-64
#  x7 :  5.00003125765279E-06  -5.28669782977005E-65
# == err :  4.619E-19 = rco :  6.286E-16 = res :  6.776E-21 ==
# solution 15 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -2.94190576551449E-08  -1.46096250884933E-06
#  x6 : -2.11885804732594E-05   8.53685060755713E-07
#  x9 : -2.35403168406489E-09   6.04925907316275E-07
#  x10 :  2.62044670179290E-05  -4.25666759989186E-07
#  x3 :  3.62020323688239E-05   1.79872021664988E-07
#  x8 : -6.20203236882388E-06  -1.79872021664988E-07
#  x1 : -2.56361634369072E-08  -6.16039194939473E-10
#  x5 :  1.27777730079002E-08   6.14456766419239E-10
#  x4 :  6.25149147668167E-11   1.48368246027497E-67
#  x7 :  4.99996874254262E-06  -7.41841230137484E-68
# == err :  4.523E-19 = rco :  6.286E-16 = res :  2.131E-21 ==
# solution 16 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x2 : -2.94190576551449E-08   1.46096250884933E-06
#  x6 : -2.11885804732594E-05  -8.53685060755712E-07
#  x9 : -2.35403168406488E-09  -6.04925907316275E-07
#  x10 :  2.62044670179290E-05   4.25666759989186E-07
#  x3 :  3.62020323688239E-05  -1.79872021664988E-07
#  x8 : -6.20203236882388E-06   1.79872021664988E-07
#  x1 : -2.56361634369072E-08   6.16039194939473E-10
#  x5 :  1.27777730079003E-08  -6.14456766419239E-10
#  x4 :  6.25149147668167E-11  -3.13214614463929E-51
#  x7 :  4.99996874254262E-06   1.56607307231965E-51
# == err :  4.619E-19 = rco :  6.286E-16 = res :  6.776E-21 ==
