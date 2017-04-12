var a20;
var a21;
var a22;
var a23;
var a30;
var a31;
var a32;
var a33;
var a34;
var a35;

subject to


cons1:  16*a20*a32 + 18*a21*a31 + 20*a22*a30 = 0;
cons2: -80*a23 + 180*a34 + 855*a35 = 0;
cons3:  7*a20*a31 + 8*a21*a30 = 0;
cons4:  210*a35 - 210 = 0;
cons5:  40*a20*a34 + 44*a21*a33 + 48*a22*a32 + 52*a23*a31 + 280*a30 = 0;
cons6:  27*a20*a33 + 30*a21*a32 + 33*a22*a31 + 36*a23*a30 = 0;
cons7:  55*a20*a35 + 60*a21*a34 + 65*a22*a33 + 70*a23*a32 + 80*a30 + 375*a31 = 0;
cons8:  78*a21*a35 + 84*a22*a34 + 90*a23*a33 - 170*a20 + 102*a31 + 480*a32 = 0;
cons9: 136*a23*a35 - 114*a22 + 152*a33 + 720*a34 = 0;
cons10: 105*a22*a35 + 112*a23*a34 - 144*a21 + 126*a32 + 595*a33 = 0;

#solve;
#display a20, a21, a22, a23, a30, a31, a32, a33, a34, a35;

# TITLE : a "dessin d'enfant", called des22_24

# ROOT COUNTS :

# total degree : 256
# 2-homogeneous Bezout number : 128
#   with partition : {a20 a32 a21 a31 a22 a30 a23 a34 a33 }{a35 }
# generalized Bezout number : 82
#   based on the set structure :
#      {a20 a21 a22 }{a32 a31 a30 }
#      {a23 a34 a35 }
#      {a20 a21 }{a31 a30 }
#      {a35 }
#      {a20 a32 a21 a31 a30 }{a22 a23 a34 a33 }
#      {a20 a32 a31 a30 }{a21 a22 a23 a33 }
#      {a20 a32 a21 a31 a22 a30 }{a23 a34 a35 a33 }
#      {a20 a32 a21 a31 a22 a23 }{a34 a35 a33 }
#      {a22 a23 a34 a33 }{a35 }
#      {a32 a21 a22 a23 a33 }{a34 a35 }
# mixed volume : 42

# REFERENCES :

# Raphael Nauheim:
#  "Systems of Algebraic Equations with Bad Reduction"
# Universitaet Heidelberg, Interdisziplinaeres Zentrum
# fuer wissenschaftliches Rechnen, Preprint 95-46, Dezember 1995.

# Birch, B: "Noncongruence Subgroups, Covers and Drawings",
# In "The Grothendieck Theory of Dessins d'Enfants",
# editor: Schneps, L., London Mathematical Society Lecture Series 200,
# Cambridge University Press, pages 25-46, 1994.

# There are 10 real and 32 conjugate complex solutions.
# Among the 10 real solutions, four of them are ill-conditioned.

# THE SOLUTIONS :

# 42 10
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -5.21442301624664E-01   1.72453316373668E+00
#  a32 : -2.49589958104655E-01   9.59735966832008E-02
#  a21 :  6.12605878167648E-01   4.27408507742323E+00
#  a31 :  1.65974672649899E-01  -9.69300921262394E-02
#  a22 :  5.92317473211998E+00   3.42130127841126E+00
#  a30 : -6.98746944965228E-02   6.48809654978709E-03
#  a23 :  8.77149809226069E+00   8.88393395652697E-01
#  a34 : -8.51556403439692E-01   3.94841509178976E-01
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  6.27886772307902E-01  -9.92042281496473E-02
# == err :  8.690E-15 = rco :  1.602E-04 = res :  1.608E-13 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.86597500217766E+02  -6.53976955142540E+01
#  a32 :  1.82302765279853E+01   2.30100480815243E+01
#  a21 : -6.09817398619465E+01  -7.42761587895783E+01
#  a31 : -2.13515066060056E+01  -2.05073641559774E+02
#  a22 :  5.12778357184201E+00  -4.36655825195166E+01
#  a30 :  2.24639632342970E+02   2.95488095558237E+02
#  a23 :  1.31759915543100E+01  -1.04208387494438E+01
#  a34 :  1.10599624636001E+00  -4.63148388864171E+00
#  a35 :  1.00000000000000E+00  -1.48986021128242E-31
#  a33 : -1.31821369840486E+01  -1.48667064130590E+00
# == err :  4.798E-13 = rco :  8.214E-06 = res :  1.277E-10 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.63061630518511E+01   2.96813278451046E+01
#  a32 :  1.28263751656046E+00   1.43499849654010E+00
#  a21 : -1.95746459897342E+00   3.38436050253517E+01
#  a31 : -8.30043233701328E+00  -2.06901627455250E+00
#  a22 :  1.74147206562589E+01   2.67947427846543E+01
#  a30 :  5.77230428445937E+00   4.75319791898517E+00
#  a23 :  1.13262147431237E+01   8.47303479955049E+00
#  a34 :  2.83873219166110E-01   3.76579324424466E+00
#  a35 :  1.00000000000000E+00  -6.28310530989662E-33
#  a33 :  1.58239626282290E+00  -5.32304731016076E+00
# == err :  5.983E-14 = rco :  8.948E-05 = res :  2.034E-12 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -8.46158832269196E+04  -1.71289242284961E-54
#  a32 : -6.21252325160952E+03  -8.66641999656054E-56
#  a21 :  2.23065985462703E+03   2.16660499914014E-56
#  a31 : -2.02999525269441E+04  -3.67048376324917E-55
#  a22 :  5.63663795687017E+02   8.12476874677551E-57
#  a30 : -6.73785430731539E+05  -1.66395263933962E-53
#  a23 : -1.04041138177933E+02  -7.76632306677071E-58
#  a34 : -5.09905058568591E+01  -3.48488855560224E-58
#  a35 :  1.00000000000000E+00   6.80642296016062E-62
#  a33 :  7.57371261299062E+02   8.60269632011524E-57
# == err :  9.368E-10 = rco :  1.032E-11 = res :  4.768E-07 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -6.68772375765163E+00   4.00957319845439E+00
#  a32 : -2.28056277016263E+00  -2.69507811290578E+00
#  a21 : -8.50033975471788E+00   9.05748332362677E-01
#  a31 :  1.94749907971695E+00   2.13718449554830E+00
#  a22 :  3.94001793254939E-01  -3.76544896145528E+00
#  a30 : -2.12749773530846E+00  -8.94164789888950E-01
#  a23 :  7.39514460344525E+00  -1.61042256968354E+00
#  a34 : -1.46326906513544E+00  -7.15743364303795E-01
#  a35 :  1.00000000000000E+00  -3.75971120784920E-32
#  a33 :  6.10067534605443E-01   2.00718098795916E+00
# == err :  6.865E-15 = rco :  7.041E-04 = res :  2.898E-13 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -2.16580787516837E+00   6.07716335728627E-64
#  a32 : -4.50407537405366E-01   5.69734064745588E-65
#  a21 : -3.12784134068775E+00   1.51929083932157E-64
#  a31 :  1.64525238380772E-01   1.13946812949118E-64
#  a22 :  3.18261760274002E+00  -4.55787251796470E-64
#  a30 : -9.96817823764183E-02   7.35906500296384E-65
#  a23 :  8.12238892404369E+00  -2.27893625898235E-64
#  a34 : -1.14004936709169E+00  -9.49556774575980E-65
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  5.19796429923932E-01   2.65875896881274E-64
# == err :  3.852E-15 = rco :  1.848E-04 = res :  1.137E-13 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  7.79008417264091E-01  -9.96654790594949E-62
#  a32 : -4.71639144329436E-02  -4.25401435010039E-63
#  a21 :  4.33907947158647E+00  -2.33363072919793E-61
#  a31 :  1.20632369858885E-02   9.11574503592941E-64
#  a22 :  9.34873086416943E+00  -1.70160574004016E-61
#  a30 : -1.89503161013087E-03   3.03858167864314E-64
#  a23 :  9.78828230464056E+00  -4.61864415153757E-62
#  a34 : -3.99652309048638E-01  -2.12700717505020E-62
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  1.46701234205377E-01   1.35976530119280E-62
# == err :  5.047E-15 = rco :  3.749E-05 = res :  2.274E-13 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -3.02217389406608E+02   4.84972259453532E+02
#  a32 : -1.25420726152472E+02   1.15635505071413E+02
#  a21 : -4.16191871003110E+01   6.21829358739311E+01
#  a31 :  2.30657472818142E+02   2.74353689973003E+01
#  a22 : -1.94405282527366E+01  -4.01886737147898E+01
#  a30 : -1.54645615456441E+03  -1.33076421955964E+02
#  a23 : -3.61942750625190E+00  -6.17714293191584E+00
#  a34 : -6.35863444722306E+00  -2.74539685862926E+00
#  a35 :  1.00000000000000E+00  -5.52668517294696E-34
#  a33 :  1.87778863292032E+01  -1.16100764903448E+01
# == err :  3.079E-12 = rco :  7.960E-07 = res :  2.400E-10 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  2.63407894021685E+00   4.65396842480159E+00
#  a32 : -1.94995329660351E+00  -9.37153295496033E-01
#  a21 :  3.88742221150536E+00   6.54045636048682E+00
#  a31 :  2.21366141476578E+00  -2.57866993365055E-01
#  a22 :  6.07795729280648E+00   2.69812122001744E+00
#  a30 : -1.36445832828497E+00   1.29647708011385E-01
#  a23 :  8.38506326648962E+00   2.88372971105167E-01
#  a34 : -1.02330521489350E+00   1.28165764935630E-01
#  a35 :  1.00000000000000E+00  -3.21756861432357E-33
#  a33 :  1.90327817013601E+00   1.15847200169758E+00
# == err :  6.562E-15 = rco :  1.273E-03 = res :  2.274E-13 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  6.75457747607353E+01  -4.37555761724612E-62
#  a32 : -1.92708131964110E+01   2.12700717505020E-62
#  a21 :  6.73039941101110E+01  -5.59099028870337E-62
#  a31 :  5.06730159372344E+01  -3.64629801437176E-62
#  a22 :  4.55775394871917E+01  -3.64629801437176E-62
#  a30 : -4.44981705086610E+01   4.86173068582902E-62
#  a23 :  1.98893535462323E+01  -1.09388940431153E-62
#  a34 :  4.08971268721435E+00  -3.64629801437176E-63
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 : -2.98490602330314E+00   1.82314900718588E-63
# == err :  1.604E-13 = rco :  1.503E-04 = res :  1.273E-11 ==
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  7.80121000142459E-01  -9.21593522974773E-01
#  a32 : -2.81022179058488E-01  -1.32965273061917E-01
#  a21 :  4.10928793671956E+00  -2.60260417738429E+00
#  a31 :  1.25389880322756E-01   1.76339805168452E-02
#  a22 :  8.92784846726175E+00  -2.36534708059111E+00
#  a30 : -2.71340752231498E-02   4.49162310220171E-03
#  a23 :  9.60155101137631E+00  -6.94928869867858E-01
#  a34 : -4.82643994943861E-01  -3.08857275496826E-01
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  3.91233316317373E-01   3.10776299160244E-01
# == err :  7.909E-15 = rco :  1.045E-04 = res :  1.913E-13 ==
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -5.21442301624663E-01  -1.72453316373668E+00
#  a32 : -2.49589958104654E-01  -9.59735966832010E-02
#  a21 :  6.12605878167649E-01  -4.27408507742323E+00
#  a31 :  1.65974672649899E-01   9.69300921262396E-02
#  a22 :  5.92317473211998E+00  -3.42130127841126E+00
#  a30 : -6.98746944965227E-02  -6.48809654978718E-03
#  a23 :  8.77149809226070E+00  -8.88393395652697E-01
#  a34 : -8.51556403439692E-01  -3.94841509178976E-01
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  6.27886772307902E-01   9.92042281496476E-02
# == err :  3.857E-15 = rco :  1.602E-04 = res :  1.711E-13 ==
# solution 13 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.20789348772292E+01   1.84589295037727E+01
#  a32 :  9.79971434453732E+00  -3.80840691065170E+00
#  a21 : -7.83062394848755E+00   1.88866927566190E+01
#  a31 : -1.02591347556095E+01   8.72975072115892E+00
#  a22 :  5.70235114381702E+00   8.17929361336767E+00
#  a30 :  1.10449908203911E+01  -6.30384576239286E+00
#  a23 :  1.05000218228669E+01   1.02958963057461E+00
#  a34 : -8.33236342813868E-02   4.57595391366493E-01
#  a35 :  1.00000000000000E+00  -3.05201472049099E-32
#  a33 : -4.72330211073788E+00   3.04570131830193E+00
# == err :  3.513E-14 = rco :  7.169E-04 = res :  1.592E-12 ==
# solution 14 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.20789348772292E+01  -1.84589295037727E+01
#  a32 :  9.79971434453731E+00   3.80840691065170E+00
#  a21 : -7.83062394848755E+00  -1.88866927566190E+01
#  a31 : -1.02591347556095E+01  -8.72975072115892E+00
#  a22 :  5.70235114381702E+00  -8.17929361336767E+00
#  a30 :  1.10449908203911E+01   6.30384576239285E+00
#  a23 :  1.05000218228669E+01  -1.02958963057461E+00
#  a34 : -8.33236342813868E-02  -4.57595391366493E-01
#  a35 :  1.00000000000000E+00   7.15793215629991E-32
#  a33 : -4.72330211073788E+00  -3.04570131830193E+00
# == err :  6.240E-14 = rco :  7.169E-04 = res :  2.265E-12 ==
# solution 15 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  1.07248400424143E+02  -7.74765179171210E+01
#  a32 :  3.47786321820574E+01   1.52127129236801E+01
#  a21 :  3.08817975844588E+01  -4.20039131704811E+01
#  a31 : -7.09423686497818E+01  -8.89798618673168E+01
#  a22 :  4.34797041322392E+00  -4.31456106259508E+00
#  a30 :  8.94584846456260E+01   2.36332229073738E+02
#  a23 :  7.81265986574173E+00   3.62976913157115E+00
#  a34 : -1.27770672633701E+00   1.61323072514273E+00
#  a35 :  1.00000000000000E+00   1.50690836739665E-31
#  a33 :  2.32299821269276E+00  -1.41252281916597E+01
# == err :  4.040E-13 = rco :  1.352E-05 = res :  1.455E-11 ==
# solution 16 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.86597500217766E+02   6.53976955142541E+01
#  a32 :  1.82302765279853E+01  -2.30100480815243E+01
#  a21 : -6.09817398619465E+01   7.42761587895783E+01
#  a31 : -2.13515066060056E+01   2.05073641559774E+02
#  a22 :  5.12778357184201E+00   4.36655825195166E+01
#  a30 :  2.24639632342970E+02  -2.95488095558237E+02
#  a23 :  1.31759915543100E+01   1.04208387494438E+01
#  a34 :  1.10599624636001E+00   4.63148388864171E+00
#  a35 :  1.00000000000000E+00   8.56571607890014E-31
#  a33 : -1.31821369840486E+01   1.48667064130590E+00
# == err :  7.438E-13 = rco :  8.214E-06 = res :  2.933E-11 ==
# solution 17 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  2.99465614628938E+00  -2.37934303528835E+00
#  a32 :  5.27446790794594E-01  -1.46246342620709E+00
#  a21 :  9.62896108633765E+00  -4.99813183389747E+00
#  a31 : -1.41774152212242E-01   9.17073475386279E-01
#  a22 :  1.44215430925602E+01  -4.24626213311965E+00
#  a30 : -1.12265030263065E-02  -2.86043784590367E-01
#  a23 :  1.13638336684732E+01  -1.52259535312830E+00
#  a34 :  3.00592741543645E-01  -6.76709045834798E-01
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 : -7.75343685999475E-01   1.38308945954516E+00
# == err :  2.329E-14 = rco :  4.343E-04 = res :  4.019E-13 ==
# solution 18 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -3.02644947747119E+00  -5.29684811548649E+00
#  a32 :  6.43596936267657E-01  -2.41946693053619E+00
#  a21 :  1.31122776182327E-01  -1.12439905037670E+01
#  a31 :  2.16390216742216E-01   2.87774362314231E+00
#  a22 :  7.97451412174043E+00  -9.87059521605463E+00
#  a30 : -7.80083601770600E-01  -1.12613760153537E+00
#  a23 :  9.41371950010189E+00  -2.99090313528679E+00
#  a34 : -5.66124666621383E-01  -1.32929028234968E+00
#  a35 :  1.00000000000000E+00   9.00645773769166E-33
#  a33 :  2.39727090999660E-01   1.56976299381939E+00
# == err :  1.266E-14 = rco :  5.737E-04 = res :  4.547E-13 ==
# solution 19 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  8.24398044230713E+00  -5.69786346993678E+00
#  a32 :  1.50145633779107E+00   5.75293620914512E+00
#  a21 :  1.56928004936062E+01  -7.36252073041826E+00
#  a31 : -2.50761502208893E+00  -6.65533308480757E+00
#  a22 :  1.71823339473881E+01  -3.96395339072417E+00
#  a30 :  1.80766505639643E+00   3.11066598716923E+00
#  a23 :  1.21310325820078E+01  -3.60642363969528E-01
#  a34 :  6.41570036447915E-01  -1.60285495097568E-01
#  a35 :  1.00000000000000E+00  -1.54308595480717E-32
#  a33 : -1.00634728548238E+00  -1.89103795113454E+00
# == err :  3.769E-14 = rco :  7.887E-04 = res :  1.890E-12 ==
# solution 20 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  7.49828571428572E+03   5.22733283340336E-58
#  a32 :  4.50045918367347E+02   5.97409466674670E-59
#  a21 :  8.33142857142857E+02   8.46330077789115E-59
#  a31 :  7.85534693877551E+03   4.77927573339736E-58
#  a22 :  1.38857142857143E+02   5.52292605910176E-60
#  a30 : -6.18608571428572E+04  -6.85029521786955E-57
#  a23 :  2.57142857142857E+01   1.68702054798267E-60
#  a34 :  6.67857142857143E+00   6.90365757387721E-61
#  a35 :  1.00000000000000E+00   1.85353482397231E-62
#  a33 :  4.95000000000000E+01  -1.08902767362570E-60
# == err :  4.474E-11 = rco :  2.644E-09 = res :  5.960E-08 ==
# solution 21 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  2.63407894021685E+00  -4.65396842480159E+00
#  a32 : -1.94995329660351E+00   9.37153295496032E-01
#  a21 :  3.88742221150536E+00  -6.54045636048682E+00
#  a31 :  2.21366141476578E+00   2.57866993365056E-01
#  a22 :  6.07795729280648E+00  -2.69812122001744E+00
#  a30 : -1.36445832828497E+00  -1.29647708011386E-01
#  a23 :  8.38506326648962E+00  -2.88372971105167E-01
#  a34 : -1.02330521489350E+00  -1.28165764935630E-01
#  a35 :  1.00000000000000E+00  -6.80239879431609E-32
#  a33 :  1.90327817013601E+00  -1.15847200169758E+00
# == err :  7.619E-15 = rco :  1.273E-03 = res :  3.813E-13 ==
# solution 22 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  7.80121000142458E-01   9.21593522974774E-01
#  a32 : -2.81022179058489E-01   1.32965273061917E-01
#  a21 :  4.10928793671956E+00   2.60260417738429E+00
#  a31 :  1.25389880322756E-01  -1.76339805168449E-02
#  a22 :  8.92784846726174E+00   2.36534708059111E+00
#  a30 : -2.71340752231498E-02  -4.49162310220180E-03
#  a23 :  9.60155101137631E+00   6.94928869867858E-01
#  a34 : -4.82643994943862E-01   3.08857275496826E-01
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  3.91233316317374E-01  -3.10776299160244E-01
# == err :  1.030E-14 = rco :  1.045E-04 = res :  1.146E-13 ==
# solution 23 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -3.05217728283331E+04  -1.60363500371269E+04
#  a32 :  3.02151015390978E+03  -9.64238921999533E+02
#  a21 : -3.46050009643349E+03  -5.30857365681648E+02
#  a31 :  1.90284307740835E+02   1.30922481114622E+04
#  a22 : -2.49939594990663E+02   8.32745627549361E+02
#  a30 :  3.51724713024513E+04  -1.07207161538227E+05
#  a23 : -9.89535191130913E+00   1.25373463141695E+02
#  a34 : -9.14793418280406E+00   5.57215391740867E+01
#  a35 :  1.00000000000000E+00  -2.14385791571834E-31
#  a33 : -1.35268640509070E+02   2.48438831236936E+02
# == err :  3.104E-10 = rco :  1.942E-10 = res :  4.768E-07 ==
# solution 24 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  8.24398044230713E+00   5.69786346993678E+00
#  a32 :  1.50145633779107E+00  -5.75293620914513E+00
#  a21 :  1.56928004936062E+01   7.36252073041826E+00
#  a31 : -2.50761502208893E+00   6.65533308480757E+00
#  a22 :  1.71823339473881E+01   3.96395339072416E+00
#  a30 :  1.80766505639643E+00  -3.11066598716923E+00
#  a23 :  1.21310325820078E+01   3.60642363969527E-01
#  a34 :  6.41570036447915E-01   1.60285495097568E-01
#  a35 :  1.00000000000000E+00  -7.73625681831120E-33
#  a33 : -1.00634728548238E+00   1.89103795113454E+00
# == err :  3.945E-14 = rco :  7.887E-04 = res :  1.729E-12 ==
# solution 25 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  2.65776274396502E+00   4.86173068582902E-63
#  a32 : -1.60177246298913E+00  -1.10604373102610E-61
#  a21 :  6.91718100964978E+00   7.77876909732643E-62
#  a31 :  1.18854045347435E+00   9.96654790594949E-62
#  a22 :  9.99414484462862E+00   1.33697593860298E-62
#  a30 : -3.99584919921292E-01  -2.58279442684667E-62
#  a23 :  9.63176834344344E+00  -1.29139721342333E-62
#  a34 : -4.69214069580693E-01  -5.54541156352372E-63
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 :  1.10030360314114E+00   5.10481722012047E-62
# == err :  1.117E-14 = rco :  1.079E-03 = res :  3.695E-13 ==
# solution 26 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  2.99465614628938E+00   2.37934303528835E+00
#  a32 :  5.27446790794594E-01   1.46246342620709E+00
#  a21 :  9.62896108633765E+00   4.99813183389747E+00
#  a31 : -1.41774152212242E-01  -9.17073475386279E-01
#  a22 :  1.44215430925602E+01   4.24626213311965E+00
#  a30 : -1.12265030263064E-02   2.86043784590367E-01
#  a23 :  1.13638336684732E+01   1.52259535312830E+00
#  a34 :  3.00592741543646E-01   6.76709045834798E-01
#  a35 :  1.00000000000000E+00   0.00000000000000E+00
#  a33 : -7.75343685999476E-01  -1.38308945954516E+00
# == err :  2.149E-14 = rco :  4.343E-04 = res :  2.831E-13 ==
# solution 27 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  1.43012000152534E+01   1.18694596821997E-66
#  a32 : -2.25510167551102E+00   2.78190461301557E-67
#  a21 :  8.15762946888016E+00   5.93472984109987E-67
#  a31 :  3.98122651555201E+00   5.93472984109987E-67
#  a22 :  5.61496961298918E-01   1.48368246027497E-67
#  a30 : -6.10707771257244E+00   3.70920615068742E-68
#  a23 :  6.07470140738288E+00   3.70920615068742E-68
#  a34 : -2.05013270782983E+00   0.00000000000000E+00
#  a35 :  1.00000000000000E+00   5.79563461044910E-70
#  a33 :  4.69701849882556E+00   2.96736492054994E-67
# == err :  6.623E-15 = rco :  7.199E-04 = res :  4.547E-13 ==
# solution 28 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -3.05217728283331E+04   1.60363500371269E+04
#  a32 :  3.02151015390978E+03   9.64238921999530E+02
#  a21 : -3.46050009643349E+03   5.30857365681650E+02
#  a31 :  1.90284307740822E+02  -1.30922481114622E+04
#  a22 : -2.49939594990664E+02  -8.32745627549361E+02
#  a30 :  3.51724713024514E+04   1.07207161538227E+05
#  a23 : -9.89535191130918E+00  -1.25373463141695E+02
#  a34 : -9.14793418280408E+00  -5.57215391740866E+01
#  a35 :  1.00000000000000E+00   5.89196360757928E-31
#  a33 : -1.35268640509070E+02  -2.48438831236936E+02
# == err :  3.069E-10 = rco :  1.942E-10 = res :  2.980E-07 ==
# solution 29 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -3.02644947747120E+00   5.29684811548649E+00
#  a32 :  6.43596936267659E-01   2.41946693053619E+00
#  a21 :  1.31122776182327E-01   1.12439905037670E+01
#  a31 :  2.16390216742216E-01  -2.87774362314232E+00
#  a22 :  7.97451412174043E+00   9.87059521605464E+00
#  a30 : -7.80083601770601E-01   1.12613760153537E+00
#  a23 :  9.41371950010189E+00   2.99090313528679E+00
#  a34 : -5.66124666621383E-01   1.32929028234968E+00
#  a35 :  1.00000000000000E+00   1.18612182852518E-31
#  a33 :  2.39727090999659E-01  -1.56976299381939E+00
# == err :  1.016E-14 = rco :  5.737E-04 = res :  7.525E-13 ==
# solution 30 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.63061630518510E+01  -2.96813278451046E+01
#  a32 :  1.28263751656046E+00  -1.43499849654010E+00
#  a21 : -1.95746459897340E+00  -3.38436050253517E+01
#  a31 : -8.30043233701328E+00   2.06901627455250E+00
#  a22 :  1.74147206562589E+01  -2.67947427846543E+01
#  a30 :  5.77230428445937E+00  -4.75319791898516E+00
#  a23 :  1.13262147431237E+01  -8.47303479955048E+00
#  a34 :  2.83873219166110E-01  -3.76579324424466E+00
#  a35 :  1.00000000000000E+00  -4.16172504287871E-32
#  a33 :  1.58239626282290E+00   5.32304731016076E+00
# == err :  7.181E-14 = rco :  8.948E-05 = res :  3.859E-12 ==
# solution 31 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -6.68772375765163E+00  -4.00957319845438E+00
#  a32 : -2.28056277016263E+00   2.69507811290578E+00
#  a21 : -8.50033975471788E+00  -9.05748332362675E-01
#  a31 :  1.94749907971695E+00  -2.13718449554830E+00
#  a22 :  3.94001793254940E-01   3.76544896145528E+00
#  a30 : -2.12749773530846E+00   8.94164789888951E-01
#  a23 :  7.39514460344526E+00   1.61042256968354E+00
#  a34 : -1.46326906513544E+00   7.15743364303794E-01
#  a35 :  1.00000000000000E+00  -1.50196547857632E-31
#  a33 :  6.10067534605442E-01  -2.00718098795916E+00
# == err :  5.950E-15 = rco :  7.041E-04 = res :  5.713E-13 ==
# solution 32 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  1.55447206999807E+01   2.46889605035042E+01
#  a32 : -2.70888901615033E+00   1.06098693586985E+01
#  a21 :  1.81480276531994E+01   1.98810480631786E+01
#  a31 :  4.19895012527010E+00  -3.37068520952933E+01
#  a22 :  1.57205495781764E+01   9.03529941395446E+00
#  a30 : -9.57730755160849E+00   3.07562796437907E+01
#  a23 :  1.04181713515821E+01   2.69936387538633E+00
#  a34 : -1.19701621519051E-01   1.19971727794948E+00
#  a35 :  1.00000000000000E+00  -1.22451340830367E-32
#  a33 :  3.03589812888593E+00  -1.32161706569315E+00
# == err :  7.316E-14 = rco :  1.078E-04 = res :  3.311E-12 ==
# solution 33 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -1.45905903849896E+03   8.08991986121948E-60
#  a32 :  2.59811202683644E+01   1.84745766061503E-61
#  a21 : -1.75960484496282E+02  -2.13916150176477E-61
#  a31 :  3.92374294037773E+02  -3.73380916671669E-60
#  a22 : -3.24794617900584E+01   3.64629801437176E-62
#  a30 : -2.84686135128959E+03   1.74244427780112E-59
#  a23 :  3.08628373574861E+01  -1.04527209745324E-61
#  a34 :  8.96681660332717E+00  -4.73907399384284E-62
#  a35 :  1.00000000000000E+00  -2.65875896881274E-64
#  a33 : -9.44481084150022E+01   3.30597686636373E-61
# == err :  4.439E-12 = rco :  2.505E-07 = res :  4.657E-10 ==
# solution 34 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  1.07248400424143E+02   7.74765179171210E+01
#  a32 :  3.47786321820574E+01  -1.52127129236801E+01
#  a21 :  3.08817975844588E+01   4.20039131704811E+01
#  a31 : -7.09423686497818E+01   8.89798618673168E+01
#  a22 :  4.34797041322392E+00   4.31456106259507E+00
#  a30 :  8.94584846456260E+01  -2.36332229073738E+02
#  a23 :  7.81265986574173E+00  -3.62976913157115E+00
#  a34 : -1.27770672633701E+00  -1.61323072514273E+00
#  a35 :  1.00000000000000E+00   2.22146803375139E-31
#  a33 :  2.32299821269276E+00   1.41252281916597E+01
# == err :  3.723E-13 = rco :  1.352E-05 = res :  2.058E-11 ==
# solution 35 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -3.02217389406608E+02  -4.84972259453532E+02
#  a32 : -1.25420726152472E+02  -1.15635505071413E+02
#  a21 : -4.16191871003110E+01  -6.21829358739311E+01
#  a31 :  2.30657472818142E+02  -2.74353689973003E+01
#  a22 : -1.94405282527366E+01   4.01886737147898E+01
#  a30 : -1.54645615456441E+03   1.33076421955964E+02
#  a23 : -3.61942750625190E+00   6.17714293191584E+00
#  a34 : -6.35863444722306E+00   2.74539685862926E+00
#  a35 :  1.00000000000000E+00   6.62670440239311E-33
#  a33 :  1.87778863292032E+01   1.16100764903448E+01
# == err :  3.235E-12 = rco :  7.960E-07 = res :  3.293E-10 ==
# solution 36 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  1.55447206999807E+01  -2.46889605035042E+01
#  a32 : -2.70888901615034E+00  -1.06098693586986E+01
#  a21 :  1.81480276531994E+01  -1.98810480631786E+01
#  a31 :  4.19895012527013E+00   3.37068520952934E+01
#  a22 :  1.57205495781764E+01  -9.03529941395447E+00
#  a30 : -9.57730755160852E+00  -3.07562796437907E+01
#  a23 :  1.04181713515821E+01  -2.69936387538633E+00
#  a34 : -1.19701621519051E-01  -1.19971727794948E+00
#  a35 :  1.00000000000000E+00   1.53152356869825E-32
#  a33 :  3.03589812888593E+00   1.32161706569315E+00
# == err :  7.441E-14 = rco :  1.078E-04 = res :  3.865E-12 ==
# solution 37 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -6.23846932348979E+00  -2.74530580201072E+01
#  a32 : -5.74025077921021E+00   2.14861423575046E+00
#  a21 : -1.23752335375474E+01  -6.40894881455591E+00
#  a31 : -7.69848444169724E-01  -7.43296043930539E+00
#  a22 : -8.86882226517601E+00   1.29009408382255E+01
#  a30 : -9.15987457874660E+00   9.51675238632348E+00
#  a23 :  4.07611771962605E+00   4.34557261046742E+00
#  a34 : -2.93839212461065E+00   1.93136560465218E+00
#  a35 :  1.00000000000000E+00  -2.48966490522606E-32
#  a33 :  3.62003014223986E+00  -3.36101220273312E+00
# == err :  2.885E-14 = rco :  1.352E-04 = res :  1.833E-12 ==
# solution 38 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -6.23846932348979E+00   2.74530580201072E+01
#  a32 : -5.74025077921021E+00  -2.14861423575046E+00
#  a21 : -1.23752335375474E+01   6.40894881455591E+00
#  a31 : -7.69848444169724E-01   7.43296043930539E+00
#  a22 : -8.86882226517601E+00  -1.29009408382255E+01
#  a30 : -9.15987457874661E+00  -9.51675238632349E+00
#  a23 :  4.07611771962605E+00  -4.34557261046742E+00
#  a34 : -2.93839212461065E+00  -1.93136560465218E+00
#  a35 :  1.00000000000000E+00   5.81866007965449E-32
#  a33 :  3.62003014223986E+00   3.36101220273312E+00
# == err :  2.711E-14 = rco :  1.352E-04 = res :  1.017E-12 ==
# solution 39 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  3.34213290424468E+02  -3.19594720531197E+01
#  a32 : -3.37160862124812E+00  -4.80598076805119E+01
#  a21 :  1.22918628796847E+02  -3.74429351459338E+01
#  a31 : -1.64784119820812E+02  -6.76624653546220E+01
#  a22 :  6.62689157724618E+01  -9.42423019576460E-01
#  a30 :  3.38414790566269E+02   2.26573586334027E+02
#  a23 :  2.20757715105472E+01  -9.92385384907263E+00
#  a34 :  5.06145400468763E+00  -4.41060171069895E+00
#  a35 :  1.00000000000000E+00  -9.57194231274121E-32
#  a33 :  5.97437229770483E+00   2.90647442825355E+01
# == err :  4.229E-13 = rco :  2.779E-06 = res :  5.821E-11 ==
# solution 40 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 : -5.36964544034678E+01  -2.27893625898235E-64
#  a32 : -2.68418244464410E+01  -9.02078935847181E-65
#  a21 : -2.95381229812747E+01  -1.13946812949118E-64
#  a31 :  4.95381797279303E+01   1.13946812949118E-64
#  a22 : -2.07989253551981E+00  -2.31454463802895E-65
#  a30 : -7.87972050337509E+01  -3.79822709830392E-64
#  a23 :  6.59307288892734E+00  -4.45104738082491E-66
#  a34 : -1.81974538269896E+00  -1.78041895232996E-66
#  a35 :  1.00000000000000E+00  -3.24555538185149E-68
#  a33 :  1.16086193157812E+00  -5.93472984109988E-66
# == err :  4.805E-14 = rco :  7.725E-05 = res :  1.819E-12 ==
# solution 41 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  2.64767721843557E+02  -1.56676867805037E-64
#  a32 : -4.01018871277897E+01   5.93472984109988E-66
#  a21 :  3.71259593441225E+01  -1.48368246027497E-65
#  a31 :  1.70224021642688E+01  -8.30862177753982E-66
#  a22 : -7.46110980379014E+01   1.95846084756296E-65
#  a30 : -1.06222435213525E+02   4.27300548559191E-65
#  a23 : -1.28911197333204E+01   2.96736492054994E-66
#  a34 : -1.04793865481424E+01   1.18694596821997E-66
#  a35 :  1.00000000000000E+00   5.65074374518787E-69
#  a33 :  5.21503567153522E+00   2.96736492054994E-67
# == err :  3.649E-13 = rco :  6.518E-06 = res :  1.455E-11 ==
# solution 42 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  a20 :  3.34213290424468E+02   3.19594720531197E+01
#  a32 : -3.37160862124812E+00   4.80598076805119E+01
#  a21 :  1.22918628796847E+02   3.74429351459338E+01
#  a31 : -1.64784119820812E+02   6.76624653546220E+01
#  a22 :  6.62689157724618E+01   9.42423019576462E-01
#  a30 :  3.38414790566269E+02  -2.26573586334027E+02
#  a23 :  2.20757715105472E+01   9.92385384907263E+00
#  a34 :  5.06145400468763E+00   4.41060171069895E+00
#  a35 :  1.00000000000000E+00  -2.95900972889082E-32
#  a33 :  5.97437229770483E+00  -2.90647442825355E+01
# == err :  3.914E-13 = rco :  2.779E-06 = res :  1.697E-10 ==
