var x1;
var x2;
var x3;
var y1;
var y2;
var y3;
var z1;
var z2;
var z3;

subject to


cons1: 62500*x1^2 + 62500*y1^2 + 62500*z1^2 -74529 = 0;

cons2: 625*x2^2 + 625*y2^2 + 625*z2^2 -1250*x2 -2624 = 0;

cons3: 12500*x3^2 + 12500*y3^2 + 12500*z3^2 + 2500*x3 -44975*y3 -10982 = 0;

cons4: 400000*x1*x2 + 400000*y1*y2 + 400000*z1*z2 -400000*x2 + 178837 = 0;

cons5: 1000000*x1*x3 + 1000000*y1*y3 + 1000000*z1*z3 + 100000*x3 -1799000*y3 -805427 = 0;

cons6: 2000000*x2*x3 + 2000000*y2*y3 + 2000000*z2*z3 -2000000*x2 + 200000*x3
-3598000*y3 -1403 = 0;

cons7:  113800000000000*x3*y2*z1
-113800000000000*x2*y3*z1 -113800000000000*x3*y1*z2 +
113800000000000*x1*y3*z2 + 113800000000000*x2*y1*z3
-113800000000000*x1*y2*z3 -206888400000000*x2*y1 +
206888400000000*x3*y1 + 206888400000000*x1*y2 -206888400000000*x3*y2
-206888400000000*x1*y3 + 206888400000000*x2*y3 -2014260000000*x2*z1 +
2014260000000*x3*z1 -61907200000000*y2*z1 + 61907200000000*y3*z1 +
2014260000000*x1*z2 -2014260000000*x3*z2 + 61907200000000*y1*z2
-61907200000000*y3*z2 -2014260000000*x1*z3 + 2014260000000*x2*z3
-61907200000000*y1*z3 + 61907200000000*y2*z3 -362960716800000*x1 +
38025201600000*x2 + 292548849600000*x3 + 11809567440000*y1 +
1475978220000*y2 -825269402280000*y3 -1212982689600000*z1
-151600474800000*z2 + 825859951200000*z3 -19295432410527 = 0;

cons8:  -777600000000*x3*y2*z1 + 777600000000*x2*y3*z1 +
777600000000*x3*y1*z2 -777600000000*x1*y3*z2 -777600000000*x2*y1*z3 +
777600000000*x1*y2*z3 -1409011200000*x2*y1 + 1409011200000*x3*y1 +
1409011200000*x1*y2 -1409011200000*x3*y2 -1409011200000*x1*y3 +
1409011200000*x2*y3 -1065312000000*x2*z1 + 1065312000000*x3*z1
-805593600000*y2*z1 + 805593600000*y3*z1 + 1065312000000*x1*z2
-1065312000000*x3*z2 + 805593600000*y1*z2 -805593600000*y3*z2
-1065312000000*x1*z3 + 1065312000000*x2*z3 -805593600000*y1*z3 +
805593600000*y2*z3 + 235685027200*x1 + 398417510400*x2 +
158626915200*x3 -311668424000*y1 -268090368000*y2 + 72704002800*y3 +
412221302400*z1 + 354583756800*z2 + 307085438400*z3 + 282499646407 = 0;

cons9: 3200*x2 + 1271 = 0;

#solve;
#display x1, x2, x3, y1, y2, y3, z1, z2, z3;

# TITLE : parallel robot with 24 real solutions

# ROOT COUNTS :

# total degree : 576
# 3-homogeneous Bezout number : 80
#   with partition : {x1 y1 z1 }{x2 y2 z2 }{x3 y3 z3 }
# generalized Bezout number : 80
#   based on the set structure :
#      {x1 y1 z1 }{x1 y1 z1 }
#      {x2 y2 z2 }{x2 y2 z2 }
#      {x3 y3 z3 }{x3 y3 z3 }
#      {x1 y1 z1 }{x2 y2 z2 }
#      {x1 y1 z1 }{x3 y3 z3 }
#      {x2 y2 z2 }{x3 y3 z3 }
#      {x1 y1 z1 }{x2 y2 z2 }{x3 y3 z3 }
#      {x1 y1 z1 }{x2 y2 z2 }{x3 y3 z3 }
#      {x2 }
# mixed volume : 80

# REFERENCES :

# From the FRISCO test suite, see
# http://www.inria.fr/safir/POL/index.html.

# ORIGIN OF THE PROBLEM

# This system was generated by Fabrice Rouillier, as a deformation of a
# system found in Innocenti (?).
# Added to the list by Jean-Charles Faug\`ere (October 1996).

# NOTE : this system needs scaling, see the files rbpl24s and rbpl24es.

# THE SOLUTIONS : 

# 40 9
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  2.60927915037453E-01   8.51188283602750E-01
#  y1 :  2.96903318580811E-01  -2.23600559834163E-01
#  z1 :  1.35057031555164E+00  -1.15292801937443E-01
#  x2 : -3.97187500000000E-01  -8.80566557837057E-32
#  y2 :  1.58367243700197E+00   2.31508118725500E-01
#  z2 : -9.67226981458723E-01   3.79055830323113E-01
#  x3 :  5.01336328968350E-01   1.89993451405436E+00
#  y3 : -9.25382696154252E-01   2.75038127785784E-01
#  z3 :  6.37698531378697E-01  -6.16577442932776E-01
# ===========================================================
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -1.76222522188102E+00   2.25601382968529E+00
#  y1 : -5.30726405899260E-01   3.40974976009772E-01
#  z1 : -2.43443218060270E+00  -1.70740796487254E+00
#  x2 : -3.97187500000000E-01   7.82610947147733E-32
#  y2 :  1.80879716576422E+00   1.10392481395847E-02
#  z2 :  1.04757018233379E-01  -1.90610243435568E-01
#  x3 : -3.13207074319612E-01   1.62600891961031E+00
#  y3 :  4.49105154709594E-01   1.67128564166409E+00
#  z3 : -2.91412061063636E+00  -8.93146449655552E-01
# ===========================================================
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -8.68845002714886E-01  -1.97993282137966E-46
#  y1 : -3.77102100144573E-01   2.96749857570171E-46
#  z1 :  5.43476188368840E-01  -1.58606456004747E-46
#  x2 : -3.97187500000000E-01  -4.90899642893848E-47
#  y2 :  1.12013020914507E+00   2.39144497698835E-46
#  z2 : -1.41123187478329E+00  -3.60896816291846E-46
#  x3 : -1.24213654470921E+00  -2.55851221860625E-46
#  y3 :  1.82329219297354E-01   3.11488972414872E-47
#  z3 :  4.54819414781284E-01  -1.56000144275981E-46
# ===========================================================
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  9.80796179012321E-01   2.75870639778900E-44
#  y1 :  4.03173199840368E-01   7.59679635379635E-44
#  z1 : -2.60680314111560E-01   2.77057489265704E-43
#  x2 : -3.97187500000000E-01  -1.49669846677857E-45
#  y2 :  3.68572446290782E-02  -1.18615670858622E-42
#  z2 :  1.80136299322543E+00  -1.33170925211691E-43
#  x3 :  1.28892076284168E+00  -4.22154516070031E-44
#  y3 :  3.57139933658504E-01   4.95890444084475E-44
#  z3 :  3.41905928051848E-01   3.19488295477207E-43
# ===========================================================
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -7.53351901004135E-01  -4.36525664759936E-35
#  y1 :  5.82986827304929E-01  -1.21080438668871E-35
#  z1 : -5.33901931484037E-01   6.09081472377513E-35
#  x2 : -3.97187500000000E-01   4.47714270126398E-38
#  y2 : -1.79224079004278E+00   3.51787845347434E-35
#  z2 :  1.84770236646933E-01   1.76704562645256E-34
#  x3 : -6.19359113296997E-01  -3.62627968075898E-35
#  y3 :  1.10131615707616E-01  -3.26431727699111E-35
#  z3 : -1.00147425926630E+00   8.57619862830868E-35
# ===========================================================
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  3.41111035814943E+00  -1.05571993852573E+01
#  y1 : -1.11444903155913E+01  -3.60166955233149E+00
#  z1 : -1.20781906139012E+00   3.41690191995156E+00
#  x2 : -3.97187500000000E-01   7.60011165303484E-30
#  y2 :  6.03162719189098E-02   9.90019647861061E-01
#  z2 :  2.05514286444033E+00  -2.90560307600381E-02
#  x3 :  5.00047689378091E+00   5.61215031457144E+00
#  y3 :  5.84169934021285E+00  -2.66885185496370E+00
#  z3 :  4.24607581166635E+00  -4.20041425584203E+00
# ===========================================================
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  9.24832555668770E-01  -3.10875852060892E-42
#  y1 : -5.61397427180790E-01  -1.62064988880990E-42
#  z1 : -1.48262175655007E-01   5.88508322026460E-42
#  x2 : -3.97187500000000E-01  -3.44951033686745E-42
#  y2 :  1.20364727903326E+00  -9.79535862574426E-42
#  z2 : -1.34070888619401E+00  -1.18258668762512E-41
#  x3 :  9.93173184314215E-01  -5.23983302370560E-42
#  y3 :  8.76873571479193E-02  -3.31723296062942E-42
#  z3 :  3.66391527050073E-02   1.02236254552706E-42
# ===========================================================
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.51664652259260E-01   1.18435203899897E-01
#  y1 :  9.43285027311184E-01  -7.62481850160367E-02
#  z1 : -4.44938486415369E-01  -2.81874394549227E-01
#  x2 : -3.97187500000000E-01   3.58839527633856E-32
#  y2 : -2.70543524413652E-01   6.36857625120939E-01
#  z2 :  1.89392111946294E+00   9.09740668073788E-02
#  x3 : -8.62910696426389E-01  -6.74601131887605E-01
#  y3 : -1.71699847062901E-01   2.13799541238411E-01
#  z3 : -4.50529715455257E-01   2.07146594145172E-01
# ===========================================================
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -5.13765913689658E-01   9.29248470834188E-44
#  y1 : -4.02040338567713E-01  -1.13951945306946E-43
#  z1 :  8.75712368357895E-01   4.77685326320178E-44
#  x2 : -3.97187500000000E-01   8.65728792445681E-45
#  y2 :  1.75913297406388E+00   1.72184038343161E-44
#  z2 : -3.89510294350373E-01   1.27035679334730E-43
#  x3 : -1.90107783864834E-01   2.12868216588040E-43
#  y3 :  1.27320250792110E-01  -1.59482353876415E-44
#  z3 :  1.14992539035157E+00   7.98720738693017E-44
# ===========================================================
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  9.47847985949589E-01   7.24475898715818E-41
#  y1 :  5.18234841145665E-01   1.94477986657187E-41
#  z1 :  1.59627206183739E-01  -1.95659909660745E-41
#  x2 : -3.97187500000000E-01   4.11137396205979E-41
#  y2 : -1.29010194920263E+00  -1.25380590409526E-40
#  z2 :  1.25773767157836E+00  -7.09552012575073E-41
#  x3 :  1.07676727911179E+00  -2.51511985137869E-41
#  y3 :  3.52370234126145E-01  -5.81791626941160E-41
#  z3 :  8.04637863451363E-01  -6.54312029137321E-41
# ===========================================================
# solution 11 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -6.31242531037861E-01  -4.89815810847513E-34
#  y1 :  2.11799613513023E-01   1.22354969602228E-34
#  z1 : -8.65527463876592E-01   3.38521070963502E-34
#  x2 : -3.97187500000000E-01   2.89453109420001E-36
#  y2 : -1.57196053617992E+00   1.19145664409642E-34
#  z2 :  8.80458495635476E-01   2.97607684455168E-34
#  x3 : -1.13806647386946E+00  -3.34057279682040E-34
#  y3 :  1.06388212448939E-01   7.70592930961836E-35
#  z3 : -4.27135028382625E-01   4.28809931415435E-34
# ===========================================================
# solution 12 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  6.45762200930782E-01  -3.46018339685167E-41
#  y1 : -1.67451146323380E-01  -2.02581236101237E-42
#  z1 : -8.64531834835485E-01   6.03794252468706E-42
#  x2 : -3.97187500000000E-01   2.68096658305760E-43
#  y2 :  1.76992806959149E+00  -1.46930379386164E-42
#  z2 :  3.37078208010970E-01   7.39116679765701E-42
#  x3 :  7.16698057815403E-02  -1.67674656758579E-41
#  y3 :  1.03033687641844E-01   5.10343532404525E-43
#  z3 : -1.10416880282373E+00   1.38018943646154E-41
# ===========================================================
# solution 13 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  2.90068765115185E+00   7.99938295244261E+00
#  y1 : -5.89159624289682E+00   5.36046990884114E+00
#  z1 : -7.22070463327437E+00  -1.16027637932974E+00
#  x2 : -3.97187500000000E-01   6.82289781171077E-32
#  y2 :  1.31524734188190E+00   4.24787304085616E-01
#  z2 : -1.36538336113613E+00   4.09189381141201E-01
#  x3 : -7.48368182081418E-01  -4.07901426555947E+00
#  y3 :  6.08132720673979E+00   4.50593810884582E-01
#  z3 : -2.41027247199097E+00   1.89783240390497E+00
# ===========================================================
# solution 14 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -1.76222522188102E+00  -2.25601382968529E+00
#  y1 : -5.30726405899260E-01  -3.40974976009772E-01
#  z1 : -2.43443218060270E+00   1.70740796487254E+00
#  x2 : -3.97187500000000E-01  -1.52100241508788E-31
#  y2 :  1.80879716576422E+00  -1.10392481395847E-02
#  z2 :  1.04757018233379E-01   1.90610243435567E-01
#  x3 : -3.13207074319611E-01  -1.62600891961030E+00
#  y3 :  4.49105154709598E-01  -1.67128564166409E+00
#  z3 : -2.91412061063636E+00   8.93146449655552E-01
# ===========================================================
# solution 15 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  2.60927915037453E-01  -8.51188283602750E-01
#  y1 :  2.96903318580811E-01   2.23600559834163E-01
#  z1 :  1.35057031555164E+00   1.15292801937443E-01
#  x2 : -3.97187500000000E-01   5.35455469992301E-32
#  y2 :  1.58367243700197E+00  -2.31508118725501E-01
#  z2 : -9.67226981458723E-01  -3.79055830323114E-01
#  x3 :  5.01336328968349E-01  -1.89993451405436E+00
#  y3 : -9.25382696154252E-01  -2.75038127785783E-01
#  z3 :  6.37698531378698E-01   6.16577442932776E-01
# ===========================================================
# solution 16 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -8.59361304931823E-01   7.08645559675221E-36
#  y1 : -1.14536036212683E-01   1.27453093335654E-35
#  z1 :  6.63960574126625E-01  -4.40782644483727E-37
#  x2 : -3.97187500000000E-01  -2.26917011589995E-39
#  y2 :  6.80577142510713E-01  -2.67050627125059E-35
#  z2 : -1.66825712734449E+00  -6.58767009861700E-36
#  x3 : -2.36579109387859E-01  -5.49436315266512E-36
#  y3 : -1.86544310915698E-01   8.02700969751911E-37
#  z3 :  4.04871505874547E-01   1.20602793210591E-36
# ===========================================================
# solution 17 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -2.06924880449655E-01  -1.13537267709195E-41
#  y1 :  1.12822207973228E-01   3.84904348592350E-42
#  z1 : -1.06626321480155E+00   3.97434191498388E-42
#  x2 : -3.97187500000000E-01  -7.98704627869241E-44
#  y2 : -1.66317221622769E+00   8.57093879752627E-43
#  z2 :  6.92910722252173E-01   1.29345418958998E-42
#  x3 : -7.92572473873798E-01  -8.64572448911427E-42
#  y3 :  3.59141187994599E-02   1.84999530496641E-42
#  z3 : -7.32688572275859E-01   5.36740336401709E-42
# ===========================================================
# solution 18 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.49613996223585E-01  -2.19640547651717E-43
#  y1 : -8.08220016454984E-01  -4.30485126715128E-43
#  z1 : -5.80595951933320E-01   1.91074130528072E-44
#  x2 : -3.97187500000000E-01   9.49508998166231E-45
#  y2 : -2.85694288028108E-02  -1.83662974232705E-43
#  z2 :  1.80151349636400E+00   9.23895849707127E-44
#  x3 : -4.86395530854822E-01   1.30995825592640E-43
#  y3 : -1.44236229493533E-01  -4.78447061629241E-44
#  z3 : -4.46645777008107E-01   6.38976590954415E-44
# ===========================================================
# solution 19 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.70249860420889E-01  -7.95842181275884E-40
#  y1 : -2.67972394831736E-01   1.47479139881701E-40
#  z1 : -9.48430210601854E-01   2.73923873525043E-40
#  x2 : -3.97187500000000E-01   1.06975034941637E-40
#  y2 :  1.69609054813755E+00   1.56725738011908E-41
#  z2 :  6.07901260372304E-01   1.89213870020019E-40
#  x3 : -8.95358221355674E-01  -6.03628764330885E-40
#  y3 :  2.51314507461867E-01  -3.26619860738897E-41
#  z3 : -1.04739482331590E+00   3.31245464750769E-40
# ===========================================================
# solution 20 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.12970217447204E+00  -2.85539557121356E-01
#  y1 : -4.81112547431015E-01   4.77265621986960E-01
#  z1 : -7.41109017768564E-01  -7.45090296479987E-01
#  x2 : -3.97187500000000E-01   5.02638552979145E-31
#  y2 : -2.51735932126628E+00  -4.63096963472948E-01
#  z2 :  6.42763329149331E-01  -1.81370250103035E+00
#  x3 :  3.69517980821657E-01  -4.12224408148848E-01
#  y3 : -4.03690539639678E-01   1.17105598424810E-01
#  z3 : -4.57678328543319E-01  -9.86487970679366E-01
# ===========================================================
# solution 21 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.02465879869054E+00   1.05596417140249E-43
#  y1 :  3.56291264945708E-02  -1.07621281678782E-43
#  z1 :  3.75857568250670E-01  -1.91074130528072E-44
#  x2 : -3.97187500000000E-01   2.22017545159457E-44
#  y2 :  1.26462869909052E+00  -3.06104957054509E-44
#  z2 : -1.28334770942265E+00  -0.00000000000000E+00
#  x3 :  3.79655252330549E-01  -1.63744781990800E-44
#  y3 : -1.53848099986028E-01  -5.98058827036554E-45
#  z3 :  2.85089700432471E-01   0.00000000000000E+00
# ===========================================================
# solution 22 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.20201863228470E-01   3.60925253897334E-48
#  y1 :  6.50130738926866E-01  -5.60269913381182E-48
#  z1 :  3.11549235047937E-01  -4.66489576484549E-48
#  x2 : -3.97187500000000E-01   1.09088809531966E-48
#  y2 :  6.53035213444466E-02   1.12098983296329E-47
#  z2 : -1.80055617517026E+00  -5.63901275456011E-48
#  x3 :  9.81087705339315E-01   9.99418835393063E-48
#  y3 :  7.97362227120991E-02   3.65026139548677E-48
#  z3 : -1.85050660608453E-02  -0.00000000000000E+00
# ===========================================================
# solution 23 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  4.24033336703691E-01  -6.75817069697591E-42
#  y1 : -6.84488003521169E-01  -1.62064988880990E-42
#  z1 : -7.37655680110672E-01  -4.89149774151862E-42
#  x2 : -3.97187500000000E-01  -3.48525655797488E-42
#  y2 : -7.68706520759768E-01   1.95907172514886E-42
#  z2 :  1.62952673337541E+00   2.95646671906281E-42
#  x3 :  6.42714680699141E-02  -7.27026832039152E-42
#  y3 : -1.64821139715059E-01   5.10343532404525E-43
#  z3 : -4.91306845043299E-01  -3.06708763658120E-42
# ===========================================================
# solution 24 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  3.49674132550947E+00   1.30129037752096E+00
#  y1 : -2.03108584682287E+00   2.95027772577835E-02
#  z1 :  1.16619457280153E+00  -3.85043222714373E+00
#  x2 : -3.97187500000000E-01   3.18755509853904E-31
#  y2 : -1.89241784269248E+00   1.73249266404645E-01
#  z2 : -4.57277000068823E-01  -7.16983366598777E-01
#  x3 :  3.69016259239375E+00   1.63894913563023E+00
#  y3 : -1.20089422667598E+00   7.37413413625633E-02
#  z3 :  1.39320947414899E+00  -4.29990435152624E+00
# ===========================================================
# solution 25 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.05989335239952E+00  -8.65045849212916E-42
#  y1 : -2.44995777916945E-01  -1.94477986657187E-41
#  z1 : -9.52215854844104E-02   5.38064751567049E-41
#  x2 : -3.97187500000000E-01  -9.50402653693917E-42
#  y2 :  1.79373551975195E+00  -5.48540083041679E-41
#  z2 : -1.69646618073993E-01  -1.89213870020019E-40
#  x3 :  1.09864715987124E+00  -4.61105306086093E-41
#  y3 :  2.10483876737831E-01  -2.85792378146534E-41
#  z3 :  4.05983389153723E-01  -2.45367010926496E-41
# ===========================================================
# solution 26 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  1.12970217447204E+00   2.85539557121357E-01
#  y1 : -4.81112547431014E-01  -4.77265621986961E-01
#  z1 : -7.41109017768565E-01   7.45090296479987E-01
#  x2 : -3.97187500000000E-01   9.01342329179742E-32
#  y2 : -2.51735932126628E+00   4.63096963472947E-01
#  z2 :  6.42763329149330E-01   1.81370250103035E+00
#  x3 :  3.69517980821657E-01   4.12224408148848E-01
#  y3 : -4.03690539639678E-01  -1.17105598424810E-01
#  z3 : -4.57678328543319E-01   9.86487970679366E-01
# ===========================================================
# solution 27 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.51664652259260E-01  -1.18435203899896E-01
#  y1 :  9.43285027311184E-01   7.62481850160367E-02
#  z1 : -4.44938486415369E-01   2.81874394549227E-01
#  x2 : -3.97187500000000E-01  -7.06605585241396E-32
#  y2 : -2.70543524413651E-01  -6.36857625120939E-01
#  z2 :  1.89392111946294E+00  -9.09740668073787E-02
#  x3 : -8.62910696426389E-01   6.74601131887605E-01
#  y3 : -1.71699847062901E-01  -2.13799541238411E-01
#  z3 : -4.50529715455257E-01  -2.07146594145172E-01
# ===========================================================
# solution 28 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -7.96340607593661E-01   1.14186052096105E-38
#  y1 :  6.45101520466892E-01   9.43866495242882E-39
#  z1 : -3.77027406150615E-01  -1.06830310674767E-38
#  x2 : -3.97187500000000E-01   1.12189514945684E-40
#  y2 : -8.80272341950868E-01   7.89897719580019E-38
#  z2 :  1.57206478678205E+00   5.37367390856856E-38
#  x3 : -8.67859631690733E-01  -2.08587273007673E-38
#  y3 : -2.45793504776309E-02   2.67828285805895E-39
#  z3 : -4.58160385161520E-01   2.05453977149119E-38
# ===========================================================
# solution 29 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  3.49674132550946E+00  -1.30129037752096E+00
#  y1 : -2.03108584682287E+00  -2.95027772577844E-02
#  z1 :  1.16619457280153E+00   3.85043222714372E+00
#  x2 : -3.97187500000000E-01  -2.15547444640150E-30
#  y2 : -1.89241784269249E+00  -1.73249266404647E-01
#  z2 : -4.57277000068827E-01   7.16983366598781E-01
#  x3 :  3.69016259239373E+00  -1.63894913563023E+00
#  y3 : -1.20089422667598E+00  -7.37413413625644E-02
#  z3 :  1.39320947414899E+00   4.29990435152623E+00
# ===========================================================
# solution 30 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -8.53729869852464E-01  -4.75775217067104E-40
#  y1 :  5.56043842813968E-01  -9.07563937733543E-41
#  z1 : -3.92968897230255E-01   6.94592679295645E-40
#  x2 : -3.97187500000000E-01  -7.88204175418935E-42
#  y2 : -1.69349464221869E+00   8.03219407311029E-40
#  z2 :  6.15095916601908E-01   2.51890964464152E-39
#  x3 : -7.06354608096206E-01  -4.61105306086093E-40
#  y3 :  3.29255260953962E-02  -4.12357574182857E-40
#  z3 : -7.98921799591847E-01   1.12050934989766E-39
# ===========================================================
# solution 31 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.64657630748941E-02   1.82086718997155E+00
#  y1 : -2.60013311576901E-01  -2.44191989277683E-01
#  z1 :  2.12212319368553E+00  -1.04110750727843E-01
#  x2 : -3.97187500000000E-01   1.96056646172370E-31
#  y2 :  1.87677828745565E+00   5.47072413549543E-02
#  z2 : -1.85203921491711E-01   5.54380068816036E-01
#  x3 :  7.94652470812097E-01   2.72061893048497E+00
#  y3 : -1.26866634186489E+00   6.13793896384178E-01
#  z3 :  1.36239312623849E+00  -4.04504075147711E-01
# ===========================================================
# solution 32 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  2.90068765115185E+00  -7.99938295244261E+00
#  y1 : -5.89159624289682E+00  -5.36046990884114E+00
#  z1 : -7.22070463327437E+00   1.16027637932974E+00
#  x2 : -3.97187500000000E-01   9.52901543714065E-32
#  y2 :  1.31524734188190E+00  -4.24787304085616E-01
#  z2 : -1.36538336113613E+00  -4.09189381141201E-01
#  x3 : -7.48368182081418E-01   4.07901426555947E+00
#  y3 :  6.08132720673979E+00  -4.50593810884582E-01
#  z3 : -2.41027247199097E+00  -1.89783240390497E+00
# ===========================================================
# solution 33 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -4.55699779552188E-01   9.89966410689834E-46
#  y1 : -6.90618194317837E-01  -0.00000000000000E+00
#  z1 :  7.12634703472443E-01   2.23914996712583E-46
#  x2 : -3.97187500000000E-01   8.72710476255727E-48
#  y2 : -3.40997139038411E-01  -1.13593636406947E-45
#  z2 : -1.76917722148217E+00   1.97365446409604E-46
#  x3 : -6.63399245758240E-01   7.67553665581873E-46
#  y3 : -9.85596027349678E-02  -4.67233458622309E-46
#  z3 :  4.54763503315025E-01   5.52283654438976E-46
# ===========================================================
# solution 34 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  9.93052559517862E-01  -8.00222848640946E-46
#  y1 :  4.34015781147816E-01  -1.83613974371544E-45
#  z1 : -1.33943703658158E-01  -9.81494068923492E-45
#  x2 : -3.97187500000000E-01   8.18166071489744E-47
#  y2 : -5.02507786785961E-01   3.87414086272112E-44
#  z2 :  1.73024651829248E+00   7.27432645338253E-45
#  x3 :  1.26337434367337E+00   2.93029602537247E-45
#  y3 :  3.73997952644466E-01  -7.28105473019764E-46
#  z3 :  4.85325214722802E-01  -1.09200100993186E-44
# ===========================================================
# solution 35 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -8.35155706358641E-01  -8.24972008908190E-47
#  y1 :  5.91094683545441E-01  -1.05098907889436E-46
#  z1 :  3.81557362949416E-01   1.67936247534437E-46
#  x2 : -3.97187500000000E-01   3.05448666689505E-47
#  y2 : -1.76908191866830E+00   3.58716746548252E-46
#  z2 : -3.41491222265833E-01  -5.75179300965130E-46
#  x3 : -8.17068047668791E-01   6.39628054651563E-47
#  y3 :  9.80941487574727E-02  -1.58664695323825E-46
#  z3 :  8.47168047214600E-01  -1.40400129848382E-46
# ===========================================================
# solution 36 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -8.17377439151422E-01  -8.07855938029755E-36
#  y1 :  3.30749510989353E-01  -5.43799864898791E-35
#  z1 :  6.44176127271546E-01   1.28227678395266E-36
#  x2 : -3.97187500000000E-01   1.96466807616954E-37
#  y2 : -2.54264100258083E-02   8.21694237307873E-35
#  z2 : -1.80156059779208E+00   2.40256203596620E-35
#  x3 : -3.92866299098061E-01   1.86808347190614E-35
#  y3 : -1.60367744054177E-01  -2.54188640421439E-36
#  z3 :  4.47290033884782E-01  -3.88609000345238E-36
# ===========================================================
# solution 37 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  8.64657630748941E-02  -1.82086718997155E+00
#  y1 : -2.60013311576900E-01   2.44191989277683E-01
#  z1 :  2.12212319368553E+00   1.04110750727844E-01
#  x2 : -3.97187500000000E-01   1.52752161623388E-32
#  y2 :  1.87677828745565E+00  -5.47072413549544E-02
#  z2 : -1.85203921491711E-01  -5.54380068816036E-01
#  x3 :  7.94652470812097E-01  -2.72061893048497E+00
#  y3 : -1.26866634186489E+00  -6.13793896384178E-01
#  z3 :  1.36239312623849E+00   4.04504075147712E-01
# ===========================================================
# solution 38 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 : -8.42970867656280E-01  -3.78457559030651E-42
#  y1 :  6.08348452846329E-01  -2.65887872382873E-42
#  z1 : -3.34329592172590E-01   3.97434191498388E-42
#  x2 : -3.97187500000000E-01   1.09472802141519E-43
#  y2 : -1.19919019185755E+00  -2.10600210453502E-41
#  z2 :  1.34469698207306E+00  -1.13639189513977E-41
#  x3 : -7.84565608446585E-01   6.28779962844672E-42
#  y3 : -3.40734253075663E-02  -1.36357412564334E-42
#  z3 : -5.44217553156221E-01  -4.60063145487179E-42
# ===========================================================
# solution 39 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  3.41111035814941E+00   1.05571993852573E+01
#  y1 : -1.11444903155913E+01   3.60166955233147E+00
#  z1 : -1.20781906139012E+00  -3.41690191995156E+00
#  x2 : -3.97187500000000E-01   4.87377656061346E-33
#  y2 :  6.03162719189093E-02  -9.90019647861061E-01
#  z2 :  2.05514286444033E+00   2.90560307600379E-02
#  x3 :  5.00047689378091E+00  -5.61215031457144E+00
#  y3 :  5.84169934021285E+00   2.66885185496369E+00
#  z3 :  4.24607581166635E+00   4.20041425584203E+00
# ===========================================================
# solution 40 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 1
# the solution for t :
#  x1 :  7.39531070933881E-01   3.37908534848795E-43
#  y1 : -7.29179210834609E-01   2.57183209894148E-43
#  z1 :  3.37424767333409E-01  -7.64296522112285E-44
#  x2 : -3.97187500000000E-01   1.73145758489137E-44
#  y2 : -7.79438832082112E-02   9.18314871163523E-44
#  z2 : -1.80005328835403E+00   5.77434906066955E-45
#  x3 :  3.92434393156963E-01   2.61991651185280E-43
#  y3 : -1.47049721511945E-01   1.79417648110966E-44
#  z3 :  3.08803901917405E-01  -3.19488295477208E-44
# ===========================================================
