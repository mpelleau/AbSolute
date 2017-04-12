# Objective: convex quadratic
# Constraints: nonconvex nonlinear

#Knowing a list of points $(x_i, y_i)$, describing the center line of 
#a wood piece, we want to fit the best polynomial that minimizes the 
#sum of the squares of errors (least square problem). This part is 
#simple. But we have to deal with three sets of conditions : 
#
#1) the polynomial must go througn the first point $(x_0, y_0)$  of 
#   the list (easy...);
#2) the initial slope of the curve $m_0$ is also known (easy too...); 
#
#3) the radius of curvature at every point must exceed a given radius 
#   $R$. In fact, I modified this last condition. I prefer choosing a 
#   condition about the square of the radius to avoid square root and 
#   absolute value functions. If this had not been satisfying, I would 
#   have tried the exact condition.
#
#From:
#
#Francois Grondin, Ph.D.
#Forintek Canada Corp.
#319, rue Franquet, Quebec, QC
#G1P 4R4
#Tel.: (418) 659-2647
#Fax : (418) 659-2922
#E-Mail : francois.grondin@qc.forintek.ca

param n;
param R;
param m_0;

param x {0..n};
param y {0..n};

var a {0..4};
var p {j in 0..n} = sum {i in 0..4} a[i]*x[j]^i;
var pprime  {j in 0..n} = sum {i in 1..4} i*a[i]*x[j]^(i-1);
var pprime2 {j in 0..n} = sum {i in 2..4} i*(i-1)*a[i]*x[j]^(i-2);

param best_val_found := 181.5729928;
param eps := 1.815729928; 		# = max(1, 1% x best_val_found)

subject to z: sum {j in 0..n} (p[j] - y[j])^2 <= best_val_found + eps;

subject to init_val: p[0] = y[0];
subject to init_der: pprime[0] = m_0;
subject to curvature {j in 0..n}: R^2 * pprime2[j]^2 <= ( 1 + pprime[j]^2)^3;

data;

param n := 195;
param R := 2500;
param: x y := 
   0     0.000000     3.556000 
   1     2.540000     3.563460 
   2     5.080000     3.570910 
   3     7.620010     3.578370 
   4    10.161200     3.593710 
   5    12.701200     3.602130 
   6    15.241200     3.610550 
   7    17.781200     3.618980 
   8    20.321200     3.627400 
   9    22.861200     3.635820 
  10    25.401200     3.642300 
  11    27.941200     3.650730 
  12    30.481200     3.665300 
  13    33.021200     3.673810 
  14    35.561200     3.682330 
  15    38.101200     3.690840 
  16    40.641100     3.506370 
  17    43.181100     3.514980 
  18    45.723100     2.774810 
  19    48.263100     2.783760 
  20    50.803100     2.792720 
  21    53.343100     2.801680 
  22    55.883900     2.785290 
  23    58.423900     2.794150 
  24    60.965000     2.476020 
  25    63.505000     2.486170 
  26    66.045000     2.483690 
  27    68.585000     2.493770 
  28    71.125000     2.276350 
  29    73.665000     2.285210 
  30    76.205000     2.267440 
  31    78.745000     2.276050 
  32    81.285000     2.284650 
  33    83.825000     2.293250 
  34    86.365000     2.490700 
  35    88.905000     2.499210 
  36    91.445000     2.507730 
  37    93.985000     2.516240 
  38    96.525000     2.533810 
  39    99.065000     2.542410 
  40   101.605000     2.748550 
  41   104.145000     2.757160 
  42   106.685000     2.765760 
  43   109.225000     2.774370 
  44   111.766000     2.607580 
  45   114.306000     2.616100 
  46   116.847000     2.260080 
  47   119.387000     2.268600 
  48   121.927000     2.286950 
  49   124.467000     2.295560 
  50   127.008000     1.478080 
  51   129.548000     1.485400 
  52   132.090000     1.110100 
  53   134.630000     1.118800 
  54   137.170000     1.127490 
  55   139.710000     1.136190 
  56   142.251000     0.978377 
  57   144.791000     0.987075 
  58   147.330000     0.822030 
  59   149.870000     0.829676 
  60   152.410000     0.837322 
  61   154.950000     0.844885 
  62   157.490000     0.852781 
  63   160.030000     0.860510 
  64   162.571000     0.538377 
  65   165.111000     0.546996 
  66   167.651000     0.600516 
  67   170.191000     0.609224 
  68   172.731000     0.789128 
  69   175.271000     0.798084 
  70   177.811000     1.009960 
  71   180.351000     1.018830 
  72   182.891000     1.037120 
  73   185.431000     1.045910 
  74   187.972000     0.720837 
  75   190.512000     0.699625 
  76   193.052000     0.708414 
  77   195.592000     0.717203 
  78   198.133000     0.609086 
  79   200.673000     0.617627 
  80   203.213000     0.199369 
  81   205.753000     0.207992 
  82   208.293000     0.225288 
  83   210.833000     0.233831 
  84   213.374000    -0.172432 
  85   215.914000    -0.163889 
  86   218.456000    -0.348890 
  87   220.996000    -0.341233 
  88   223.536000    -0.333576 
  89   226.076000    -0.325920 
  90   228.616000    -0.522016 
  91   231.156000    -0.514426 
  92   233.696000    -0.506837 
  93   236.236000    -0.499248 
  94   238.776000    -0.491658 
  95   241.316000    -0.484069 
  96   243.856000    -0.476480 
  97   246.396000    -0.468890 
  98   248.936000    -0.462453 
  99   251.476000    -0.455008 
 100   254.016000    -0.447563 
 101   256.556000    -0.440118 
 102   259.095000     0.055237 
 103   261.635000     0.063873 
 104   264.176000     0.062826 
 105   266.716000     0.070575 
 106   269.256000    -0.275144 
 107   271.797000    -0.275842 
 108   274.337000    -0.268089 
 109   276.877000    -0.260335 
 110   279.417000    -0.118406 
 111   281.957000    -0.110665 
 112   284.497000    -0.102923 
 113   287.037000    -0.095181 
 114   289.577000     0.103362 
 115   292.117000     0.111028 
 116   294.657000     0.131239 
 117   297.197000     0.138981 
 118   299.736000     0.272430 
 119   302.276000     0.280024 
 120   304.815000     0.421614 
 121   307.355000     0.429144 
 122   309.895000     0.436674 
 123   312.435000     0.444203 
 124   314.976000     0.484027 
 125   317.516000     0.491804 
 126   320.056000     0.515482 
 127   322.598000     0.571256 
 128   325.138000     0.580224 
 129   327.678000     0.589193 
 130   330.215000     1.111820 
 131   332.755000     1.120710 
 132   335.295000     1.345510 
 133   337.835000     1.354480 
 134   340.375000     1.383820 
 135   342.915000     1.392870 
 136   345.454000     1.772980 
 137   347.994000     1.782020 
 138   350.534000     2.006330 
 139   353.074000     2.015440 
 140   355.615000     2.412930 
 141   358.155000     2.420930 
 142   360.695000     2.390290 
 143   363.235000     2.398230 
 144   365.773000     2.891230 
 145   368.313000     2.899110 
 146   370.853000     3.337140 
 147   373.393000     3.345090 
 148   375.933000     3.404410 
 149   378.473000     3.412480 
 150   381.012000     3.702710 
 151   383.552000     3.710940 
 152   386.090000     4.153530 
 153   388.632000     4.127350 
 154   391.170000     4.560110 
 155   393.710000     4.569230 
 156   396.250000     4.772520 
 157   398.790000     4.781630 
 158   401.329000     4.891490 
 159   403.869000     4.899350 
 160   406.410000     4.964780 
 161   408.950000     4.972720 
 162   411.490000     5.167670 
 163   414.030000     5.175530 
 164   416.570000     5.385770 
 165   419.110000     5.393640 
 166   421.650000     5.401510 
 167   424.190000     5.409370 
 168   426.729000     5.283580 
 169   429.269000     5.291360 
 170   431.810000     5.142030 
 171   434.350000     5.148650 
 172   436.891000     4.800670 
 173   439.431000     4.807290 
 174   441.972000     4.861690 
 175   444.512000     4.869470 
 176   447.052000     4.561590 
 177   449.592000     4.569370 
 178   452.134000     3.788240 
 179   454.674000     3.796020 
 180   457.214000     3.803800 
 181   459.754000     3.811580 
 182   462.295000     3.665340 
 183   464.835000     3.673060 
 184   467.375000     3.676650 
 185   469.915000     3.684500 
 186   472.455000     3.690400 
 187   474.995000     3.698320 
 188   477.535000     3.706240 
 189   480.075000     3.714160 
 190   482.616000     3.501540 
 191   485.156000     3.509630 
 192   487.696000     3.517720 
 193   490.236000     3.525810 
 194   492.776000     3.533900 
 195   495.316000     3.541990 
;

let m_0 := (y[15]-y[0])/(x[15]-x[0]);

#solve;

#display a;

##display x, y, p;
