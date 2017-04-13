
# Domains
var pch      >= 92000.0, <= 98000.0;
var pav      >= 84.5, <= 87.0;
var dal      >= 18.5, <= 19.5;
var deltats  >= 18.0, <= 22.0;
var p1       >= 36.5, <= 39.5;
var p2       >= 15.0, <= 17.0;
var p3       >= 7.5, <= 8.5;
var t1       >= 289.5, <= 290.5;
var tav      >= 284.0, <= 288.0;
var tliq     >= 119.0, <= 121.0;
var tf       >= 181.0, <= 183.0;
var t2       >= 249.5, <= 250.5;
var t3       >= 174.5, <= 175.5;
var tt2      >= 247.0, <= 248.0;
var tt3      >= 171.0, <= 176.0;
var du1      >= 8.75, <= 10.5;
var prt2     >= 15.4, <= 16.8;
var prt3     >= 7.3, <= 8.3;
var dav1     >= 0.0, <= 100.0;
var dav2     >= 0.0, <= 100.0;
var dch      >= 0.0, <= 100.0;
var dfl      >= 0.0, <= 100.0;
var deg      >= 0.0, <= 100.0;
var dt2      >= 0.0, <= 100.0;
var dt3      >= 0.0, <= 100.0;
var dfg      >= 0.0, <= 100.0;
var du2      >= 0.0, <= 100.0;
var du3      >= 0.0, <= 100.0;
var dp1      >= 0.0, <= 100.0;
var dp2      >= 0.0, <= 100.0;
var dp3      >= 0.0, <= 100.0;
var pt2      >= 0.0, <= 200000.0;
var pt3      >= 0.0, <= 200000.0;
var pu1      >= 0.0, <= 200000.0;
var pu2      >= 0.0, <= 200000.0;
var pu3      >= 0.0, <= 200000.0;
var eta2     >= 0.0, <= 1.0;
var eta3     >= 0.0, <= 1.0;
var pf       >= 0.0, <= 100.0;

subject to
cons1 : dav1 + dt2 + dt3 + du1 - dch = 0;

cons2 : dav2 + dav1 + dt2 - du2 - dp1 = 0;

cons3 : dt3 + dfg - dp2 - du3 = 0;

cons4 : du1 + du2 + du3 - dfl - dfg = 0;

cons5 : dfl + dal - deg - dch = 0;

cons6 : dch * (2.0* t1 - 4.18 * tliq + 2400.0) - pch = 0;

cons7 : 2.0 * dt2 * (t1 - tt2) - pt2 = 0;

cons8 : 2.0 * dt3 * ( t1 - tt3) - pt3 = 0;

cons9 : 2.0 * eta2 * dt2 * (t1 + 273.0) * (1.0- exp(0.25 * log(prt2 / p1))) - pt2 = 0;

cons10 : 2.0 * eta3 * dt3 * (t1 + 273.0) * (1.0- exp(0.25 * log(prt3 / p1))) - pt3 = 0;

cons11 : (dav1 + dav2) * (2.0* tav + 2400) - dav1 * (2.0* t1 + 2400.0) - pav = 0;

cons12 : (2800.0+ 2.0* deltats) * dav2 - pav = 0;

cons13 : du1 * (2400.0+ 2.0* t1 - 422.0* exp(0.25 * log(p1))) - pu1 = 0;

cons14 : du2 * (2400.0+ 2.0* t2 - 422.0* exp(0.25 * log(p2))) - pu2 = 0;

cons15 : du3 * (2400.0+ 2.0* t3 - 422.0* exp(0.25 * log(p3))) - pu3 = 0;

cons16 : du1 * (2.0* t1 + 2400.0) + du2 * (2.0* t2 + 2400.0) + du3 * (2.0* t3 +2400.0) - pu1 - pu2 - pu3 - dfl * (4.18 * tf + 0.3) - dfg * (2.0* tf + 2400.0) = 0;

cons17 : dfl * (tf - 20.0) - (dal + dfl) * (tliq - 20.0) = 0;

cons18 : 100.0* exp(0.25 * log(pf / 0.965)) - tf = 0;

cons19 : (dav1 + dav2) * tav + dt2 * tt2 -(dav1 + dav2 + dt2) * t2 =0;

cons20 : dt3 * tt3 + dfg * tf - (dt3 + dfg) * t3=0;


#solve;
#display dal, dav1, dav2, dch, deg, deltats, dfg, dfl, dp1, dp2, dp3, dt2, dt3, du1, du2, du3, eta2, eta3, p1, p2, p3, pav, pch, pf, prt2, prt3, pt2, pt3, pu1, pu2, pu3, t1, t2, t3, tav, tf, tliq, tt2, tt3;
