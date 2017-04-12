# Domains

var X >= -1.0e8, <= 1.0e8;
var Y >= -1.0e8, <= 1.0e8;
var Z >= -1.0e8, <= 1.0e8;

var v1 >= -1.0e8, <= 1.0e8;
var v2 >= -1.0e8, <= 1.0e8;
var v3 >= -1.0e8, <= 1.0e8;
var v4 >= -1.0e8, <= 1.0e8;
var v5 >= -1.0e8, <= 1.0e8;
var v6 >= -1.0e8, <= 1.0e8;
var v7 >= -1.0e8, <= 1.0e8;
var v8 >= -1.0e8, <= 1.0e8;
var v9 >= -1.0e8, <= 1.0e8;
var v10 >= -1.0e8, <= 1.0e8;
var v11 >= -1.0e8, <= 1.0e8;
var v12 >= -1.0e8, <= 1.0e8;
var v20 >= -1.0e8, <= 1.0e8;
var v21 >= -1.0e8, <= 1.0e8;
var v22 >= -1.0e8, <= 1.0e8;
var v23 >= -1.0e8, <= 1.0e8;
var v24 >= -1.0e8, <= 1.0e8;
var v25 >= -1.0e8, <= 1.0e8;
var v26 >= -1.0e8, <= 1.0e8;
var v27 >= -1.0e8, <= 1.0e8;
var v28 >= -1.0e8, <= 1.0e8;
var v29 >= -1.0e8, <= 1.0e8;
var v30 >= -1.0e8, <= 1.0e8;
var v31 >= -1.0e8, <= 1.0e8;
var v40 >= -1.0e8, <= 1.0e8;
var v41 >= -1.0e8, <= 1.0e8;

subject to
cons1 : v1 = X^9;
cons2 : v2 = 5*v1;
cons3 : v3 = X^5;

cons4 : v4 = Y^2;
cons5 : v5 = v3*v4;
cons6 : v6 = 6*v5;
cons7 : v7 = Y^4;
cons8 : v8 = X*v7;
cons9 : v9 = X*Z;
cons10 : v10 = 2*v9;
cons11 : v11 = v2-v6;
cons12 : v12 = v11 + v8;
cons13 : v12 + v10 = 0;



cons14 : v20 = X^9;
cons15 : v21 = 5*v20;
cons16 : v22 = X^5;
cons17 : v23 = Y^2;
cons18 : v24 = v22*v23;
cons19 : v25 = 6*v24;
cons20 : v26 = v21 - v25;

cons21 : v27 = Y^4;
cons22 : v28 = X*v27;

cons23 : v29 = v26 + v28;

cons24 : v30 = X*Z;

cons25 : v31 = 2*v30;

cons26 : v29 + v31 = 0;





cons27 : v40 = X^2;
cons28 : v41 = Y^2;
cons29 : v40 + v41 = 0.265625;

#solve;
#display X, Y, Z, v1, v10, v11, v12, v2, v20, v21, v22, v23, v24, v25, v26, v27, v28, v29, v3, v30, v31, v4, v40, v41, v5, v6, v7, v8, v9;
