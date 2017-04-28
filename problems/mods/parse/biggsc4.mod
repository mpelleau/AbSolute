var x1 >= 0.0, <= 5.0;
var x2 >= 0.0, <= 5.0;
var x3 >= 0.0, <= 5.0;
var x4 >= 0.0, <= 5.0;

param best_val_found := -24.375;
param eps := 1;

subject to f: (-x1)*x3-x2*x4 <= best_val_found + eps;
subject to f1: 0 <= x1+x2 -2.5;

subject to f2: x1+x2 -2.5<= 5.0;
subject to f3: 0 <= x1+x3 -2.5;
subject to f4: x1+x3 -2.5 <= 5.0;

subject to f5: 0 <= x1+x4 -2.5;
subject to f6: x1+x4 -2.5<= 5.0;

subject to f7: 0 <= x2+x3 -2.0;
subject to f8: x2+x3 -2.0 <= 5.0;

subject to f9: 0 <= x2+x4 -2.0;
subject to f10: x2+x4 -2.0<= 5.0;

subject to f11: 0 <= x3+x4 -1.5;
subject to f12: x3+x4 -1.5<= 5.0;

subject to f13: x1+x2+x3+x4-5.0 >= 0;
