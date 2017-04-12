# Objective: convex nonlinear
# Constraints: bounds
# Feasible set: convex

param n0 := 31;
param n  := n0+1;

set X := {0..n};
set Y := {0..n};

set X0 := {0..n0};
set Y0 := {0..n0};

param xsize := 2;
param ysize := 2;

param hx := xsize/n;
param hy := ysize/n;

param gamma0 {x in X} := 1.5*x*(n-x)/(n/2)^2;
param gamma1 {y in Y} := 2*y*(n-y)/(n/2)^2;
param gamma2 {x in X} := 4*x*(n-x)/(n/2)^2;
param gamma3 {y in Y} := 2*y*(n-y)/(n/2)^2;

#param gamma0 {x in X} := 2*(if (x <= n/2) then x else (n-x))/(n/2);
#param gamma1 {y in Y} := 0*(if (y <= n/2) then y else (n-y))/(n/2);
#param gamma2 {x in X} := 2*(if (x <= n/2) then x else (n-x))/(n/2);
#param gamma3 {y in Y} := 0*(if (y <= n/2) then y else (n-y))/(n/2);

var z {X, Y};

minimize area: 
   (hx*hy/2)*
   sum {x in X0, y in Y0}
      (
      sqrt(1 + ((z[x+1,y] - z[x,y])/hx)^2 + ((z[x,y+1] - z[x,y])/hx)^2)
      +
      sqrt(1 + ((z[x+1,y+1] - z[x,y+1])/hx)^2 + ((z[x+1,y+1] - z[x+1,y])/hx)^2)
      );

subject to bndcnd0 {x in X}: z[x,0] = gamma0[x];
subject to bndcnd1 {y in Y}: z[n,y] = gamma1[y];
subject to bndcnd2 {x in X}: z[x,n] = gamma2[x];
subject to bndcnd3 {y in Y}: z[0,y] = gamma3[y];

#solve;

param maxz;
let maxz := max {x in X, y in Y} z[x,y];
printf {x in X, y in Y}: "%7.3f, \n", z[x,y] > height.wrl;
