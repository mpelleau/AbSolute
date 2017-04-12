param T := 10;
param XT := 10;
param mass := 0.5;
param tol := 0.1;

var x {i in 0..T} := i*XT/T, >= 0, <= XT;
var y{0..T};
var vx{i in 0..T} := if (i=0) then 0 else if (i=T) then 0 else 1.0;
var vy{0..T};
var ux{1..T} <= XT/T, >= -XT/T;
var uy{1..T} <= XT/T, >= -XT/T;

param best_val_found := 0.1159369042;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
	sum {i in 1..T} (2*i*(x[i]-XT)/(T*(T+1)))^2 <= best_val_found + eps;

subject to cons1{i in 1..T}:
	mass*T*(vx[i]-vx[i-1])/XT -ux[i] = 0;
subject to cons2{i in 1..T}:
	mass*T*(vy[i]-vy[i-1])/XT -uy[i] = 0;
subject to cons3{i in 1..T}:
	T*(x[i]-x[i-1])/XT -vx[i] = 0;
subject to cons4{i in 1..T}:
	T*(y[i]-y[i-1])/XT -vy[i] = 0;
subject to cons5{i in 1..T}:
	tol <= y[i]-sin(x[i]) <= 2*tol;
subject to cons6:
	x[0] = 0;
subject to cons7:
	y[0] = 0;
subject to cons8:
	vx[0] = 0;
subject to cons9:
	vy[0] = 0;
subject to cons10:
	vx[T] = 0;
subject to cons11:
	vy[T] = 0;


#solve;
##display f;
#display x, y, vx, vy, ux, uy;
