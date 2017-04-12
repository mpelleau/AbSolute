var x{i in 1..1000} := if (i mod 2 == 1) then -3 else -1;

minimize f:
	sum {i in 1..250} (100*(x[4*i-2]-x[4*i-3]^2)^2 + (1-x[4*i-3])^2 + 90*(x[4*i]-x[4*i-1]^2)^2 + (1-x[4*i-1])^2 +
	10*(x[4*i-2]+x[4*i]-2)^2 + 10*(x[4*i-2]-x[4*i])^2 );


#solve;
#display f;
#display x;

