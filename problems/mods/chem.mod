param r := 10;
param r5 := 0.193;
param r6 := 0.002597/sqrt(40);
param r7 := 0.003448/sqrt(40);
param r8 := 0.00001799/40;
param r9 := 0.0002155/sqrt(40);
param r10:= 0.00003846/40;

var
    y  {1..5} >= 0, <= 1e8;

subject to
cons1:        0 = 3*y[5] - y[1]*(y[2] + 1);

cons2:        0 = y[2]*(2*y[1] + y[3]^2 + r8 + 2*r10*y[2] + r7*y[3] + r9*y[4]) +
           y[1] - r*y[5];
    
cons3:        0 = y[3]*(2*y[2]*y[3] + 2*r5*y[3] + r6 + r7*y[2]) - 8*y[5];

cons4:        0 = y[4]*(r9*y[2] + 2*y[4]) - 4*r*y[5];

cons5:        0 = y[2]*(y[1]  + r10*y[2] + y[3]^2 + r8  +r7*y[3] + r9*y[4]) +
           y[1] + r5*y[3]^2 + y[4]^2 - 1 + r6*y[3];

#solve;
##display r, r5, r6, r7, r8, r9, r10;
#display y;
