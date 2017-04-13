# Domains
var k1  >= 0.2, <= 5;
var k2  >=0, <= 0.1;
var k3  >= 0.9, <= 1.11111112;
var DT1 >= 1, <= 1000;
var DT2 >= 0.1, <= 1000;
var DTc >= 0.001, <= 1000;
var DTh >= 0.001, <= 1000;
var Fc  >= 1.0e-20, <= 1.0e20;
var Fh  >= 1.0e-20, <= 1.0e20;
var Kc  >= 1.0e-20, <= 1.0e20;
var Kh  >= 1.0e-20, <= 1.0e20;

subject to
cons1 : DTh + DT2 = DT1 + DTc;
cons2 : DTh*Kh*Fh = DTc*Kc*Fc;
cons3 : DT2       = k1*DT1;
cons4 : DT1       = k2*DTh;
cons5 : Kh        = k3*Kc;

#solve;
#display DT1, DT2, DTc, DTh, Fc, Fh, Kc, Kh, k1, k2, k3;
