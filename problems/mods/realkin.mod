param pi := 3.1415;

var
   x {1..6} >= 0, <= 2*pi;

subject to
cons1:    sin(x[2])*cos(x[5])*sin(x[6]) - sin(x[3])*cos(x[5])*sin(x[6]) -
   sin(x[4])*cos(x[5])*sin(x[6]) + cos(x[2])*cos(x[6]) + cos(x[3])*cos(x[6]) + cos(x[4])*cos(x[6]) = 0.4077;
cons2:    cos(x[1])*cos(x[2])*sin(x[5]) + cos(x[1])*cos(x[3])*sin(x[5]) + cos(x[1])*cos(x[4])*sin(x[5]) + sin(x[1])*cos(x[5]) = 1.9115;
cons3:    sin(x[2])*sin(x[5]) + sin(x[3])*sin(x[5]) + sin(x[4])*sin(x[5]) = 1.9791;
cons4:    3*cos(x[1])*cos(x[2]) + 2*cos(x[1])*cos(x[3]) + cos(x[1])*cos(x[4]) = 4.0616;
cons5:    3*sin(x[1])*cos(x[2]) + 2*sin(x[1])*cos(x[3]) + sin(x[1])*cos(x[4]) = 1.7172;
cons6:    3*sin(x[2]) + 2*sin(x[3]) + sin(x[4]) = 3.9701;

#solve;
#display x;
