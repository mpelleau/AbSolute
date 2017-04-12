var x1;
var x2;
var x3;
var x4;
var x5;

subject to

cons1:  x1^2*x2^2*x3^2*x4^2*x5^2 
    + 3*x1^2 + x2^2 + x3^2 + x4^2 + x5^2 + x1*x2*x3*x4*x5 + 5 = 0;
cons2:  x1^2*x2^2*x3^2*x4^2*x5^2 
    + x1^2 + 3*x2^2 + x3^2 + x4^2 + x5^2 + x1*x2*x3*x4*x5 + 5 = 0;
cons3:  x1^2*x2^2*x3^2*x4^2*x5^2 
    + x1^2 + x2^2 + 3*x3^2 + x4^2 + x5^2 + x1*x2*x3*x4*x5 + 5 = 0;
cons4:  x1^2*x2^2*x3^2*x4^2*x5^2 
    + x1^2 + x2^2 + x3^2 + 3*x4^2 + x5^2 + x1*x2*x3*x4*x5 + 5 = 0;
cons5:  x1^2*x2^2*x3^2*x4^2*x5^2 
    + x1^2 + x2^2 + x3^2 + x4^2 + 3*x5^2 + x1*x2*x3*x4*x5 + 5 = 0;

#solve;
#display x1, x2, x3, x4, x5;

# TITLE : a 5-dimensional sparse symmetric polynomial system

# ROOT COUNTS :

# Total degree : 10000
# 5-homogeneous Bezout number : 3840
# mixed volume : 160

# REFERENCES :

# Jan Verschelde and Karin Gatermann:
# `Symmetric Newton Polytopes for Solving Sparse Polynomial Systems',
# Adv. Appl. Math., 16(1): 95-127, 1995.

# SYMMETRY GROUP :

# invariant under all permutations
# + sign symmetry, generated by

# -x1 -x2 x3 x4 x5
# -x1 x2 -x3 x4 x5
# -x1 x2 x3 -x4 x5
# -x1 x2 x3 x4 -x5

# THE GENERATING SOLUTIONS :

# 10 5
# ===========================================================
# solution 1 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  5.23681844701480E-01  -1.16679253451397E+00
#  x2 :  5.23681844701480E-01  -1.16679253451397E+00
#  x3 : -5.23681844701480E-01   1.16679253451397E+00
#  x4 :  5.23681844701480E-01  -1.16679253451397E+00
#  x5 :  5.23681844701480E-01  -1.16679253451397E+00
# == err :  3.555E-15 = rco :  6.219E-02 = res :  3.722E-15 ==
# solution 2 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  5.62945437488535E-01  -1.06731054777957E+00
#  x2 :  5.62945437488535E-01  -1.06731054777957E+00
#  x3 : -5.62945437488535E-01   1.06731054777957E+00
#  x4 :  5.62945437488535E-01  -1.06731054777957E+00
#  x5 : -5.62945437488535E-01   1.06731054777957E+00
# == err :  2.585E-15 = rco :  7.123E-02 = res :  3.286E-15 ==
# solution 3 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 : -2.86768329351213E-02   8.27714843748319E-01
#  x2 : -2.86768329351213E-02   8.27714843748319E-01
#  x3 :  2.86768329351213E-02  -8.27714843748319E-01
#  x4 :  2.86768329351213E-02  -8.27714843748319E-01
#  x5 : -2.86768329351213E-02   8.27714843748319E-01
# == err :  3.373E-16 = rco :  2.222E-01 = res :  4.450E-16 ==
# solution 4 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  2.86768329351213E-02   8.27714843748319E-01
#  x2 :  2.86768329351213E-02   8.27714843748319E-01
#  x3 : -2.86768329351213E-02  -8.27714843748319E-01
#  x4 : -2.86768329351213E-02  -8.27714843748319E-01
#  x5 : -2.86768329351213E-02  -8.27714843748319E-01
# == err :  3.406E-16 = rco :  2.222E-01 = res :  4.441E-16 ==
# solution 5 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  5.23681844701480E-01   1.16679253451397E+00
#  x2 : -5.23681844701480E-01  -1.16679253451397E+00
#  x3 :  5.23681844701480E-01   1.16679253451397E+00
#  x4 :  5.23681844701480E-01   1.16679253451397E+00
#  x5 :  5.23681844701480E-01   1.16679253451397E+00
# == err :  3.555E-15 = rco :  6.219E-02 = res :  3.722E-15 ==
# solution 6 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  1.23947841569073E+00   4.36197964162307E-01
#  x2 : -1.23947841569073E+00  -4.36197964162307E-01
#  x3 :  1.23947841569073E+00   4.36197964162307E-01
#  x4 :  1.23947841569073E+00   4.36197964162307E-01
#  x5 :  1.23947841569073E+00   4.36197964162307E-01
# == err :  3.568E-15 = rco :  5.702E-02 = res :  1.601E-14 ==
# solution 7 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  1.22889165583880E+00   5.12364025922987E-01
#  x2 : -1.22889165583880E+00  -5.12364025922987E-01
#  x3 : -1.22889165583880E+00  -5.12364025922987E-01
#  x4 : -1.22889165583880E+00  -5.12364025922987E-01
#  x5 : -1.22889165583880E+00  -5.12364025922987E-01
# == err :  5.297E-16 = rco :  4.594E-02 = res :  3.972E-15 ==
# solution 8 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 :  1.22889165583880E+00  -5.12364025922987E-01
#  x2 : -1.22889165583880E+00   5.12364025922987E-01
#  x3 : -1.22889165583880E+00   5.12364025922987E-01
#  x4 : -1.22889165583880E+00   5.12364025922987E-01
#  x5 : -1.22889165583880E+00   5.12364025922987E-01
# == err :  5.297E-16 = rco :  4.594E-02 = res :  3.972E-15 ==
# solution 9 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 : -1.23947841569073E+00   4.36197964162307E-01
#  x2 : -1.23947841569073E+00   4.36197964162307E-01
#  x3 : -1.23947841569073E+00   4.36197964162307E-01
#  x4 : -1.23947841569073E+00   4.36197964162307E-01
#  x5 : -1.23947841569073E+00   4.36197964162307E-01
# == err :  3.568E-15 = rco :  5.702E-02 = res :  1.601E-14 ==
# solution 10 :
# t :  1.00000000000000E+00   0.00000000000000E+00
# m : 16
# the solution for t :
#  x1 : -5.62945437488535E-01  -1.06731054777957E+00
#  x2 : -5.62945437488535E-01  -1.06731054777957E+00
#  x3 : -5.62945437488535E-01  -1.06731054777957E+00
#  x4 : -5.62945437488535E-01  -1.06731054777957E+00
#  x5 :  5.62945437488535E-01   1.06731054777957E+00
# == err :  2.585E-15 = rco :  7.123E-02 = res :  3.286E-15 ==
