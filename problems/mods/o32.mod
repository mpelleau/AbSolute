/* From the Colville's collection:
   Non convex quadratic Function
   Non convex quadratic constraints */

var x1 >= 78, <= 102;
var x2 >= 33, <= 45;
var x3 >= 27, <= 45;
var x4 >= 27, <= 45;
var x5 >= 27, <= 45;

param best_val_found := -30665.53867;
param eps := 1; 		# = max(1, 1% x best_val_found)

subject to f:
  37.293239 * x1 + 0.8356891 * x5 * x1 + 5.3578547 * x3^2 - 40792.141 <= best_val_found + eps;

subject to
  cons1: -0.0022053 * x3 * x5 + 0.0056858 * x2 * x5 + 0.0006262 * x1 * x4 - 
          6.665593 <= 0;
 
  cons2: -0.0022053 * x3 * x5 - 0.0056858 * x2 * x5 - 0.0006262 * x1 * x4 - 
         85.334407 <= 0;      
  
  cons3: 0.0071317 * x2 * x5 + 0.0021813 * x3^2 + 0.0029955 * x1 * x2 - 
         29.48751 <= 0;
  
  cons4:  - 0.0071317 * x2 * x5 - 0.0021813 * x3^2 - 0.0029955 * x1 * x2 + 
         9.48751 <= 0;
 
  cons5:  0.0047026 *x3 * x5 + 0.0019085 * x3 * x4 + 0.0012547 * x1 * x3 - 
         15.699039 <= 0;
 
  cons6:  - 0.0047026 *x3 * x5 - 0.0019085 * x3 * x4 - 0.0012547 * x1 * x3 + 
         10.699039 <= 0;

#solve;
#display x1, x2, x3, x4, x5;
