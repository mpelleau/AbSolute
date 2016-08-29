/* 
   C implementation for float.ml module.

   Copyright (C) 2011 Antoine Miné
*/

#include <math.h>
#include <caml/mlvalues.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#include <fenv.h>

/* init */

value ml_float_init(value dummy)
{
  CAMLparam0();
  fesetround(FE_UPWARD);
  CAMLreturn(Val_unit);
}
