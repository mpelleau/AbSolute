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

int curr_method = -1;

value ml_float_restore(value dummy)
{
  if (curr_method != -1){
    fesetround(curr_method);
  };
  CAMLparam0();
  CAMLreturn(Val_unit);
}

value ml_float_init(value dummy)
{
  curr_method = fegetround();
  CAMLparam0();
  fesetround(FE_UPWARD);
  CAMLreturn(Val_unit);
}
