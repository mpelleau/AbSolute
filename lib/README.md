# lib
This repository defines all the auxiliary modules that will be used by the solver. They provide general functions that can be re-used and are not specific to AbSolute's implementation.

- **abstractext**, **linconsext** and **tconsext** are small extensions to the **abstract1**, **lincons1** and **tcons1** module of the [apron library]( http://apron.cri.ensmp.fr/library/)
- **apron_utils** provides few utilities over the apron types
- **mapext** is an extension of the ocaml's stdlib **map** module
- **bot** defines types and operations over possibly empty/bottom values (in the set-theoritic sense)
- **itv_sig** defines the signatue of the intervals used by the solver
- **bound_sig** defines the signature of the bounds used by the intervals. Those bounds should handle rounding errors
- **itv** is a generic functor paramztrized by a **bound module** that defines interval arithmetic w.r.t rouning errors
- **bound_float** defines the types and operations of the floatting bounds. It provides for each possibly not exact operation two versions : one that performs an upward rounding, and one that performs a downward rounding
- **ml_float.c** is a *C file* that provides a routine to set the rounding mode of the processor. It is used to provide a sound handling of the rounding erros.
- **constant** defines a bunch of values that are initialized at the starting of the solver and used during the whole solving process
