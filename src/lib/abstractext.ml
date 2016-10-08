include Apron.Abstract1

let filter_lincons man abs l =
  let ear = Linconsext.array_make abs.env 1 in
  Linconsext.array_set ear 0 l;
  meet_lincons_array man abs ear

let filter_tcons man abs l =
  let ear = Tconsext.array_make abs.env 1 in
  Tconsext.array_set ear 0 l;
  meet_tcons_array man abs ear
