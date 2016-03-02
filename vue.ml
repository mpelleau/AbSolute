open Graphics
open Format
  
module Col = struct 
  
  let cpt = ref 0 and size = ref 0

  let n_colors n =
    let step = 0xFFFFFF / (n+2) in
    let arr = Array.make n 0x000000 |> Array.mapi (fun e i -> (i+1) * step) in
    Array.sort (fun _ _ -> (Random.int 3) -1) arr;
    arr
	
  let cols = ref [||]

  let init n = cols := n_colors n; size := n

  let col () = 
    let c = !cols.(!cpt) in
    cpt := (!cpt + 1) mod !size;
    c
    
end
	       
	       
(* Pour bien afficher les polyèdres, on part d'un point de x minimale,
   et on classes les points selon leur angle par rapport à ce point *)
(*Aucun comportement n'est garanti si tab ne contient pas les sommets
   d'un polyèdre convexe *)
(* C'est un parcours de Graham *)
(* Aff affiche un domaine *)
	       
let aff tab =
  let l = Array.length tab in  
  if l<>0 then 
    let minx=ref (tab.(0),0) in 
    Array.iteri 
      (fun i (u,v) -> 
	if u<fst(fst(!minx)) || u<=fst(fst(!minx)) && (v<(snd (fst !minx))) then
	  minx:= ((u,v),i) 
      )
      tab;
    let (x0,y0),k= !minx in 
    tab.(k)<-tab.(0);
    tab.(0)<-(x0,y0);

    let angle (x,y) = 
      if (x,y)=(x0,y0) then (0.,-5000.),x,y else
	((x0-.x)/.sqrt ((x0-.x)**2. +. (y0-.y)**2.) ,y-.y0),x,y
    in
    
    let tab2 = Array.map angle tab in 
    let comp ((cosin1,sign1),x1,y1) ((cosin2,sign2),x2,y2) = 
      if sign1>0. then
	if sign2<=0. then 1 
	else if cosin1<cosin2 then -1 
	else if cosin1=cosin2 then compare x1 x2
	else 1
	else if sign2>0. then -1
	else if cosin1<cosin2 then 1 
	else if cosin1=cosin2 then compare x2 x1
	else -1
    in
    Array.sort comp tab2;
    let tab3 = Array.map (fun (u,x,y)->truncate x,truncate y) tab2 in
    for i=0 to l-2 do
      let (x1,y1)=tab3.(i) in
      let (x2,y2)=tab3.(i+1) in
      let (x3,y3)=tab3.((i+2) mod l) in
      if (x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)<0 then 
 	tab3.((i+1))<-(x1,y1)
    done;
    fill_poly tab3
      
(* On affiche une liste de domaines donnés au départ par un tableau de
   flottants*)
(* On normalise l'affichage grace aux informations: n1 n2 sont les
   emplacements des variables à afficher Vient ensuite la position du
   centre de l'intersection entre le plan à afficher et le domaine Puis
   la taille du domaine selon ces variables *)
      
let aff_l l infoaff color =
  let (n1, n2, (centr,taillex,tailley)) = infoaff in
  let x0 = float (size_x()) and y0 = float (size_y()) in 
  set_color color;
  let aff2 tab = 
    let g2 tab = 
      try
	let (x,y) = (tab.(n1),tab.(n2)) in
	let (u,v)=((x0*.(x-.fst(centr))/.(taillex)), (y0*.(y-.snd(centr))/. (tailley))) in
	(u+.x0/.2.,v+.y0/.2.)
      with Invalid_argument "index out of bounds" -> failwith "Affichage.ml a reçu de mauvaise variables à afficher"
    in 
    aff (Array.map g2 tab);
  in
  List.iter aff2 l
	     
(* La meme fonction, mais qui affiche en haut à gauche une chaine de caractères *)
let aff_l_string s l infoaff color = 
  aff_l l infoaff color;
  set_color black;	
  moveto 10 650;
  draw_string s

let loop state = 
  loop_at_exit [] (fun _ -> ())

let create_window width height =
  Format.sprintf " %ix%i" width height |> open_graph;
  set_window_title "AbSolute";
  loop ()
