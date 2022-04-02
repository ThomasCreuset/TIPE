#load "graphics.cma";;
open Graphics;;
Graphics.open_graph "800x600";;

(* Partie 1 *)
(* Q1 *)

type point   = {x: float;  y: float;  z: float};;
type vecteur = {vx: float; vy: float; vz: float};;

(* Q2 *)

let x0   = ref (-10.)
and y0   = ref (-10.)
and zoom = ref 5.;;

let couleur_lumiere   = (0, 0, 255);;
let direction_lumiere = {vx=0.; vy= 0.; vz=1.};;

let base = ref ({vx = 1.; vy = 0.; vz = 0.},
{vx = 0.; vy = 1.; vz = 0.},
{vx = 0.; vy = 0.; vz = 1.});;

(* Q3 *)

let vecteur pt1 pt2 = {vx = (pt2.x -. pt1.x); vy = (pt2.y -. pt1.y); vz = (pt2.z -. pt1.z)};;

(* Q4 *)

let produit_scalaire vct1 vct2 = vct1.vx *. vct2.vx +. vct1.vy *. vct2.vy +. vct1.vz *. vct2.vz;;

(* Q5 *)

let norme vct = sqrt(vct.vx**2. +. vct.vy**2. +. vct.vz**2.);;

(* Q6 *)

let unitaire vct = {vx = (vct.vx /. (norme vct));
						  vy = (vct.vy /. (norme vct));
						  vz = (vct.vz /. (norme vct))};;

(* Q7 *)

let produit_vectoriel vct1 vct2 =
	{vx = (vct1.vy *. vct2.vz -. vct1.vz *. vct2.vy);
	 vy = (vct1.vz *. vct2.vx -. vct1.vx *. vct2.vz);
	 vz = (vct1.vx *. vct2.vy -. vct1.vy *. vct2.vx)};;

(* Q8 *)

let dans_base pt bse = let vctb1, vctb2, vctb3 = bse and origine = {x = 0.; y = 0.; z = 0.} in
	{x = (produit_scalaire (vecteur origine pt) vctb1);
	 y = (produit_scalaire (vecteur origine pt) vctb2);
	 z = (produit_scalaire (vecteur origine pt) vctb3)};;

(* Q9 *)

let rotation_x vct theta =
	{vx = vct.vx;
	 vy = vct.vy *. cos theta -. vct.vz *. sin theta;
	 vz = vct.vy *. sin theta +. vct.vz *. cos theta};;

let rotation_y vct theta =
	{vx = vct.vx *. cos theta +. vct.vz *. sin theta;
	 vy = vct.vy;
	 vz = vct.vz *. cos theta -. vct.vx *. sin theta};;

let rotation_base_x theta = let vct1, vct2, vct3 = !base in
	base := ((rotation_x vct1 theta),
				(rotation_x vct2 theta),
				(rotation_x vct3 theta));;

let rotation_base_y theta = let vct1, vct2, vct3 = !base in
	base := ((rotation_y vct1 theta),
				(rotation_y vct2 theta),
				(rotation_y vct3 theta));;

(* Partie 2 *)
(* Q10 *)

let tri tab clef = let taille = (Array.length tab) - 1 in
	for i = 1 to taille do
		let j = ref i and check = clef tab.(i) and temp = tab.(i) in
		while !j > 0 && clef (tab.(!j-1)) > check do
			tab.(!j) <- tab.(!j-1);
			j := !j - 1;
		done;	
		tab.(!j) <- temp;
	done;;

(* Q11 *)

let projette pt = (int_of_float (!x0 +. !zoom *. pt.x), int_of_float (!y0 +. !zoom *. pt.y));;

(* Q12 *)

let echelle v x = int_of_float ((float_of_int v) *. x);;

(* Q13 *)

let trace_triangle point1 point2 point3 =
	let pt1 = dans_base point1 !base
	and pt2 = dans_base point2 !base
	and pt3 = dans_base point3 !base 
	and lum1, lum2, lum3 = couleur_lumiere in
	let normale = unitaire (produit_vectoriel (vecteur pt1 pt2) (vecteur pt1 pt3)) in
	let scale   = abs_float (produit_scalaire direction_lumiere normale) in
	let c1 = echelle lum1 scale
	and c2 = echelle lum2 scale
	and c3 = echelle lum3 scale in
	let proj1 = projette pt1
	and proj2 = projette pt2
	and proj3 = projette pt3 in
	set_color (rgb c1 c2 c3);
	fill_poly [|proj1; proj2; proj3|];
	set_color (rgb 0 0 0);
	draw_poly [|proj1; proj2; proj3|];;

(* Q14 *)

let cote pt = let proj = dans_base pt !base in proj.z;;

(* Q15 *)

let cote_moyenne triangle = let (pt1, pt2, pt3) = triangle in (cote pt1 +. cote pt2 +. cote pt3) /. 3.;;

(* Q16 *)

let tri_angle triangles = tri triangles cote_moyenne;;

(* Q17 *)

let peintre triangles = let taille = Array.length triangles in
	tri_angle triangles;
	for i = taille-1 downto 0 do
		let (pt1, pt2, pt3) = triangles.(i) in
		trace_triangle pt1 pt2 pt3
	done;;

(* Q18 *)

let cube =
let triangle =
let p1 = {x= 1. ; y=1. ; z=0.}
and p2 = {x= 1. ; y=2. ; z=0.}
and p3 = {x= 5. ; y=3. ; z=5.}
in (p1,p2,p3)
in
let tableau = Array.make 12 triangle
and make_triangle p1 p2 p3 =(p1,p2,p3);
and make_point i j k = {x= float_of_int i; y=float_of_int j; z=float_of_int k}
in
(*Face de devant*)
tableau.(0)<- make_triangle (make_point 0 0 0) (make_point 1 0 0) (make_point 0 0 1 );
tableau.(1)<- make_triangle (make_point 1 0 1) (make_point 1 0 0) (make_point 0 0 1 );

(*face de droite*)
tableau.(2)<- make_triangle (make_point 1 0 0) (make_point 1 0 1) (make_point 1 1 0 );
tableau.(3)<- make_triangle (make_point 1 1 1) (make_point 1 0 1) (make_point 1 1 0 );

(*face de gauche*)
tableau.(4)<- make_triangle (make_point 0 0 0) (make_point 0 1 0) (make_point 0 0 1 );
tableau.(5)<- make_triangle (make_point 0 1 1) (make_point 0 1 0) (make_point 0 0 1 );

(*face de derrière*)
tableau.(6)<- make_triangle (make_point 0 1 1) (make_point 1 1 1) (make_point 1 1 0 );
tableau.(7)<- make_triangle (make_point 0 1 1) (make_point 0 1 0) (make_point 1 1 0 );

(*face de dessous*)
tableau.(8)<- make_triangle (make_point 0 0 0) (make_point 1 0 0) (make_point 0 1 0 );
tableau.(9)<- make_triangle (make_point 1 1 0) (make_point 1 0 0) (make_point 0 1 0 );

(*face de dessus*)
tableau.(10)<- make_triangle (make_point 0 0 1) (make_point 0 1 1) (make_point 1 0 1 );
tableau.(11)<- make_triangle (make_point 1 1 1) (make_point 0 1 1) (make_point 1 0 1 );

tableau;;

(* Q20 *)



let en_sync triangles =
Graphics.open_graph "800x600";
auto_synchronize false;
display_mode false;
	while true do
		let event = wait_next_event [Key_pressed] in let key = event.key in
		if key = 's' then y0 := !y0 -. 5.;
		if key = 'z' then y0 := !y0 +. 5.;
		if	key = 'q' then x0 := !x0 -. 5.;
		if	key = 'd' then x0 := !x0 +. 5.;
		if key = 'o' then rotation_base_y (0.05);
		if	key = 'l' then rotation_base_y (-0.05);
		if key = 'k' then rotation_base_x (-0.05);
		if key = 'm' then rotation_base_x (0.05);
		if key = 'a' then zoom := !zoom +. 5.;
		if key = 'e' then zoom := !zoom -. 5.;
		clear_graph ();
		peintre triangles;
		synchronize ();
	done;;

(* en_sync cube;; *)

(* Q21 *)

type 'a tableau_dynamique = {mutable support: 'a array;
								  	  mutable taille: int};;

(* Q22 *)

let ajoute td valeur =
	if td.taille <> Array.length td.support then
		begin
		td.support.(td.taille) <- valeur;
		td.taille <- td.taille + 1;
		end
	else
		begin
		let new_support = Array.make (td.taille*2) valeur in
		for i = 0 to (td.taille-1) do
			new_support.(i) <- td.support.(i);
		done;
		td.support <- new_support;
		td.taille <- td.taille + 1;
		end;;

(* Q23 *)

let lecture_obj nomFichier =
	let fichier = open_in nomFichier
	and points  = {support = [|{x = 0.; y = 0.; z = 0.}|]; taille = 0}
	and faces   = {support = [|({x = 0.; y = 0.; z = 0.},{x = 0.; y = 0.; z = 0.},{x = 0.; y = 0.; z = 0.})|]; taille = 0} in
	try
		while true do
   		let ligne = input_line fichier in
   		let acces = Scanf.sscanf ligne "%s" (fun x -> x) in
   		if acces = "v" then
   		begin
   			let x1, y1, z1 = (Scanf.sscanf ligne "%c %f %f %f" (fun t x y z -> (x,y,z))) in
   			ajoute points {x = x1; y = y1; z = z1};
   		end;
   		if acces = "f" then
   			let p1, p2, p3 = Scanf.sscanf ligne "%c %s %s %s" (fun t x y z -> (x,y,z)) in
   			let s1 = Scanf.sscanf p1 "%d" (fun x -> x-1)
   			and s2 = Scanf.sscanf p2 "%d" (fun x -> x-1)
   			and s3 = Scanf.sscanf p3 "%d" (fun x -> x-1) in
   			ajoute faces (points.support.(s1), points.support.(s2), points.support.(s3));
  		done;
  		faces.support;
	with End_of_file ->
  		close_in fichier;
  		Array.sub faces.support 0 faces.taille;;

(* Q24 *)

en_sync (lecture_obj "C:\\Users\\thoma\\OneDrive\\Bureau\\Informatique\\Projet_Caml\\3D\\ourson.obj");;
