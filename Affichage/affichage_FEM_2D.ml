
(*Pour Windows*)
(*
#load "graphics.cma";; 
Graphics.open_graph "800x600";;
open Graphics;;
open_graph "720x1280";;
*)

(*Pour Linux*)
#use "topfind";;
#require "graphics";;
open Graphics;;
let hauteur=720 and largeur=1280;;
open_graph "";;
resize_window largeur hauteur;;

(*------------Types,variables et fonctions outils--------*)


type point   = {x: float;  y: float;  z: float};;
type vecteur = {vx: float; vy: float; vz: float};;


let x0   = ref (float_of_int (size_x()/2))
and y0   = ref (float_of_int (size_y()/2))
and zoom = ref 150.;;


let base = ref ({vx = 1.; vy = 0.; vz = 0.},
{vx = 0.; vy = 1.; vz = 0.},
{vx = 0.; vy = 0.; vz = 1.});;


let vecteur pt1 pt2 = {vx = (pt2.x -. pt1.x); vy = (pt2.y -. pt1.y); vz = (pt2.z -. pt1.z)};;


let produit_scalaire vct1 vct2 = vct1.vx *. vct2.vx +. vct1.vy *. vct2.vy +. vct1.vz *. vct2.vz;;


let norme vct = sqrt(vct.vx**2. +. vct.vy**2. +. vct.vz**2.);;


let unitaire vct = {vx = (vct.vx /. (norme vct));
						  vy = (vct.vy /. (norme vct));
						  vz = (vct.vz /. (norme vct))};;


let produit_vectoriel vct1 vct2 =
	{vx = (vct1.vy *. vct2.vz -. vct1.vz *. vct2.vy);
	 vy = (vct1.vz *. vct2.vx -. vct1.vx *. vct2.vz);
	 vz = (vct1.vx *. vct2.vy -. vct1.vy *. vct2.vx)};;

let dans_base pt bse = let vctb1, vctb2, vctb3 = bse and origine = {x = 0.; y = 0.; z = 0.} in
	{x = (produit_scalaire (vecteur origine pt) vctb1);
	 y = (produit_scalaire (vecteur origine pt) vctb2);
	 z = (produit_scalaire (vecteur origine pt) vctb3)};;


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
				(rotation_y vct3 theta))
;;

let projette pt = (int_of_float (!x0 +. !zoom *. pt.x), int_of_float (!y0 +. !zoom *. pt.y));;

let make_point (x,y,z) = { x = x; y=y; z=z;};;

type element = int * int * float * float;; (*indice du noeud1, indice noeud2, module young, section*)

let make_element indice_noeud1 indice_noeud2 mod_young section = (indice_noeud1,indice_noeud2,mod_young,section);;

type item_affichable = Arete of (point*point*int*int) | Noeud of (point*int*int);; (*Arete(point de départ, point d'arrivée, epaisseur,couleur) et Noeud(point, rayon, couleur)*)


(*COULEURS ET EPAISSEUR DES ITEMS*)

(*Fonction pour resize des intervalles (proportionnalité)*)
let map debut1 fin1 debut2 fin2 x = 
  if debut1<>fin1 then
  begin
	let t = (x -. debut1)/.(fin1 -. debut1) in
	(1. -. t)*. debut2 +. t*.fin2
  end
  else debut2
  
;;


(*Détermine la couleur d'un noeud à l'aide de la norme de la force appliquée en ce noeud*)
let couleurs_noeuds noeuds forces =
	let normes_forces = Array.map norme forces in
	(*let max_norme_force force1 force2 = max (norme force1) (norme force2) in
	let min_norme_force force1 force2 = min (norme force1) (norme force2) in *)
  let max_force  = Array.fold_left max normes_forces.(0) normes_forces
  and min_force = Array.fold_left min normes_forces.(0) normes_forces
in
let tab_couleurs = Array.map (fun norme_force -> let teinte = int_of_float (map min_force max_force 0. 255. norme_force) in rgb teinte 0 (255 - teinte) ) normes_forces in
tab_couleurs;; (*Censé faire un dégradé du bleu au rouge*)

(*Calcule l'emplacement des noeuds après application de la force, ajoutant les déplacements*)
let noeuds_deplaces noeuds deplacements = 
		Array.map2 (fun point vecteur -> 
		print_string "Coordonnées : ";print_float point.x;print_string " "; print_float point.y;print_string " "; print_float point.z;
		print_newline();
		print_string "Deplacement : ";print_float vecteur.vx;print_string " "; print_float vecteur.vy;print_string " "; print_float vecteur.vz;
		print_newline();
		print_newline();
		make_point ((point.x +. vecteur.vx),(point.y +. vecteur.vy),(point.z +. vecteur.vz)) ) noeuds deplacements 
;;

(*Calcule l'épaisseur à afficher des aretes. Attention, renvoie le max et le min des sections (unité d'origine)*)
let epaisseurs_elements elements = 
	let sections = Array.map (fun (i1,i2,young,section) -> section) elements in
let max_section = Array.fold_left max 0. sections
and min_section = Array.fold_left min infinity sections
and min_epaisseur = 3. (*Constantes d'épaisseurs des traits*)
in
print_float min_section;print_newline();print_float max_section;print_newline();print_newline(); let max_epaisseur = (max_section *. min_epaisseur) /. min_section
in
let tab_epaisseurs = Array.map (fun section -> let valeur = map min_section max_section min_epaisseur max_epaisseur section
							in print_float valeur;print_newline(); int_of_float (valeur) ) sections in
print_int tab_epaisseurs.(0); print_newline();
print_newline();
tab_epaisseurs,min_section,max_section
;;

(*Crée un tableau des items à afficher (noeuds et arêtes), qui sera trié par la cote moyenne ou la cote en fonction de si c'est un poin ou une arete.
	 Contient les aretes et noeuds avant et après application des forces*)
let make_items_affichables elements noeuds forces deplacements= 
	let epaisseurs,_,_ = epaisseurs_elements elements in
	let couleurs = couleurs_noeuds noeuds forces in
	let noeuds_depl = noeuds_deplaces noeuds deplacements in
	let tab_aretes_originelles = Array.map2 (fun (i1,i2,young,section) epaisseur -> Arete(noeuds.(i1),noeuds.(i2),epaisseur,rgb 127 127 127) ) elements epaisseurs
	and tab_noeuds_originels = Array.map (fun point -> Noeud(point,3,rgb 127 127 127)) noeuds
	and tab_aretes_deplacees = Array.map2 (fun (i1,i2,young,section) epaisseur -> Arete(noeuds_depl.(i1),noeuds_depl.(i2),epaisseur,black) ) elements epaisseurs
	and tab_noeuds_deplaces = Array.map2 (fun point couleur -> Noeud(point,7,couleur)) noeuds_depl couleurs

	in
	Array.concat [tab_aretes_originelles;tab_noeuds_originels;tab_aretes_deplacees;tab_noeuds_deplaces] 
;;

(*Fonction auxiliaire pour tracer une arete*)
let trace_arete point1 point2 epaisseur couleur = 
	let pt1 = dans_base point1 !base
	and pt2 = dans_base point2 !base
  in
	let x1,y1 = projette pt1
	and x2,y2 = projette pt2 in
	set_color couleur;
	set_line_width epaisseur;
	moveto x1 y1;
	lineto x2 y2;
;;

(*Fonction auxiliaire pour tracer un noeud*)
let trace_noeud point rayon couleur =
	let epaisseur_trait = max 1 (int_of_float(float_of_int (rayon) *. 0.2)) in
	let pt = dans_base point !base in
	let x,y = projette pt in
	set_color couleur;
	fill_circle x y rayon;
	set_color black;

	set_line_width epaisseur_trait;
	draw_circle x y rayon
;;

(*------Algorithme du Peintre--------------*)


(*Profondeur d'un point dans la direction z*)
let cote pt = let proj = dans_base pt !base in proj.z;;

(*Profondeur pour une arete*)
let cote_moyenne (point1, point2) = (cote point1 +. cote point2)/. 2.;;

(*Tri des items pour l'algo du peintre*)
let tri tab clef = let taille = (Array.length tab) - 1 in
	for i = 1 to taille do
		let j = ref i and check = clef tab.(i) and temp = tab.(i) in
		while !j > 0 && clef (tab.(!j-1)) > check do
			tab.(!j) <- tab.(!j-1);
			j := !j - 1;
		done;	
		tab.(!j) <- temp;
	done
;;

let tri_items items_a_afficher = 
	let clef_tri item = match item with
		|Arete(point1,point2,epaisseur,couleur_arete) -> cote_moyenne (point1,point2)
		|Noeud(p,rayon,couleur_noeud) -> cote p
	in
	tri items_a_afficher clef_tri
;;
	

(*Affichage des items dans le bon ordre*)
let peintre_items items_a_afficher = let taille = Array.length items_a_afficher in
	tri_items items_a_afficher;
	for i = taille-1 downto 0 do
		(*print_string "Traçage de l'item n° : ";print_int i; print_newline();*)
		let item= items_a_afficher.(i) in
		match item with 
		| Noeud(point,rayon,couleur) -> trace_noeud point rayon couleur
		| Arete(point1,point2,epaisseur,couleur) -> trace_arete point1 point2 epaisseur couleur
	done
;;


(*-----Récuperation des données dans un fichier extérieur----------*)


(*Type tableau dynamique pour faciliter la récupération des données*)
type 'a tableau_dynamique = {mutable support: 'a array;
								  	  mutable taille: int};;

let make_td element = {
	support = Array.make 16 element; taille = 0};;

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


(*Fonction qui lit le fichier contenant les données et qui renvoie les tableaux contenant :
	 -les noeuds (indicés par i)
	 -le déplacement des noeuds (deplacement du noeud i à l'indice i)
	 -les forces appliquées au noeud i
	 -les elements, ie (indice_noeud1,indice_noeud2,module_young,section)*)
let lecture_fichier nomFichier =
	let fichier = open_in nomFichier in
	let point_generique = make_point (0.,0.,0.) in
	let element_generique = make_element 0 0 0. 0.  in
	let deplacement_generique = vecteur point_generique point_generique in
	let force_generique = vecteur point_generique point_generique in
	let noeuds  = make_td point_generique in
	let deplacements = make_td deplacement_generique in
	let forces = make_td force_generique in
	let elements = make_td element_generique in
	
	let ligne = ref (input_line fichier) in
	let nb_noeuds, nb_elements = Scanf.sscanf !ligne "%d;%d" (fun n1 n2 -> (n1,n2)) in
	
	for i = 0 to nb_noeuds-1 do
		ligne:= input_line fichier;
		let x,dx,fx = Scanf.sscanf !ligne "%f;%f;%f" (fun x dx fx-> (x,dx,fx)) in
		ligne:= input_line fichier;
		let y,dy,fy = Scanf.sscanf !ligne "%f;%f;%f" (fun y dy fy -> (y,dy,fy)) in
		ligne:= input_line fichier;
		let z,dz,fz = Scanf.sscanf !ligne "%f;%f;%f" (fun z dz fz-> (z,dz,fz)) in
			
			ajoute noeuds (make_point (x,y,z));
			ajoute deplacements (vecteur point_generique (make_point (dx,dy,dz)));
			ajoute forces (vecteur point_generique (make_point (fx,fy,fz)));
	done;
	for i = 0 to nb_elements-1 do
		ligne:=input_line fichier;
		let element = Scanf.sscanf !ligne "%d;%d;%f;%f" (fun i1 i2 module_young section -> (i1,i2,module_young,section)) in
			ajoute elements element;
	done;
	close_in fichier;
	let coupe_tableau_dyn tab = Array.sub (tab.support) 0 (tab.taille)  in
  coupe_tableau_dyn noeuds,coupe_tableau_dyn deplacements,coupe_tableau_dyn forces ,coupe_tableau_dyn elements
;;



(*Boucle pour afficher la structure et la faire tourner à l'aide du clavier*)
let en_sync_items items_a_afficher =
	auto_synchronize false;
	display_mode false;
	peintre_items items_a_afficher;

		while true do
			let event = wait_next_event [Key_pressed] in let key = event.key in
			if key = 's' then y0 := !y0 -. 5.;
			if key = 'z' then y0 := !y0 +. 5.;
			if key = 'q' then x0 := !x0 -. 5.;
			if key = 'd' then x0 := !x0 +. 5.;
			if key = 'o' then rotation_base_y (0.05);
			if key = 'l' then rotation_base_y (-0.05);
			if key = 'k' then rotation_base_x (-0.05);
			if key = 'm' then rotation_base_x (0.05);
			if key = 'a' then zoom := !zoom +. 5.;
			if key = 'e' then zoom := !zoom -. 5.;
			clear_graph ();
			peintre_items items_a_afficher;
			synchronize ();
		done
;;


(*Fonction main : récupere les tableaux et lance la fonction en_sync_items.*)
let main () =
let noeuds,deplacements,forces,elements = lecture_fichier "resultat.txt" in
print_string "Nombre d'éléments : ";
print_int (Array.length elements); print_newline();

(*let noeuds2 = noeuds_deplaces noeuds deplacements in
let tous_noeuds = Array.append noeuds noeuds2 in
let tous_elements = Array.append elements elements in
*)
let items_a_afficher = make_items_affichables elements noeuds forces deplacements in
(*affiche_aretes_elements elements noeuds;*)
(*set_line_width 10;
lineto (size_x()/2) (size_y()/2); *)
synchronize ();
en_sync_items items_a_afficher;
;;

main();; 
