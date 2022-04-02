#use "topfind";;
#require "graphics";;
open Graphics;;

let hauteur=720 and largeur=1280;;
open_graph "";resize_window largeur hauteur;;

type point = {
   x : float;
   y : float;
   z : float;
};;

type vecteur = {
   vx : float;
   vy : float;
   vz : float;
};;


let x0= ref (float_of_int(size_x()/2))
and y0 = ref (float_of_int(size_y()/2))
and zoom = ref 100.;;

let couleur_lumiere= (0,0,255);;
let direction_lumiere= {vx = 0. ; vy = 0. ; vz = 1. };;

let base = ref ({vx = 1.; vy = 0.; vz = 0.},
                {vx = 0.; vy = 1.; vz = 0.},
                {vx = 0.; vy = 0.; vz = 1.});;
let reset_base b = b:=({vx = 1.; vy = 0.; vz = 0.},
{vx = 0.; vy = 1.; vz = 0.},
{vx = 0.; vy = 0.; vz = 1.});;

(*Q3*)
let vecteur point1 point2 = { vx = point2.x -. point1.x ; 
                              vy = point2.y -. point1.y ; 
                              vz = point2.z -. point1.z};;


let pointA = { x = 1.; y=0.; z=0.;};;
let origine = { x = 0.; y=0.; z=0.;};;

let vectA = vecteur origine pointA;;

let pointB = { x = 0.; y=1.; z=0.;};;
let vectB= vecteur origine pointB;;

let produit_scalaire vect1 vect2 = vect1.vx *. vect2.vx +. vect1.vy *. vect2.vy +. vect1.vz *. vect2.vz;;

let norme vect = sqrt(produit_scalaire vect vect);;

let unitaire vect = let facteur = norme vect in { vx=vect.vx /. facteur; vy = vect.vy /.facteur; vz = vect.vz /. facteur };;

let produit_vectoriel vect1 vect2 = {
  vx = vect1.vy *. vect2.vz -. vect1.vz *. vect2.vy;
  vy = vect1.vz *. vect2.vx -. vect1.vx *. vect2.vz;
  vz = vect1.vx *. vect2.vy -. vect1.vy *. vect2.vx;
};;



let dans_base pointP base = 
  let i,j,k = !base
  and vectP = vecteur {x=0.; y=0.; z=0.;} pointP in 
  {
    x = produit_scalaire vectP i;
    y = produit_scalaire vectP j;
    z = produit_scalaire vectP k;
  };;


let rotation_Ox vect theta=
  let matrice_rotation angle = ({vx = 1.; vy = 0.; vz = 0.},
                                {vx = 0.; vy = cos angle; vz = -.sin angle},
                                {vx = 0.; vy = sin angle; vz = cos angle})
  in 
  let i,j,k = matrice_rotation theta 
  in 
  {vx = produit_scalaire vect i;
  vy = produit_scalaire vect j;
  vz = produit_scalaire vect k;
};;

let rotation_Oy vect theta=
  let matrice_rotation angle = ({vx = cos angle; vy = 0.; vz = sin angle},
                                {vx = 0.;        vy = 1. ; vz = 0.},
                                {vx = -. sin angle; vy = 0.; vz = cos angle})
  in 
  let i,j,k = matrice_rotation theta 
  in 
  {vx = produit_scalaire vect i;
  vy = produit_scalaire vect j;
  vz = produit_scalaire vect k;
};;


let rotation_Ox_base base theta = 
  let i,j,k = !base
  in 
  base:= (rotation_Ox i theta,
          rotation_Ox j theta,
          rotation_Ox k theta);      
  ();;


let rotation_Oy_base base theta = 
  let i,j,k = !base
  in 
  base:= (rotation_Oy i theta,
          rotation_Oy j theta,
          rotation_Oy k theta);      
  ();;
