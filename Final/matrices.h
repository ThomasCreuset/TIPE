/*  ~ Matrices ~  */

/*  - Importations -  */

#include "standard_lib.h"


/*  - Déclarations fonctions (f) et procédures (p) -  */

double** creer_matrice(int lignes, int colonnes);
// f - créer une matrice dans les dimensions spécifiées

void free_matrice(double** matrice, int nombreLignes);
// p - free une matrice de double proprement

double** mult_matrice(double** matriceA, int nbreLignesA, int nbreColonnesA, int deltaLigneA, int deltaColonneA, double** matriceB, int nbreLignesB, int nbreColonnesB, int deltaLigneB, int deltaColonneB);
// f - multiplie les matrices 'matriceA' et 'matriceB' de manière non destructive

double** transp_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes);
// f - transpose la matrice 'matriceEntree' de manière non destructive

double det_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes);
// f - calcule le déterminant de la matrice carrée 'matriceEntree'

void echange_ligne(double** matriceEntree, int nbreColonnes, int ligne1, int ligne2);
// p - échange les lignes d'indice 'ligne1' et 'ligne2' de la matrice 'matriceEntree' par effet de bord

void echange_colonne(double** matriceEntree, int nbreLignes, int colonne1, int colonne2);
// p - échange les colonnes d'indice 'colonne1' et 'colonne2' de la matrice 'matriceEntree' par effet de bord

void combinaison_lignes(double** matriceEntree, int nbreColonnes, int ligneDest, double scalaire, int ligneAjout);
// p - affecte à la ligne d'indice 'ligneDest' elle-même plus la ligne d'indice 'ligneAjout' multipliée par un scalaire 'scalaire' par effet de bord

void combinaison_colonne(double** matriceEntree, int nbreLignes, int colonneDest, double scalaire, int colonneAjout);
// p - affecte à la colonne d'indice 'colonneDest' elle-même plus la colonne d'indice 'colonneAjout' multipliée par un scalaire 'scalaire' par effet de bord

void dilatation_ligne(double** matriceEntree, int nbreColonnes, double scalaire, int ligne);
// p - affecte à la ligne d'indice 'ligne' elle-même multipliée par un scalaire 'scalaire' non-nul par effet de bord

void dilatation_colonne(double** matriceEntree, int nbreLignes, double scalaire, int colonne);
// p - affecte à la colonne d'indice 'colonne' elle-même multipliée par un scalaire 'scalaire' non-nul par effet de bord

double** inv_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes, bool verifie);
// f - calcule la matrice inverse de la matrice carrée inversible 'matriceEntree' de manière non destructive

double** affichage_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes);
// p - affiche la matrice 'matriceEntree'