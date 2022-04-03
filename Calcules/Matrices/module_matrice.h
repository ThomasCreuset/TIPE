/*  ~Matrices~  */

/*  -Importations-  */

#include "standard_lib.h"


/*  -Types et structures-   */

typedef double valeur;

struct matrice_s {
    int lignes;
    int colonnes;
    valeur** contenu;
};

typedef struct matrice_s matrice;;

/*  -Déclarations fonctions (f) et procédures (p)-  */

matrice* creer_matrice(int lignes, int colonnes);
// f - l.8 - crée une matrice de dimension 'lignes'x'colonnes' initialisé à 0 (valeur par default du type 'valeur')

void supprimer_matrice(matrice* matriceEntree);
// p - l.29 - vide la mémoire utilisée par la matrice 'matriceEntree'

matrice* add_matrice(matrice* matriceA, matrice* matriceB);
// f - l.37 - additionne les matrices 'matriceA' et 'matriceB' de manière non destructive

matrice* soustract_matrice(matrice* matriceA, matrice* matriceB);
// f - l.56 - soustrait la matrice 'matriceB' à la matrice 'matriceB' de manière non destructive

matrice* mult_matrice(matrice* matriceA, matrice* matriceB);
// f - l.75 - multiplie les matrices 'matriceA' et 'matriceB' de manière non destructive

matrice* transp_matrice(matrice* matriceEntree);
// f - l.97 - transpose la matrice 'matriceEntree' de manière non destructive

matrice* dilatation_matrice(valeur scalaire, matrice* matriceEntree);
// f - l.107 - dilate la matrice par une scalaire (de type 'valeur') 'scalaire'

valeur det_matrice(matrice* matriceEntree);
// f - l.117 - calcule le déterminant de la matrice carrée 'matriceEntree'

void echange_ligne(matrice* matriceEntree, int ligne1, int ligne2);
// p - l.185 - échange les lignes d'indice 'ligne1' et 'ligne2' de la matrice 'matriceEntree' par effet de bord

void echange_colonne(matrice* matriceEntree, int colonne1, int colonne2);
// p - l.200 - échange les colonnes d'indice 'colonne1' et 'colonne2' de la matrice 'matriceEntree' par effet de bord

void combinaison_lignes(matrice* matriceEntree, int ligneDest, valeur scalaire, int ligneAjout);
// p - l.215 - affecte à la ligne d'indice 'ligneDest' elle-même plus la ligne d'indice 'ligneAjout' multipliée par un scalaire 'scalaire' par effet de bord

void combinaison_colonnes(matrice* matriceEntree, int colonneDest, valeur scalaire, int colonneAjout);
// p - l.228 - affecte à la colonne d'indice 'colonneDest' elle-même plus la colonne d'indice 'colonneAjout' multipliée par un scalaire 'scalaire' par effet de bord

void dilatation_ligne(matrice* matriceEntree, valeur scalaire, int ligne);
// p - l.241 - affecte à la ligne d'indice 'ligne' elle-même multipliée par un scalaire 'scalaire' non-nul par effet de bord

void dilatation_colonne(matrice* matriceEntree, valeur scalaire, int colonne);
// p - l.259 - affecte à la colonne d'indice 'colonne' elle-même multipliée par un scalaire 'scalaire' non-nul par effet de bord

matrice* inv_matrice(matrice* matriceEntree);
// f - l.277 - calcule la matrice inverse de la matrice carrée inversible 'matriceEntree' de manière non destructive

matrice* affichage_matrice(matrice* matriceEntree);
// p - l.342 - affiche la matrice 'matriceEntree'
