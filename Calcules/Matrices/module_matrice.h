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
// f - l.8 - crée une matrice de dimension 'lignes'x'colonnes' initialisé à 'DEFAULT' (valeur par default du type 'valeur')

void supprimer_matrice(matrice* matriceEntree);
// p - l.27 - vide la mémoire utilisée par la matrice 'matriceEntree'

matrice* add_matrice(matrice* matriceA, matrice* matriceB);
// f - l.35 - additionne les matrices 'matriceA' et 'matriceB' de manière non destructive

matrice* soustract_matrice(matrice* matriceA, matrice* matriceB);
// f - l.50 - soustrait la matrice 'matriceB' à la matrice 'matriceB' de manière non destructive

matrice* mult_matrice(matrice* matriceA, matrice* matriceB);
// f - l.65 - multiplie les matrices 'matriceA' et 'matriceB' de manière non destructive

matrice* transp_matrice(matrice* matriceEntree);
// f - l.85 - transpose la matrice 'matriceEntree' de manière non destructive

matrice* dilatation_matrice(int scalaire, matrice* matriceEntree);
// f - l.95 - dilate la matrice par une scalaire (de type 'valeur') 'scalaire'
