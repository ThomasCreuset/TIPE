/*  ~Outils pour l'analogique~  */

/*  -Importations-  */

#include "standard_lib.h"
#include "module_matrice.h"


/*  -Types et structures-   */



/*  -Déclarations fonctions (f) et procédures (p)-  */

double calcul_proportion(double tensionSeuil, matrice* donnees);
// f - calcul le coefficient de proportionnalité pour ne pas sortir de la zone de linéarité du système

void adapte_donnees(double coefficientProportionalite, matrice* donnees);
// p - mise à l'échelle des données (de manière destructive) 
