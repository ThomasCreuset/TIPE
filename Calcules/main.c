/*  -Importation-   */

#include "module_matrice.h"
#include "standard_lib.h"


/*  -Main-  */

int main() {
    matrice* maMatrice1 = creer_matrice(2, 2);
    matrice* maMatrice2 = creer_matrice(2, 2);
    maMatrice1->contenu[0][0] = 3.2;
    maMatrice1->contenu[1][0] = 5.1;
    maMatrice1->contenu[0][1] = 1.0;
    maMatrice1->contenu[1][1] = 2.7;
    maMatrice2->contenu[0][0] = 9.3;
    maMatrice2->contenu[1][0] = 12.6;
    maMatrice2->contenu[0][1] = 0.5;
    maMatrice2->contenu[1][1] = 2.1;
    printf("-> affichage simple:\n");
    affichage_matrice(maMatrice1);
    affichage_matrice(maMatrice2);
    printf("-> inverse:\n");
    affichage_matrice(inv_matrice(maMatrice1));
    printf("-> multiplication:\n");
    affichage_matrice(mult_matrice(maMatrice1, maMatrice2));
    printf("-> addition:\n");
    affichage_matrice(add_matrice(maMatrice1, maMatrice2));
    return EXIT_SUCCESS;
}
