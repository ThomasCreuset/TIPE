/*  -Fichier entête-  */

#include "module_matrice.h"


/*  -Constantes-  */

const valeur DEFAULT = 0;


/* -Fonctions- */

matrice* creer_matrice(int lignes, int colonnes) {
    if (lignes <= 0 || colonnes <= 0) {
        fprintf(stderr, "création impossible:\n");
        fprintf(stderr, "\t-> les dimensions doivent-être des entiers non nuls.");
        exit(EXIT_FAILURE);
    }
    matrice* matriceSortie  = malloc(sizeof(matrice));
    matriceSortie->lignes   = lignes;
    matriceSortie->colonnes = colonnes;
    matriceSortie->contenu  = malloc(sizeof(valeur*)*lignes);
    for (int ligne = 0; ligne < lignes; ++ligne) {
        matriceSortie->contenu[ligne] = malloc(sizeof(valeur)*colonnes);
        for (int colonne = 0; colonne < colonnes; ++colonne) {
            matriceSortie->contenu[ligne][colonne] = DEFAULT;
        }
    }
    return matriceSortie;
}

void supprimer_matrice(matrice* matriceEntree) {
    for (int ligne = 0; ligne < matriceEntree->lignes; ++ligne) {
        free(matriceEntree->contenu[ligne]);
    }
    free(matriceEntree->contenu);
    free(matriceEntree);
}

matrice* add_matrice(matrice* matriceA, matrice* matriceB) {
    if (matriceA->lignes != matriceB->lignes || matriceA->colonnes != matriceB->colonnes) {
        fprintf(stderr, "addition impossible:\n");
        fprintf(stderr, "\t-> les deux matrices doivent-être de taille identique.");
        exit(EXIT_FAILURE);
    }
    matrice* matriceSortie = creer_matrice(matriceA->lignes, matriceA->colonnes);
    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne) {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne) {
            matriceSortie->contenu[ligne][colonne] = matriceA->contenu[ligne][colonne] + matriceB->contenu[ligne][colonne];
        }
    }
    return matriceSortie;
}

matrice* soustract_matrice(matrice* matriceA, matrice* matriceB) {
    if (matriceA->lignes != matriceB->lignes || matriceA->colonnes != matriceB->colonnes) {
        fprintf(stderr, "soustraction impossible:\n");
        fprintf(stderr, "\t-> les deux matrices doivent-être de taille identique.");
        exit(EXIT_FAILURE);
    }
    matrice* matriceSortie = creer_matrice(matriceA->lignes, matriceA->colonnes);
    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne) {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne) {
            matriceSortie->contenu[ligne][colonne] = matriceA->contenu[ligne][colonne] - matriceB->contenu[ligne][colonne];
        }
    }
    return matriceSortie;
}

matrice* mult_matrice(matrice* matriceA, matrice* matriceB) {
    if (matriceA->colonnes != matriceB->lignes) {
        fprintf(stderr, "multiplication impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceA' doit avoir autant de colonnes que la matrice 'matriceB' a de lignes.");
        exit(EXIT_FAILURE);
    }
    int tailleCommune = matriceA->colonnes;
    matrice* matriceSortie = creer_matrice(matriceA->lignes, matriceB->colonnes);
    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne) {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne) {
            int somme = 0;
            for (int k = 0; k < tailleCommune; ++k) {
                somme += matriceA->contenu[ligne][k] * matriceB->contenu[k][colonne];
            }
            matriceSortie->contenu[ligne][colonne] = somme;
        }
    }
    return matriceSortie;
}

matrice* transp_matrice(matrice* matriceEntree) {
    matrice* matriceSortie = creer_matrice(matriceEntree->colonnes, matriceEntree->lignes);
    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne) {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne) {
            matriceSortie->contenu[ligne][colonne] = matriceEntree->contenu[colonne][ligne];
        }
    }
    return matriceSortie;
}

matrice* dilatation_matrice(int scalaire, matrice* matriceEntree) {
    matrice* matriceSortie = creer_matrice(matriceEntree->lignes, matriceEntree->colonnes);
    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne) {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne) {
            matriceSortie->contenu[ligne][colonne] = scalaire * matriceEntree->contenu[ligne][colonne];
        }
    }
    return matriceSortie;
}
