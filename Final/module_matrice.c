/*  -Fichier entête-  */

#include "module_matrice.h"


/* -Fonctions- */

matrice* creer_matrice(int lignes, int colonnes)
{
    if (lignes <= 0 || colonnes <= 0)
    {
        fprintf(stderr, "création impossible:\n");
        fprintf(stderr, "\t-> les tailles 'lignes' et 'colonnes' doivent-être des entiers non nuls.\n");
        fprintf(stderr, "\t\t'lignes': {%d}\n", lignes);
        fprintf(stderr, "\t\t'colonnes': {%d}\n", colonnes);
        exit(EXIT_FAILURE);
    }
    
    matrice* matriceSortie  = malloc(sizeof(matrice));
    matriceSortie->lignes   = lignes;
    matriceSortie->colonnes = colonnes;
    matriceSortie->contenu  = malloc(sizeof(valeur*)*lignes);

    for (int ligne = 0; ligne < lignes; ++ligne)
    {
        matriceSortie->contenu[ligne] = malloc(sizeof(valeur)*colonnes);

        for (int colonne = 0; colonne < colonnes; ++colonne)
        {
            matriceSortie->contenu[ligne][colonne] = 0.0;
        }
    }

    return matriceSortie;
}

void supprimer_matrice(matrice* matriceEntree)
{
    for (int ligne = 0; ligne < matriceEntree->lignes; ++ligne)
    {
        free(matriceEntree->contenu[ligne]);
    }

    free(matriceEntree->contenu);
    free(matriceEntree);
}

matrice* sous_matrice(matrice* matriceEntree, int ligneDepart, int colonneDepart, int nbreLignes, int nbreColonnes)
{
    matrice* matriceSortie = creer_matrice(nbreLignes, nbreColonnes);

    for (int i = 0; i < nbreLignes; ++i)
    {
        for (int j = 0; j < nbreColonnes; ++j)
        {
            matriceSortie->contenu[i][j] = matriceEntree->contenu[i+ligneDepart][j+colonneDepart];
        }
    }

    return matriceSortie;
}

matrice* add_matrice(matrice* matriceA, matrice* matriceB)
{
    if (matriceA->lignes != matriceB->lignes || matriceA->colonnes != matriceB->colonnes)
    {
        fprintf(stderr, "addition impossible:\n");
        fprintf(stderr, "\t-> les deux matrices doivent-être de taille identique.\n");
        fprintf(stderr, "\t\t'matriceA': {%d} lignes\n", matriceA->lignes);
        fprintf(stderr, "\t\t'matriceA': {%d} colonnes\n", matriceA->colonnes);
        fprintf(stderr, "\t\t'matriceB': {%d} lignes\n", matriceB->lignes);
        fprintf(stderr, "\t\t'matriceB': {%d} colonnes\n", matriceB->colonnes);
        exit(EXIT_FAILURE);
    }

    matrice* matriceSortie = creer_matrice(matriceA->lignes, matriceA->colonnes);

    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne)
    {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne)
        {
            matriceSortie->contenu[ligne][colonne] = matriceA->contenu[ligne][colonne] + matriceB->contenu[ligne][colonne];
        }
    }

    return matriceSortie;
}

matrice* soustract_matrice(matrice* matriceA, matrice* matriceB)
{
    if (matriceA->lignes != matriceB->lignes || matriceA->colonnes != matriceB->colonnes)
    {
        fprintf(stderr, "soustraction impossible:\n");
        fprintf(stderr, "\t-> les deux matrices doivent-être de taille identique.\n");
        fprintf(stderr, "\t\t'matriceA': {%d} lignes\n", matriceA->lignes);
        fprintf(stderr, "\t\t'matriceA': {%d} colonnes\n", matriceA->colonnes);
        fprintf(stderr, "\t\t'matriceB': {%d} lignes\n", matriceB->lignes);
        fprintf(stderr, "\t\t'matriceB': {%d} colonnes\n", matriceB->colonnes);
        exit(EXIT_FAILURE);
    }

    matrice* matriceSortie = creer_matrice(matriceA->lignes, matriceA->colonnes);

    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne)
    {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne)
        {
            matriceSortie->contenu[ligne][colonne] = matriceA->contenu[ligne][colonne] - matriceB->contenu[ligne][colonne];
        }
    }

    return matriceSortie;
}

matrice* mult_matrice(matrice* matriceA, matrice* matriceB)
{
    if (matriceA->colonnes != matriceB->lignes)
    {
        fprintf(stderr, "multiplication impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceA' doit avoir autant de colonnes que la matrice 'matriceB' a de lignes.\n");
        fprintf(stderr, "\t\t'matriceA': {%d} colonnes\n", matriceA->colonnes);
        fprintf(stderr, "\t\t'matriceB': {%d} lignes\n", matriceB->lignes);
        exit(EXIT_FAILURE);
    }

    int tailleCommune = matriceA->colonnes;
    matrice* matriceSortie = creer_matrice(matriceA->lignes, matriceB->colonnes);

    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne)
    {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne)
        {
            valeur somme = 0;

            for (int k = 0; k < tailleCommune; ++k)
            {
                somme += matriceA->contenu[ligne][k] * matriceB->contenu[k][colonne];
            }

            matriceSortie->contenu[ligne][colonne] = somme;
        }
    }

    return matriceSortie;
}

matrice* transp_matrice(matrice* matriceEntree)
{
    matrice* matriceSortie = creer_matrice(matriceEntree->colonnes, matriceEntree->lignes);

    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne)
    {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne)
        {
            matriceSortie->contenu[ligne][colonne] = matriceEntree->contenu[colonne][ligne];
        }
    }

    return matriceSortie;
}

matrice* dilatation_matrice(valeur scalaire, matrice* matriceEntree)
{
    matrice* matriceSortie = creer_matrice(matriceEntree->lignes, matriceEntree->colonnes);

    for (int ligne = 0; ligne < matriceSortie->lignes; ++ligne)
    {
        for (int colonne = 0; colonne < matriceSortie->colonnes; ++colonne)
        {
            matriceSortie->contenu[ligne][colonne] = scalaire * matriceEntree->contenu[ligne][colonne];
        }
    }

    return matriceSortie;
}

valeur det_matrice(matrice* matriceEntree)
{
    if (matriceEntree->colonnes != matriceEntree->lignes)
    {
        fprintf(stderr, "calcule du déterminant impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceEntree' doit avoir autant de colonnes que de lignes.\n");
        fprintf(stderr, "\t\t'matriceEntree': {%d} lignes\n", matriceEntree->lignes);
        fprintf(stderr, "\t\t'matriceEntree': {%d} colonnes\n", matriceEntree->colonnes);
        exit(EXIT_FAILURE);
    }

    int tailleCommune = matriceEntree->colonnes;

    // cas d'arrêt

    if (tailleCommune == 1)
    {
        return matriceEntree->contenu[0][0];
    }

    // recherche meilleur ligne/colonne (celle possèdant le moins de 0)

    int idMeilleur   = 0;
    int nbreZeroMax  = 0;
    int nbreZero     = 0;
    bool estVertical = false;

    for (int ligne  = 0; ligne < tailleCommune; ++ligne)
    {
        nbreZero = 0;

        for (int colonne = 0; colonne < tailleCommune; ++colonne)
        {
            if (matriceEntree->contenu[ligne][colonne] == 0.0)
            {
                nbreZero += 1;
            }
        }

        if (nbreZero > nbreZeroMax)
        {
            nbreZeroMax = nbreZero;
            idMeilleur  = ligne;
        }
    }

    for (int colonne  = 0; colonne < tailleCommune; ++colonne)
    {
        nbreZero = 0;

        for (int ligne = 0; ligne < tailleCommune; ++ligne)
        {
            if (matriceEntree->contenu[ligne][colonne] == 0.0)
            {
                nbreZero += 1;
            }
        }

        if (nbreZero > nbreZeroMax)
        {
            nbreZeroMax = nbreZero;
            idMeilleur  = colonne;
            estVertical = true;
        }
    }

    //  calcule du déterminant (on se ramène à la transoposé si le calcule le plus intéressant est sur une colonne)

    // cas simple

    if (nbreZeroMax == tailleCommune)
    {
        return 0.0;
    }

    // cas général

    valeur det   = 0.0;
    valeur signe = (idMeilleur % 2 == 0) ? 1.0 : -1.0;
    int ligneTemp;
    int colonneTemp;

    if (estVertical)
    {
        matriceEntree = transp_matrice(matriceEntree);
    }

    matrice* matriceTemp = creer_matrice(tailleCommune-1, tailleCommune-1);

    for (int colonneEnCours = 0; colonneEnCours < tailleCommune; ++colonneEnCours)
    {
        if (matriceEntree->contenu[idMeilleur][colonneEnCours] != 0.0)
        {
            ligneTemp = 0;

            for (int ligne = 0; ligne < tailleCommune; ++ligne)
            {
                colonneTemp = 0;

                if (ligne != idMeilleur)
                {
                    for(int colonne = 0; colonne < tailleCommune; ++colonne)
                    {
                        if (colonne != colonneEnCours)
                        {
                            matriceTemp->contenu[ligneTemp][colonneTemp] = matriceEntree->contenu[ligne][colonne];
                            colonneTemp++;
                        }
                    }

                    ligneTemp++;
                }
            }

            det += signe * matriceEntree->contenu[idMeilleur][colonneEnCours] * det_matrice(matriceTemp);
        }

        signe *= -1.0;
    }

    if (estVertical)
    {
        supprimer_matrice(matriceEntree);
    }
    supprimer_matrice(matriceTemp);

    return det;
}

void echange_ligne(matrice* matriceEntree, int ligne1, int ligne2)
{
    if (ligne1 >= matriceEntree->lignes || ligne1 < 0 || ligne2 >= matriceEntree->lignes || ligne2 < 0)
    {
        fprintf(stderr, "échange des lignes impossible:\n");
        fprintf(stderr, "\t-> les lignes 'ligne1' et 'ligne2' doivent exister.\n");
        fprintf(stderr, "\t\t'ligne1': {%d}\n", ligne1);
        fprintf(stderr, "\t\t'ligne2': {%d}\n", ligne2);
        exit(EXIT_FAILURE);
    }

    for (int colonne = 0; colonne < matriceEntree->colonnes; ++colonne)
    {
        valeur stockageTemp                     = matriceEntree->contenu[ligne1][colonne];
        matriceEntree->contenu[ligne1][colonne] = matriceEntree->contenu[ligne2][colonne];
        matriceEntree->contenu[ligne2][colonne] = stockageTemp;
    }
}

void echange_colonne(matrice* matriceEntree, int colonne1, int colonne2)
{
    if (colonne1 >= matriceEntree->colonnes || colonne1 < 0 || colonne2 >= matriceEntree->colonnes || colonne2 < 0)
    {
        fprintf(stderr, "échange des colonnes impossible:\n");
        fprintf(stderr, "\t-> les colonnes 'colonne1' et 'colonne2' doivent exister.\n");
        fprintf(stderr, "\t\t'colonne1': {%d}\n", colonne1);
        fprintf(stderr, "\t\t'colonne2': {%d}\n", colonne2);
        exit(EXIT_FAILURE);
    }

    for (int ligne = 0; ligne < matriceEntree->lignes; ++ligne)
    {
        valeur stockageTemp                     = matriceEntree->contenu[ligne][colonne1];
        matriceEntree->contenu[ligne][colonne1] = matriceEntree->contenu[ligne][colonne2];
        matriceEntree->contenu[ligne][colonne2] = stockageTemp;
    }
}

void combinaison_lignes(matrice* matriceEntree, int ligneDest, valeur scalaire, int ligneAjout)
{
    if (ligneDest >= matriceEntree->lignes || ligneDest < 0 || ligneAjout >= matriceEntree->lignes || ligneAjout < 0)
    {
        fprintf(stderr, "combinaison des lignes impossible:\n");
        fprintf(stderr, "\t-> les lignes 'ligneDest' et 'ligneAjout' doivent exister.\n");
        fprintf(stderr, "\t\t'ligneDest': {%d}\n", ligneDest);
        fprintf(stderr, "\t\t'ligneAjout': {%d}\n", ligneAjout);
        exit(EXIT_FAILURE);
    }

    for (int colonne = 0; colonne < matriceEntree->colonnes; ++colonne)
    {
        matriceEntree->contenu[ligneDest][colonne] = matriceEntree->contenu[ligneDest][colonne] + scalaire * matriceEntree->contenu[ligneAjout][colonne];
    }
}

void combinaison_colonne(matrice* matriceEntree, int colonneDest, valeur scalaire, int colonneAjout)
{
    if (colonneDest >= matriceEntree->colonnes || colonneDest < 0 || colonneAjout >= matriceEntree->colonnes || colonneAjout < 0)
    {
        fprintf(stderr, "combinaison des colonnes impossible:\n");
        fprintf(stderr, "\t-> les colonnes 'colonneDest' et 'colonneAjout' doivent exister.\n");
        fprintf(stderr, "\t\t'colonneDest': {%d}\n", colonneDest);
        fprintf(stderr, "\t\t'colonneAjout': {%d}\n", colonneAjout);
        exit(EXIT_FAILURE);
    }

    for (int ligne = 0; ligne < matriceEntree->lignes; ++ligne)
    {
        matriceEntree->contenu[ligne][colonneDest] = matriceEntree->contenu[ligne][colonneDest] + scalaire * matriceEntree->contenu[ligne][colonneAjout];
    }
}

void dilatation_ligne(matrice* matriceEntree, valeur scalaire, int ligne)
{
    if (ligne >= matriceEntree->lignes || ligne < 0)
    {
        fprintf(stderr, "dilatation de la ligne impossible:\n");
        fprintf(stderr, "\t-> la ligne 'ligne' doit exister.\n");
        fprintf(stderr, "\t\t'ligne': {%d}\n", ligne);
        exit(EXIT_FAILURE);
    }

    if (scalaire == 0)
    {
        fprintf(stderr, "dilatation de la ligne impossible:\n");
        fprintf(stderr, "\t-> le scalaire 'scalaire' doit être non nul.\n");
        fprintf(stderr, "\t\t'scalaire': {%f}\n", scalaire); // à modifier si valeur change de type
        exit(EXIT_FAILURE);
    }

    for (int colonne = 0; colonne < matriceEntree->colonnes; ++colonne)
    {
        matriceEntree->contenu[ligne][colonne] = scalaire * matriceEntree->contenu[ligne][colonne];
    }
}

void dilatation_colonne(matrice* matriceEntree, valeur scalaire, int colonne)
{
    if (colonne >= matriceEntree->colonnes || colonne < 0)
    {
        fprintf(stderr, "dilatation de la colonne impossible:\n");
        fprintf(stderr, "\t-> la colonne 'colonne' doit exister.\n");
        fprintf(stderr, "\t\t'colonne': {%d}\n", colonne);
        exit(EXIT_FAILURE);
    }

    if (scalaire == 0)
    {
        fprintf(stderr, "dilatation de la colonne impossible:\n");
        fprintf(stderr, "\t-> le scalaire 'scalaire' doit être non nul.\n");
        fprintf(stderr, "\t\t'scalaire': {%f}\n", scalaire); // à modifier si valeur change de type
        exit(EXIT_FAILURE);
    }

    for (int ligne = 0; ligne < matriceEntree->lignes; ++ligne)
    {
        matriceEntree->contenu[ligne][colonne] = scalaire * matriceEntree->contenu[ligne][colonne];
    }
}

matrice* inv_matrice(matrice* matriceEntree, bool verifie)
{
    if (matriceEntree->colonnes != matriceEntree->lignes)
    {
        fprintf(stderr, "calcule de l'inverse impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceEntree' doit avoir autant de colonnes que de lignes.\n");
        fprintf(stderr, "\t\t'matriceEntree': {%d} lignes\n", matriceEntree->lignes);
        fprintf(stderr, "\t\t'matriceEntree': {%d} colonnes\n", matriceEntree->colonnes);
        exit(EXIT_FAILURE);
    }

    if (verifie)
    {
        valeur det = det_matrice(matriceEntree);
        printf("%f\n", det);

        if (det == 0)
        {
            fprintf(stderr, "calcule de l'inverse impossible:\n");
            fprintf(stderr, "\t-> la matrice 'matriceEntree' est de déterminant nul.\n");
            fprintf(stderr, "\t\tdéterminant: {%f}\n", det); // à modifier si valeur change de type
            exit(EXIT_FAILURE);

        }
    }

    // mise en place

    int tailleCommune = matriceEntree->colonnes;
    matrice* matriceTemp = creer_matrice(tailleCommune, 2*tailleCommune);

    for (int ligne = 0; ligne < tailleCommune; ++ligne)
    {
        for (int colonne = 0; colonne < tailleCommune; ++ colonne)
        {
            matriceTemp->contenu[ligne][colonne] = matriceEntree->contenu[ligne][colonne];

            if (ligne == colonne)
            {
                matriceTemp->contenu[ligne][colonne+tailleCommune] = 1;
            }
            else
            {
                matriceTemp->contenu[ligne][colonne+tailleCommune] = 0;
            }
        }
    }

    // algorithme de Gauss-Jordan

    int lignePivot   = -1;

    for (int colonne = 0; colonne < tailleCommune; ++colonne)
    {
        int ligneMax = lignePivot+1;
        int maximum  = matriceTemp->contenu[lignePivot+1][colonne];

        for (int ligne = lignePivot+2; ligne < tailleCommune; ++ligne)
        {
            if (matriceTemp->contenu[ligne][colonne] > maximum)
            {
                maximum = matriceTemp->contenu[ligne][colonne];
                ligneMax = ligne;
            }
        }

        if (matriceTemp->contenu[ligneMax][colonne] != 0)
        {
            lignePivot += 1;
            dilatation_ligne(matriceTemp, 1/matriceTemp->contenu[ligneMax][colonne], ligneMax);

            if (ligneMax != lignePivot)
            {
                echange_ligne(matriceTemp, lignePivot, ligneMax);
            }

            for (int ligne = 0; ligne < tailleCommune; ++ligne)
            {
                if (ligne != lignePivot)
                {
                    combinaison_lignes(matriceTemp, ligne, (-1)*matriceTemp->contenu[ligne][colonne], lignePivot);
                }
            }
        }
    }

    // recopie de la matrice inverse

    matrice* matriceSortie = creer_matrice(tailleCommune, tailleCommune);
    for (int ligne = 0; ligne < tailleCommune; ++ligne)
    {
        for (int colonne = 0; colonne < tailleCommune; ++colonne)
        {
            matriceSortie->contenu[ligne][colonne] = matriceTemp->contenu[ligne][colonne+tailleCommune];
        }
    }

    supprimer_matrice(matriceTemp);
    return matriceSortie;
}

matrice* affichage_matrice(matrice* matriceEntree)
{
    printf("Affichage:\n");

    for (int ligne = 0; ligne < matriceEntree->lignes; ++ligne)
    {
        printf("|");
        for (int colonne = 0; colonne < matriceEntree->colonnes; ++colonne)
        {
            printf(" {%f} ", matriceEntree->contenu[ligne][colonne]);
        }
        printf("|\n");
    }
}
