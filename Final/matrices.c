/*  -Fichier entête-  */

#include "matrices.h"


/* -Fonctions- */

double** creer_matrice(int lignes, int colonnes)
{
    double** matriceSortie  = malloc(sizeof(double*) * lignes);
    
    for (int ligne = 0; ligne < lignes; ++ligne)
    {
        matriceSortie[ligne] = malloc(sizeof(double) * colonnes);
        
        for (int colonne = 0; colonne < colonnes; ++colonne)
        {
            matriceSortie[ligne][colonne] = 0.0;
        }
    }
    return matriceSortie;
}

void free_matrice(double** matrice, int nombreLignes)
{
    for (int i = 0; i < nombreLignes; ++i)
    {
        free(matrice[i]);
    }
    
    free(matrice);
    
    return;
}

double** mult_matrice(double** matriceA, int nbreLignesA, int nbreColonnesA, int deltaLigneA, int deltaColonneA, double** matriceB, int nbreLignesB, int nbreColonnesB, int deltaLigneB, int deltaColonneB)
{
    if (nbreColonnesA != nbreLignesB)
    {
        fprintf(stderr, "multiplication impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceA' doit avoir autant de colonnes que la matrice 'matriceB' a de lignes.\n");
        fprintf(stderr, "\t\t'matriceA': {%d} colonnes\n", nbreColonnesA);
        fprintf(stderr, "\t\t'matriceB': {%d} lignes\n", nbreLignesB);
        exit(EXIT_FAILURE);
    }
    
    int tailleCommune = nbreColonnesA;
    
    double** matriceSortie = creer_matrice(nbreLignesA, nbreColonnesB);
    
    for (int ligne = 0; ligne < nbreLignesA; ++ligne)
    {
        for (int colonne = 0; colonne < nbreColonnesB; ++colonne)
        {
            double somme = 0;
            for (int k = 0; k < tailleCommune; ++k) {
                somme += matriceA[ligne + deltaLigneA][k + deltaColonneA] * matriceB[k + deltaLigneB][colonne + deltaColonneB];
            }
            matriceSortie[ligne][colonne] = somme;
        }
    }
    return matriceSortie;
}

double** transp_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes)
{
    double** matriceSortie = creer_matrice(nbreColonnes, nbreLignes);
    
    for (int ligne = 0; ligne < nbreLignes; ++ligne)
    {
        for (int colonne = 0; colonne < nbreColonnes; ++colonne)
        {
            matriceSortie[ligne][colonne] = matriceEntree[colonne][ligne];
        }
    }
    return matriceSortie;
}

double det_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes)
{
    if (nbreColonnes != nbreLignes)
    {
        fprintf(stderr, "calcule du déterminant impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceEntree' doit avoir autant de colonnes que de lignes.\n");
        fprintf(stderr, "\t\t'matriceEntree': {%d} lignes\n", nbreLignes);
        fprintf(stderr, "\t\t'matriceEntree': {%d} colonnes\n", nbreColonnes);
        exit(EXIT_FAILURE);
    }
    
    int tailleCommune = nbreColonnes;
    
    // cas d'arrêt
    
    if (tailleCommune == 1) {
        return matriceEntree[0][0];
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
            if (matriceEntree[ligne][colonne] == 0.0)
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
            if (matriceEntree[ligne][colonne] == 0.0)
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
    
    if (nbreZeroMax == tailleCommune) {
        return 0.0;
    }
    
    // cas général
    
    double det   = 0.0; 
    double signe = (idMeilleur % 2 == 0) ? 1.0 : -1.0;
    int ligneTemp;
    int colonneTemp;
    
    if (estVertical)
    {
        matriceEntree = transp_matrice(matriceEntree, nbreLignes, nbreColonnes);
    }
    
    double** matriceTemp = creer_matrice(tailleCommune-1, tailleCommune-1);
    
    for (int colonneEnCours = 0; colonneEnCours < tailleCommune; ++colonneEnCours)
    {
        if (matriceEntree[idMeilleur][colonneEnCours] != 0.0)
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
                            matriceTemp[ligneTemp][colonneTemp] = matriceEntree[ligne][colonne];
                            colonneTemp++;
                        }
                    }
                    ligneTemp++;
                }
            }
            det += signe * matriceEntree[idMeilleur][colonneEnCours] * det_matrice(matriceTemp, tailleCommune-1, tailleCommune-1);
        }
        signe *= -1.0;
    }
    
    // free et retour
    
    if (estVertical)
    {
        free_matrice(matriceEntree, tailleCommune);
    }
    free_matrice(matriceTemp, tailleCommune);
    
    return det;
}

void echange_ligne(double** matriceEntree, int nbreColonnes, int ligne1, int ligne2)
{
    for (int colonne = 0; colonne < nbreColonnes; ++colonne)
    {
        double stockageTemp            = matriceEntree[ligne1][colonne];
        matriceEntree[ligne1][colonne] = matriceEntree[ligne2][colonne];
        matriceEntree[ligne2][colonne] = stockageTemp;
    }
}

void echange_colonne(double** matriceEntree, int nbreLignes, int colonne1, int colonne2)
{
    for (int ligne = 0; ligne < nbreLignes; ++ligne)
    {
        double stockageTemp            = matriceEntree[ligne][colonne1];
        matriceEntree[ligne][colonne1] = matriceEntree[ligne][colonne2];
        matriceEntree[ligne][colonne2] = stockageTemp;
    }
}

void combinaison_lignes(double** matriceEntree, int nbreColonnes, int ligneDest, double scalaire, int ligneAjout)
{
    for (int colonne = 0; colonne < nbreColonnes; ++colonne) {
        matriceEntree[ligneDest][colonne] = matriceEntree[ligneDest][colonne] + scalaire * matriceEntree[ligneAjout][colonne];
    }
}

void combinaison_colonne(double** matriceEntree, int nbreLignes, int colonneDest, double scalaire, int colonneAjout)
{
    for (int ligne = 0; ligne < nbreLignes; ++ligne)
    {
        matriceEntree[ligne][colonneDest] = matriceEntree[ligne][colonneDest] + scalaire * matriceEntree[ligne][colonneAjout];
    }
}

void dilatation_ligne(double** matriceEntree, int nbreColonnes, double scalaire, int ligne)
{
    if (scalaire == 0)
    {
        fprintf(stderr, "dilatation de la ligne impossible:\n");
        fprintf(stderr, "\t-> le scalaire 'scalaire' doit être non nul.\n");
        fprintf(stderr, "\t\t'scalaire': {%f}\n", scalaire);
        exit(EXIT_FAILURE);
    }
    
    for (int colonne = 0; colonne < nbreColonnes; ++colonne)
    {
        matriceEntree[ligne][colonne] = scalaire * matriceEntree[ligne][colonne];
    }
}

void dilatation_colonne(double** matriceEntree, int nbreLignes, double scalaire, int colonne)
{
    if (scalaire == 0)
    {
        fprintf(stderr, "dilatation de la colonne impossible:\n");
        fprintf(stderr, "\t-> le scalaire 'scalaire' doit être non nul.\n");
        fprintf(stderr, "\t\t'scalaire': {%f}\n", scalaire);
        exit(EXIT_FAILURE);
    }
    
    for (int ligne = 0; ligne < nbreLignes; ++ligne)
    {
        matriceEntree[ligne][colonne] = scalaire * matriceEntree[ligne][colonne];
    }
}

double** inv_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes, bool verifie)
{
    if (nbreColonnes != nbreLignes)
    {
        fprintf(stderr, "calcule de l'inverse impossible:\n");
        fprintf(stderr, "\t-> la matrice 'matriceEntree' doit avoir autant de colonnes que de lignes.\n");
        fprintf(stderr, "\t\t'matriceEntree': {%d} lignes\n", nbreLignes);
        fprintf(stderr, "\t\t'matriceEntree': {%d} colonnes\n", nbreColonnes);
        exit(EXIT_FAILURE);
    }
    
    if (verifie)
    {
        double det = det_matrice(matriceEntree, nbreColonnes, nbreLignes);

        if (det == 0)
        {
            fprintf(stderr, "calcule de l'inverse impossible:\n");
            fprintf(stderr, "\t-> la matrice 'matriceEntree' est de déterminant nul.\n");
            fprintf(stderr, "\t\tdéterminant: {%f}\n", det);
            exit(EXIT_FAILURE);
            
        }
    }
    
    // mise en place
    
    int tailleCommune = nbreColonnes;
    
    double** matriceTemp = creer_matrice(tailleCommune, 2*tailleCommune);
    
    for (int ligne = 0; ligne < tailleCommune; ++ligne)
    {
        for (int colonne = 0; colonne < tailleCommune; ++ colonne)
        {
            matriceTemp[ligne][colonne] = matriceEntree[ligne][colonne];
            
            if (ligne == colonne)
            {
                matriceTemp[ligne][colonne+tailleCommune] = 1;
            }
            else
            {
                matriceTemp[ligne][colonne+tailleCommune] = 0;
            }
        }
    }
    
    // algorithme de Gauss-Jordan
    
    int lignePivot   = -1;
    
    for (int colonne = 0; colonne < tailleCommune; ++colonne)
    {
        int ligneMax = lignePivot+1;
        int maximum  = matriceTemp[lignePivot+1][colonne];
        
        for (int ligne = lignePivot+2; ligne < tailleCommune; ++ligne)
        {
            if (matriceTemp[ligne][colonne] > maximum)
            {
                maximum = matriceTemp[ligne][colonne];
                ligneMax = ligne;
            }
        }
        
        if (matriceTemp[ligneMax][colonne] != 0)
        {
            lignePivot += 1;
            dilatation_ligne(matriceTemp, 2 * tailleCommune, 1/matriceTemp[ligneMax][colonne], ligneMax);
            
            if (ligneMax != lignePivot)
            {
                echange_ligne(matriceTemp, 2 * tailleCommune, lignePivot, ligneMax);
            }
            
            for (int ligne = 0; ligne < tailleCommune; ++ligne)
            {
                if (ligne != lignePivot)
                {
                    combinaison_lignes(matriceTemp, 2 * tailleCommune, ligne, (-1) * matriceTemp[ligne][colonne], lignePivot);
                }
            }
        }
    }
    
    // recopie de la matrice inverse
    
    double** matriceSortie = creer_matrice(tailleCommune, tailleCommune);
    
    for (int ligne = 0; ligne < tailleCommune; ++ligne)
    {
        for (int colonne = 0; colonne < tailleCommune; ++colonne)
        {
            matriceSortie[ligne][colonne] = matriceTemp[ligne][colonne+tailleCommune];
        }
    }
    
    free_matrice(matriceTemp, tailleCommune);
    
    return matriceSortie;
}

double** affichage_matrice(double** matriceEntree, int nbreLignes, int nbreColonnes) {
    printf("Matrice:\n");
    
    for (int ligne = 0; ligne < nbreLignes; ++ligne)
    {
        printf("|");
        
        for (int colonne = 0; colonne < nbreColonnes; ++colonne)
        {
            printf(" {%f} ", matriceEntree[ligne][colonne]);
        }
        printf("|\n");
    }
}
