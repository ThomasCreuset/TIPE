/*
Documentation:

    - Types de conditions limites (toujours l'une des deux)

    {-1} Dirichlet : contrainte de position
    {1}  Neumann : contrainte de force

    - Forme final du problème initiale

    [Fc]   [K1  K2]   [Ui]
    [  ] = [      ] x [  ]
    [Fi]   [K3  K4]   [Uc]
, où les indices i correspondent aux inconnus et c au connus pour U (déplacements) et F (forces)

    K1 : degree_de_liberte x degree_de_liberte
    K4 : degree_de_contrainte x degree_de_contrainte

*/


/* - Imports - */

#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <string.h>
#include "module_matrice.h"


/* - Constantes - */

#define Dimension 3 // s.u.
#define NoeudsParElement 2 // s.u.


/* - Structures et types - */

typedef struct
{
    matrice* position;
    matrice* deplacement;
    matrice* force;
    int* typeConstraite;
    int* indicesK;
} noeud_t;

typedef struct
{
    int* indices;
    double e; // Pa module de Young
    double a; // m^2 section
} element_t;

typedef struct
{
    noeud_t* noeuds;
    element_t* elements;
    int nbreNoeuds;
    int nbreElements;
    int degreeDeLiberte;
    int degreeDeContrainte;
} probleme_t;


/* - Décalrations - */

probleme_t* lecture_donnees(char* lien);
/* f - récupère les données à l'adresse fournie */

void ecrit_resultat(char* lien, probleme_t* probleme);
/* f - écrit les données traités à l'adresse fournie */

void supprime_probleme(probleme_t* probleme);
/* f - delete la structure du problème */

matrice* raideur_element(probleme_t* probleme, int i);
/* f - créer la matrice de raideur associée à l'élément d'indice i élément */

matrice* creation_matrice_raideur(probleme_t* probleme);
/* f - créer la matrice de raideur pour le problème */

matrice* recuperation_forces_connues(probleme_t* probleme);
/* f - récupération du vecteur colonne des forces connues */

matrice* recuperation_deplacements_connus(probleme_t* probleme);
/* f - récupération du vecteur colonne des déplacements connus */

void applique_elements_finis(char* lienDonnees, char* lienSortie);
/* f - effectue la méthode des éléments finis */


/* - Fonctions - */

probleme_t* lecture_donnees(char* lien)
{
    // Initialisation du problème

    probleme_t* probleme = malloc(sizeof(probleme_t));

    probleme->degreeDeLiberte    = 0;
    probleme->degreeDeContrainte = 0;

    // Ouverture du fichier

    FILE* fichier = NULL;

    fichier = fopen(lien, "r");

    // Lecture des informations primaires

    fscanf(fichier, "%d;%d\n", &(probleme->nbreNoeuds), &(probleme->nbreElements));

    // Lecture des noeuds

    probleme->noeuds = malloc(sizeof(noeud_t) * probleme->nbreNoeuds);

    double valeur;

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        // Initialisation du noeud

        probleme->noeuds[i].position       = creer_matrice(Dimension, 1);
        probleme->noeuds[i].deplacement    = creer_matrice(Dimension, 1);
        probleme->noeuds[i].force          = creer_matrice(Dimension, 1);
        probleme->noeuds[i].typeConstraite = malloc(sizeof(int) * Dimension);
        probleme->noeuds[i].indicesK       = malloc(sizeof(int) * Dimension);

        // Lecture du noeud

        for (int d = 0; d < Dimension; ++d)
        {
            fscanf(fichier, "%lf;%d;%lf\n", &(probleme->noeuds[i].position->contenu[d][0]), &(probleme->noeuds[i].typeConstraite[d]), &valeur);

            if (probleme->noeuds[i].typeConstraite[d] == 1)
            {
                ++(probleme->degreeDeLiberte);
                probleme->noeuds[i].force->contenu[d][0]       = valeur;
                probleme->noeuds[i].deplacement->contenu[d][0] = 0.0;
                probleme->noeuds[i].indicesK[d]                = probleme->degreeDeLiberte;
            }
            else
            {
                --(probleme->degreeDeContrainte);
                probleme->noeuds[i].deplacement->contenu[d][0] = valeur;
                probleme->noeuds[i].force->contenu[d][0]       = 0.0;
                probleme->noeuds[i].indicesK[d]                = probleme->degreeDeContrainte;
            }
        }
    };

    // Formatage des indices dans K

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        for (int d = 0; d < Dimension; ++d)
        {
            if (probleme->noeuds[i].indicesK[d] < 0)
            {
                probleme->noeuds[i].indicesK[d] = abs(probleme->noeuds[i].indicesK[d]) + (probleme->degreeDeLiberte) - 1;
            }
            else
            {
                probleme->noeuds[i].indicesK[d] = probleme->noeuds[i].indicesK[d] - 1;
            }
        }
    }

    probleme->degreeDeContrainte = abs(probleme->degreeDeContrainte);

    // Lecture des elements

    probleme->elements = malloc(sizeof(element_t) * probleme->nbreElements);

    for (int i = 0; i < probleme->nbreElements; ++i)
    {
        // Initialisation de l'élément

        probleme->elements[i].indices = malloc(sizeof(int) * NoeudsParElement);

        // Lecture de l'élément

        fscanf(fichier, "%d;%d;%lf;%lf\n", &(probleme->elements[i].indices[0]), &(probleme->elements[i].indices[1]), &(probleme->elements[i].e), &(probleme->elements[i].a));
    }

    fclose(fichier);
    
    return probleme;
}

void ecrit_resultat(char* lien, probleme_t* probleme)
{
    // Ouverture du fichier

    FILE* fichier = NULL;

    fichier = fopen(lien, "w");

    // Ecriture des informations primaires

    fprintf(fichier, "%d;%d\n", probleme->nbreNoeuds, probleme->nbreElements);

    // Ecriture des noeuds

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        for (int d = 0; d < Dimension; ++d)
        {
            fprintf(fichier, "%lf;%lf;%lf\n", probleme->noeuds[i].position->contenu[d][0], probleme->noeuds[i].deplacement->contenu[d][0], probleme->noeuds[i].force->contenu[d][0]);
        }
    }

    // Ecriture des elements

    for (int i = 0; i < probleme->nbreElements; ++i)
    {
        fprintf(fichier, "%d;%d;%lf;%lf\n", probleme->elements[i].indices[0], probleme->elements[i].indices[1], probleme->elements[i].e, probleme->elements[i].a);
    }

    fclose(fichier);
}

void supprime_probleme(probleme_t* probleme)
{
    // free des noeuds

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        supprimer_matrice(probleme->noeuds[i].position);
        supprimer_matrice(probleme->noeuds[i].deplacement);
        supprimer_matrice(probleme->noeuds[i].force);
        free(probleme->noeuds[i].typeConstraite);
        free(probleme->noeuds[i].indicesK);
    }

    free(probleme->noeuds);

    // free des elements

    for (int i = 0; i < probleme->nbreElements; ++i)
    {
        free(probleme->elements[i].indices);
    }

    free(probleme->elements);

    // free du probleme

    free(probleme);

    return;
}

matrice* raideur_element(probleme_t* probleme, int i)
{
    // Calcul de la rotation selon Oz

    int indice1 = probleme->elements[i].indices[0];
    int indice2 = probleme->elements[i].indices[1];

    double x1 = probleme->noeuds[indice1].position->contenu[0][0];
    double y1 = probleme->noeuds[indice1].position->contenu[1][0];
    double x2 = probleme->noeuds[indice2].position->contenu[0][0];
    double y2 = probleme->noeuds[indice2].position->contenu[1][0];

    double deltax = x2 - x1;
    double deltay = y2 - y1;

    double longeurProj = sqrt(deltax * deltax + deltay * deltay);

    double cosinus;
    double sinus;

    if (sqrt(longeurProj * longeurProj) <= 0.001) // on évite la division par 0
    {
        cosinus = 1.0;
        sinus   = 0.0;
    }
    else
    {
        cosinus = deltax / longeurProj;
        sinus   = deltay / longeurProj;
    }

    // - création de la matrice de rotation R(-angle) (A)

    matrice* mat_A = creer_matrice(Dimension * NoeudsParElement, Dimension * NoeudsParElement);

    for (int n = 0; n < NoeudsParElement; ++n)
    {
        mat_A->contenu[0 + Dimension * n][0 + Dimension * n] = cosinus;
        mat_A->contenu[0 + Dimension * n][1 + Dimension * n] = sinus;
        mat_A->contenu[1 + Dimension * n][0 + Dimension * n] = -sinus;
        mat_A->contenu[1 + Dimension * n][1 + Dimension * n] = cosinus;
        mat_A->contenu[2 + Dimension * n][2 + Dimension * n] = 1.0;
    }

    // - calcule des positions après la première rotation

    matrice* positions = creer_matrice(Dimension * 2, 1);
    for (int d = 0; d < Dimension; ++d)
    {
        positions->contenu[d][0]           = probleme->noeuds[indice1].position->contenu[d][0];
        positions->contenu[Dimension+d][0] = probleme->noeuds[indice2].position->contenu[d][0];
    }
    matrice* nouvellePositions = mult_matrice(mat_A, positions);

    // Calcul de la rotation selon Oy

    x1        = nouvellePositions->contenu[0][0];
    double z1 = nouvellePositions->contenu[2][0];
    x2        = nouvellePositions->contenu[3][0];
    double z2 = nouvellePositions->contenu[5][0];

    deltax        = x2 - x1;
    double deltaz = z2 - z1;

    double longeur = sqrt(deltax * deltax + deltaz * deltaz); // conservée par rotation (et plus de composante selon y)

    if (sqrt(longeur * longeur) <= 0.001) // on évite la division par 0
    {
        cosinus = 1.0;
        sinus   = 0.0;
    }
    else
    {
        cosinus = deltax / longeur;
        sinus   = deltaz / longeur;
    }

    // - création de la matrice de rotation R(-angle) (B)

    matrice* mat_B = creer_matrice(Dimension * NoeudsParElement, Dimension * NoeudsParElement);

    for (int n = 0; n < NoeudsParElement; ++n)
    {
        mat_B->contenu[0 + Dimension * n][0 + Dimension * n] = cosinus;
        mat_B->contenu[0 + Dimension * n][2 + Dimension * n] = -sinus;
        mat_B->contenu[1 + Dimension * n][1 + Dimension * n] = 1.0;
        mat_B->contenu[2 + Dimension * n][0 + Dimension * n] = sinus;
        mat_B->contenu[2 + Dimension * n][2 + Dimension * n] = cosinus;
    }

    // Calcul de la matrice de raideur de l'élément

    // - création de la matrice dans la base canonique (K)

    matrice* mat_K = creer_matrice(Dimension * NoeudsParElement, Dimension * NoeudsParElement);

    double constante = (probleme->elements[i].e) * (probleme->elements[i].a) / longeur;

    mat_K->contenu[0][0]                 = constante;
    mat_K->contenu[0][Dimension]         = -constante;
    mat_K->contenu[Dimension][0]         = -constante;
    mat_K->contenu[Dimension][Dimension] = constante;

    // - calcule de la matrice dans la base tournée tA x tB x K x B x A = t(BA) x K x BA

    matrice* mat_BA  = mult_matrice(mat_B, mat_A);
    matrice* mat_tBA = transp_matrice(mat_BA);

    matrice* mat_tBAK      = mult_matrice(mat_tBA, mat_K);
    matrice* mat_K_finale = mult_matrice(mat_tBAK, mat_BA);

    // free des matrices intermédiaires et retour

    supprimer_matrice(mat_A);
    supprimer_matrice(positions);
    supprimer_matrice(nouvellePositions);
    supprimer_matrice(mat_K);
    supprimer_matrice(mat_BA);
    supprimer_matrice(mat_tBA);
    supprimer_matrice(mat_tBAK);

    return mat_K_finale;
}

matrice* creation_matrice_raideur(probleme_t* probleme)
{
    // Initialisation de la matrice
    
    matrice* matriceRaideur = creer_matrice((probleme->nbreNoeuds) * Dimension, (probleme->nbreNoeuds) * Dimension);
    
    // Assemblage de la matrice

    for (int i = 0; i < probleme->nbreElements; ++i)
    {
        matrice* matriceElement = raideur_element(probleme, i);

        // Intération sur toutes les conbinaisons des noeuds

        for (int noeud1 = 0; noeud1 < NoeudsParElement; ++noeud1)
        {
            for (int direction1 = 0; direction1 < Dimension; ++direction1)
            {
                for (int noeud2 = 0; noeud2 < NoeudsParElement; ++noeud2)
                {
                    for (int direction2 = 0; direction2 < Dimension; ++direction2)
                    {
                        int ligne     = probleme->noeuds[probleme->elements[i].indices[noeud1]].indicesK[direction1];
                        int colonne   = probleme->noeuds[probleme->elements[i].indices[noeud2]].indicesK[direction2];
                        matriceRaideur->contenu[ligne][colonne] += matriceElement->contenu[noeud1 * Dimension + direction1][noeud2 * Dimension + direction2];
                    }
                }
            }
        }

        supprimer_matrice(matriceElement);
    }

    return matriceRaideur;
}

matrice* recuperation_forces_connues(probleme_t* probleme)
{
    matrice* forces_connues = creer_matrice(probleme->degreeDeLiberte, 1); // vecteur colonne

    int indice = 0;

    // la création de la matrice est fortement liée au tri choisi

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        for (int d = 0; d < Dimension; ++d)
        {
            if (probleme->noeuds[i].typeConstraite[d] == 1)
            {
                forces_connues->contenu[indice][0] = probleme->noeuds[i].force->contenu[d][0];
                ++indice;
            }
        }
    }

    return forces_connues;
}

matrice* recuperation_deplacements_connus(probleme_t* probleme)
{
    matrice* deplacements_connus = creer_matrice(probleme->degreeDeContrainte, 1); // vecteur colonne

    int indice = 0;

    // la création de la matrice est fortement liée au tri choisi

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        for (int d = 0; d < Dimension; ++d)
        {
            if (probleme->noeuds[i].typeConstraite[d] == -1)
            {
                deplacements_connus->contenu[indice][0] = probleme->noeuds[i].deplacement->contenu[d][0];
                ++indice;
            }
        }
    }

    return deplacements_connus;
}

void applique_elements_finis(char* lienDonnees, char* lienSortie)
{
    // précalculs

    probleme_t* probleme = lecture_donnees(lienDonnees);
    
    printf("donnees lues\n");

    matrice* matriceRaideur = creation_matrice_raideur(probleme);
    
    printf("matrice créée\n");

    // récupération des matrices

    matrice* forces_connues      = recuperation_forces_connues(probleme);
    matrice* deplacements_connus = recuperation_deplacements_connus(probleme);
    matrice* k1                  = sous_matrice(matriceRaideur, 0, 0, probleme->degreeDeLiberte, probleme->degreeDeLiberte);
    matrice* k2                  = sous_matrice(matriceRaideur, 0, probleme->degreeDeLiberte, probleme->degreeDeLiberte, probleme->degreeDeContrainte);
    matrice* k3                  = sous_matrice(matriceRaideur, probleme->degreeDeLiberte, 0, probleme->degreeDeContrainte, probleme->degreeDeLiberte);
    matrice* k4                  = sous_matrice(matriceRaideur, probleme->degreeDeLiberte, probleme->degreeDeLiberte, probleme->degreeDeContrainte, probleme->degreeDeContrainte);

    // calculs matriciels

    // A / F = Fc - K2 x Uc
    matrice* k2_x_uc = mult_matrice(k2, deplacements_connus);

    matrice* force_temp = soustract_matrice(forces_connues, k2_x_uc);

    // B / Ui = K1^-1 x F

    matrice* invK1 = inv_matrice(k1, false);

    matrice* deplacements_inconnus = mult_matrice(invK1, force_temp);

    // C / Fi = K3 x Ui + K4 x Up

    matrice* mult1 =  mult_matrice(k3, deplacements_inconnus);
    matrice* mult2 =  mult_matrice(k4, deplacements_connus);

    matrice* forces_inconnues = add_matrice(mult1, mult2);

    // mise à jour des noeuds

    int idTampDepl = 0;
    int idTampForc = 0;

    for (int i = 0; i < probleme->nbreNoeuds; ++i)
    {
        for (int d = 0; d < Dimension; ++d)
        {
            if (probleme->noeuds[i].typeConstraite[d] == 1)
            {
                probleme->noeuds[i].deplacement->contenu[d][0] = deplacements_inconnus->contenu[idTampDepl][0];
                ++idTampDepl;
            }
            else
            {
                probleme->noeuds[i].force->contenu[d][0] = forces_inconnues->contenu[idTampForc][0];
                ++idTampForc;
            }
        }
    }

    // free et retour

    ecrit_resultat(lienSortie, probleme);

    supprimer_matrice(matriceRaideur);
    supprimer_matrice(forces_connues);
    supprimer_matrice(deplacements_connus);
    supprimer_matrice(k1);
    supprimer_matrice(k2);
    supprimer_matrice(k3);
    supprimer_matrice(k4);
    supprimer_matrice(k2_x_uc);
    supprimer_matrice(force_temp);
    supprimer_matrice(invK1);
    supprimer_matrice(deplacements_inconnus);
    supprimer_matrice(mult1);
    supprimer_matrice(mult2);
    supprimer_matrice(forces_inconnues);

    supprime_probleme(probleme);

    return;
}



/* - Main (exemple) - */

int main()
{
    srand(time(NULL));

    applique_elements_finis("donnees.txt", "resultat.txt");

    printf("Termine.\n");
    return 0;
}
