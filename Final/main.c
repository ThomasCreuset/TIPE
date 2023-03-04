/*
Documentation:

    - Types de conditions limites (toujours l'une des deux)

    {-1} Dirichlet : contrainte de position
    {1}  Neumann : contrainte de force

    - Forme final du problème initiale

    [Fc]   [K1  K2]   [Ui]
    [  ] = [      ] x [  ], où les indices u correspondent aux inconnus et p au connus pour U (déplacements) et F (forces)
    [Fi]   [K3  K4]   [Uc]

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
#include "matrices.h"


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
    double s; // m^2 section
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

probleme_t* lecture_donnees(char* liens);
/* f - récupère les données à l'adresse fournie */

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
    }

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

    for (int i = 0; i < probleme->nbreElements; ++i)
    {
        // Initialisation de l'élément

        probleme->elements[i].indices = malloc(sizeof(int) * NoeudsParElement);

        // Lecture de l'élément

        fscanf(fichier, "%d;%d;%lf;lf\n", &(probleme->elements[i].indices[0]), &(probleme->elements[i].indices[1]), &(probleme->elements[i].e), &(probleme->elements[i].a));
    }

    fclose(fichier);
}

matrice* raideur_element(probleme_t* probleme, int i)
{
    // Calcul de la rotation selon Oz

    int indice1 = probleme->elements[i].indices[0];
    int indice2 = probleme->elements[i].indices[1];

    double x1 = probleme->noeuds[indice1].position->contenu[0][0];
    double y1 = noeuds[indice1].position->contenu[1][0];
    double x2 = noeuds[indice2].position->contenu[0][0];
    double y2 = noeuds[indice2].position->contenu[1][0];

    double deltax = x2 - x1;
    double deltay = y2 - y1;

    double lougeurProj = sqrt(deltax * deltax + deltay * deltay);

    double cosinus = deltax / lougeurProj;
    double sinus   = deltay / lougeurProj;

    // - création de la matrice de rotation R(-angle) (A)

    matrice* mat_A = creer_matrice(Dimension * NoeudsParElement, Dimension * NoeudsParElement);

    for (int n = 0; n < NoeudsParElement; ++n)
    {
        mat_A->contenu[0 + NoeudsParElement * n][0 + NoeudsParElement * n] = cosinus;
        mat_A->contenu[0 + NoeudsParElement * n][1 + NoeudsParElement * n] = sinus;
        mat_A->contenu[1 + NoeudsParElement * n][0 + NoeudsParElement * n] = -sinus;
        mat_A->contenu[1 + NoeudsParElement * n][1 + NoeudsParElement * n] = cosinus;
        mat_A->contenu[2 + NoeudsParElement * n][2 + NoeudsParElement * n] = 1.0;
    }

    // - calcule des positions après la première rotation

    matrice* nouvellePosition1 = mult_matrice(mat_A, probleme->noeuds[indice1].position);
    matrice* nouvellePosition2 = mult_matrice(mat_A, probleme->noeuds[indice2].position);

    // Calcul de la rotation selon Oy

    x1        = nouvellePosition1->contenu[0][0];
    double z1 = nouvellePosition1->contenu[2][0];
    x2        = nouvellePosition2->contenu[0][0];
    double z2 = nouvellePosition2->contenu[2][0];

    deltax        = x2 - x1;
    double deltaz = z2 - z1;

    double longeur = sqrt(deltax * deltax + deltaz * deltaz); // conservée par rotation (et plus de composante selon y)

    cosinus = deltax / longeur;
    sinus   = deltaz / longeur;

    // - création de la matrice de rotation R(-angle) (B)

    matrice* mat_B = creer_matrice(Dimension * NoeudsParElement, Dimension * NoeudsParElement);

    for (int n = 0; n < NoeudsParElement; ++n)
    {
        mat_B->contenu[0 + NoeudsParElement * n][0 + NoeudsParElement * n] = cosinus;
        mat_B->contenu[0 + NoeudsParElement * n][2 + NoeudsParElement * n] = -sinus;
        mat_B->contenu[1 + NoeudsParElement * n][1 + NoeudsParElement * n] = 1.0;
        mat_B->contenu[2 + NoeudsParElement * n][0 + NoeudsParElement * n] = sinus;
        mat_B->contenu[2 + NoeudsParElement * n][2 + NoeudsParElement * n] = cosinus;
    }

    // Calcul de la matrice de raideur de l'élément

    // - création de la matrice dans la base canonique (K)

    matrice* mat_K = creer_matrice(Dimension * NoeudsParElement, Dimension * NoeudsParElement);

    double constante = (probleme->elements[i].e) * (probleme->elements[i].s) / longeur;

    mat_K->contenu[0][0]                 = constante;
    mat_K->contenu[0][Dimension]         = -constante;
    mat_K->contenu[Dimension][0]         = -constante;
    mat_K->contenu[Dimension][Dimension] = constante;

    // - calcule de la matrice dans la base tournée A x B x K x tA x tB

    matrice* mat_tA = transp_matrice(mat_A);
    matrice* mat_tB = transp_matrice(mat_B);

    matrice* mat_AB   = mult_matrice(mat_A, mat_B);
    matrice* mat_tAtB = mult_matrice(mat_tA, mat_tB);

    matrice* mat_ABK      = mult_matrice(mat_AB, mat_K);
    matrice* mat_K_finale = mult_matrice(mat_ABK, mat_tAtB);

    // free des matrices intermédiaires et retour

    supp_matrice(mat_A);
    supp_matrice(nouvellePosition1);
    supp_matrice(nouvellePosition2);
    supp_matrice(mat_K);
    supp_matrice(mat_tA);
    supp_matrice(mat_tB);
    supp_matrice(mat_AB);
    supp_matrice(mat_tAtB);
    supp_matrice(mat_ABK);

    return mat_K_finale;
}

matrice* creation_matrice_raideur(probleme_t* probleme)
{
    // Initialisation de la matrice

    double** matriceRaideur = creer_matrice(NombreDeNoeuds * Dimension, NombreDeNoeuds * Dimension);

    // Assemblage de la matrice

    for (int i = 0; i < NombreElements; ++i)
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

        supp_matrice(matriceElement);
    }

    return matriceRaideur;
}

matrice* recuperation_forces_connues(probleme_t* probleme)
{
    matrice* forces_connues = creer_matrice(probleme->degreeDeLiberte, 1); // vecteur colonne

    int indice = 0;

    // la création de la matrice est fortement liée au tri choisi

    for (int i = 0; i < NombreDeNoeuds; ++i)
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
    matrice* deplacements_connus = cree_matrice(probleme->degreeDeContrainte, 1); // vecteur colonne

    int indice = 0;

    // la création de la matrice est fortement liée au tri choisi

    for (int i = 0; i < NombreDeNoeuds; ++i)
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

void applique_elements_finis(char* lienDonnees, char* lienSortie) // A MODIFIER !!!!!
{
    // précalculs

    probleme_t* probleme = lecture_donnees(lien);

    matrice* matriceRaideur = creation_matrice_raideur(probleme);

    // récupération des matrices

    matrice* forces_connues      = recuperation_forces_connues(probleme);
    matrice* deplacements_connus = recuperation_deplacements_connus(probleme);
    matrice* k1                  = sous_matrice(matriceRaideur, probleme->degreeDeLiberte, probleme->degreeDeLiberte, 0, 0);
    matrice* k2                  = sous_matrice(matriceRaideur, probleme->degreeDeLiberte, probleme->degreeDeContrainte, 0, probleme->degreeDeLiberte);
    matrice* k3                  = sous_matrice(matriceRaideur, probleme->degreeDeContrainte, probleme->degreeDeLiberte, probleme->degreeDeLiberte, 0);
    matrice* k4                  = sous_matrice(matriceRaideur, probleme->degreeDeContrainte, probleme->degreeDeContrainte, probleme->degreeDeLiberte, probleme->degreeDeLiberte);

    // calculs matriciels

    // A / F = Fc - K2 x Uc
    matrice* k2_x_uc = mult_matrice(k2, deplacements_connus);

    matrice* force_temp = sous_matrice(forces_connues, k2_x_uc);

    // B / Ui = K1^-1 x F

    matrice* invK1 = inv_matrice(k1, false);

    matrice* deplacements_inconnus = mult_matrice(invK1, force_temp);

    // C / Fi = K3 x Ui + K4 x Up

    matrice* mult1 =  mult_matrice(k3, deplacements_inconnus);
    matrice* mult2 =  mult_matrice(k4, deplacements_connus);

    matrice* forces_inconnues = add_matrice(mult1, mult2);

    // mise à jour des noeuds (A FINIR)

    // mise en forme des résultats

    FILE* fichier = NULL;

    fichier = fopen("resultat.txt", "w");

    int idTampDeplC = 0;
    int idTampDeplI = 0;
    int idTampForcC = 0;
    int idTampForcI = 0;

    for (int i = 0; i < NombreDeNoeuds; ++i)
    {
        fprintf(fichier, "%d:\n", i);

        for (int j = 0; j < Dimension; ++j)
        {
            if (conditionsLimites[i][j] == 1)
            {
                fprintf(fichier, "%lf | %lf | %lf\n", noeuds[i][j], noeuds[i][j] + deplacements_inconnus[idTampDeplI][0], forces_connues[idTampForcC][0]);
                ++idTampDeplI;
                ++idTampForcC;
            }
            else
            {
                fprintf(fichier, "%lf | %lf | %lf\n", noeuds[i][j], noeuds[i][j] + deplacements_connus[idTampForcC][0], forces_inconnues[idTampForcI][0]);
                ++idTampDeplC;
                ++idTampForcI;
            }
        }
    }

    fclose(fichier);

    // free et retour (CHANGER)

    free(tableau_noeud_indice);
    free_matrice(kic_x_uc, degree_de_liberte);
    free_matrice(force_temp, degree_de_liberte);
    free_matrice(invKii, degree_de_liberte);
    free_matrice(mult1, degree_de_contrainte);
    free_matrice(mult2, degree_de_contrainte);
    free_matrice(deplacements_connus, degree_de_contrainte);
    free_matrice(deplacements_inconnus, degree_de_liberte);
    free_matrice(forces_connues, degree_de_liberte);
    free_matrice(forces_inconnues, degree_de_contrainte);
    free_matrice(matriceRaideur, NombreDeNoeuds * Dimension);

    return;
}



/* - Main (exemple) - */

int main()
{
    srand(time(NULL));

    double n1[Dimension] = {0.0, 0.0};
    double n2[Dimension] = {1.0, 0.0};
    double n3[Dimension] = {0.5, 1.0};
    double* noeuds[NombreDeNoeuds] = {n1, n2, n3};

    int e1[Dimension] = {0, 1};
    int e2[Dimension] = {1, 2};
    int e3[Dimension] = {2, 0};
    int* elements[NombreElements] = {e1, e2, e3};

    int c1[Dimension] = {-1, -1};
    int c2[Dimension] = {1, -1};
    int c3[Dimension] = {1, 1};
    int* conditionsLimites[NombreDeNoeuds] = {c1, c2, c3};

    double f1[Dimension] = {0.0, 0.0};
    double f2[Dimension] = {0.0, 0.0};
    double f3[Dimension] = {0.0, -20.0};
    double* forces[NombreDeNoeuds] = {f1, f2, f3};

    double d1[Dimension] = {0.0, 0.0};
    double d2[Dimension] = {0.0, 0.0};
    double d3[Dimension] = {0.0, -2.0};
    double* deplacements[NombreDeNoeuds] = {d1, d2, d3};

    applique_elements_finis(noeuds, elements, conditionsLimites, forces, deplacements);

    printf("Termine.\n");
    return 0;
}
