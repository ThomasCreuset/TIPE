/*  -Fichier entête-  */

#include "analogue.h"


/* -Fonctions- */

double calcul_proportion(double tensionSeuil, matrice* donnees) {
    
    // recherche de max des valeurs absolues
    
    double maxAbs = 0.0;
    double tampon;
    
    for (int i = 0; i < donnees->lignes; ++i) {
        for (int j = 0; j < donnees->colonnes; ++j) {
            tampon = abs(donnees->contenu[i][j]);
            if (tampon > maxAbs) {
                maxAbs = tampon;
            }
        }
    }
    
    // calcule du coefficient de proportionnalité

    return tensionSeuil / maxAbs;
}

void adapte_donnees(double coefficientProportionalite, matrice* donnees) {

    // calcul du nouveau coefficient de proportionalite
    
    for (int i = 0; i < donnees->lignes; ++i) {
        for (int j = 0; j < donnees->colonnes; ++j) {
            donnees->contenu[i][j] = coefficientProportionalite * donnees->contenu[i][j];
        }
    }
    
    return;
}
