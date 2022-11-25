/* - Importations - */

#include "standard_lib.h"
#include "module_matrice.h"


/* - Structures et types - */

typedef struct {
    double x;
    double y;
} point;

typedef struct {
    int* idPoints; //indices des points du triangle (dans un tableau de points à côté)
    double** delta_x;
    double** delta_y;
    double z; //Epaisseur de l'élément
} element;

typedef struct {
    double valeur;
    bool fixe;
} force;


/* - Déclarations (f: fonctions; p: procedure; (a) possible en analogique) - */

point init_point(double x, double y);
// f - l. - renvoie un point (x,y)

void init_element(element* monElement, int pt0, int pt1, int pt2, double z, point* listePoints);
// p - l. - initialise un element: 
// pt0; pt1; pt2 : les sommets 1,2 et 3 du triangle
// z : epaisseur du triangle

void liberer_element(element* monElement);
// p - l. - nettoie la mémoire utilisée par un élément qu'on souhaite ne plus utiliser

double obtenir_det_jacobien(element* monElement);
// f (a) - l. - calcule le determinant jacobian d'un élément

matrice* obtenir_matrice_B(element* monElement);
// f - l. - renvoie la matrice B "strain displacement matrix", de taille 3 x 6

double obtenir_aire(element* monElement);
// f - l. - renvoie l'aire d'un element

matrice* obtenir_matrice_D(bool type, double coefficientElasicite, double coefficientPoisson);
// f - l. - renvoie la matrice D "constitutive matrix", de taille 3 x 3

matrice* obtenir_matrice_Ke(element* monElement,bool type,double coefficientElasticite, double coefficientPoisson);
// f - l. - renvoie la matrice Ke "stiffness matrix", de taille 6 x 6 (je crois)

void assemble_Kg(matrice* Ke, matrice* Kg, element* monElement);
// p - l. - remplit la matrice globale Kg avec les valeurs de Ke, au bon endroit

matrice* reduction_matrice(force* mesForces, matrice* ancienneKg, int nbrePoints);
// f - l. - créer une version de la matrice sans les points fixés

int obtenir_demi_bande(element** elements, int nbElements, int nbPoints);
// f - l. - recherche la meilleure demi-bande pour réduire l'espace mémoire de la matrice (non utilisé)


/* - Fonctions et procédures - */

/* ~ fonctions du type element ~ */
point init_point(double x, double y){
    point pt;
    pt.x = x;
    pt.y = y;
    return pt;
}

void init_element(element* monElement, int idPt0, int idPt1, int idPt2, double z, point* listePoints) {

    monElement->idPoints    = malloc(sizeof(int) * 3);
    monElement->idPoints[0] = idPt0;
    monElement->idPoints[1] = idPt1;
    monElement->idPoints[2] = idPt2;
    
    monElement->delta_x = malloc(sizeof(double*) * 3);
    monElement->delta_y = malloc(sizeof(double*) * 3);
    for (int i = 0; i < 3; i++) {
        monElement->delta_x[i] = malloc(sizeof(double) * 3);
        monElement->delta_y[i] = malloc(sizeof(double) * 3);
        for (int j = 0; j < 3; j++) {
            monElement->delta_x[i][j] = listePoints[monElement->idPoints[i]].x - listePoints[monElement->idPoints[j]].x;
            monElement->delta_y[i][j] = listePoints[monElement->idPoints[i]].y - listePoints[monElement->idPoints[j]].y;
        }
    }
    
    monElement->z = z;
}

void liberer_element(element* monElement) {

    for (int i = 0; i < 3; i++) {
        free(monElement->delta_x[i]);
    }
    free(monElement->delta_x);
    
    for (int i = 0; i < 3; i++) {
        free(monElement->delta_y[i]);
    }
    free(monElement->delta_y);
    
    free(monElement->idPoints);
    free(monElement); // éventuelle source d'erreur
}

/* ~ fonctions générals ~ */

double obtenir_det_jacobien(element* monElement) {
    return abs(monElement->delta_x[0][2] * monElement->delta_y[1][2] - monElement->delta_x[1][2] * monElement->delta_y[0][2]);
}

matrice* obtenir_matrice_B(element* monElement) {

    matrice* B  = creer_matrice(3,6);
    double detJ = obtenir_det_jacobien(monElement);
    
    double y12 = monElement->delta_y[1][2];
    double x21 = monElement->delta_x[2][1];
    B->contenu[0][0] = y12/detJ;
    B->contenu[1][0] = 0.;
    B->contenu[2][0] = x21/detJ;
    
    B->contenu[0][1] = 0;
    B->contenu[1][1] = x21/detJ;
    B->contenu[2][1] = y12/detJ;
    
    
    double y20 = monElement->delta_y[2][0];
    double x02 = monElement->delta_x[0][2];
    B->contenu[0][2] = y20/detJ;
    B->contenu[1][2] = 0.;
    B->contenu[2][2] = x02/detJ;
    
    B->contenu[0][3] = 0;
    B->contenu[1][3] = x02/detJ;
    B->contenu[2][3] = y20/detJ;
    
    double y01 = monElement->delta_y[0][1];
    double x10 = monElement->delta_x[1][0];
    B->contenu[0][4] = y01/detJ;
    B->contenu[1][4] = 0.;
    B->contenu[2][4] = x10/detJ;
    
    B->contenu[0][5] = 0;
    B->contenu[1][5] = x10/detJ;
    B->contenu[2][5] = y01/detJ;    
    
    return B;
}

double obtenir_aire(element* monElement) {
    return 0.5 * obtenir_det_jacobien(monElement);
}

// https://fr.wikipedia.org/wiki/Coefficient_de_Poisson
matrice* obtenir_matrice_D(bool type, double coefficientElasticite, double coefficientPoisson) {

    matrice* D = creer_matrice(3, 3);
    double c;
    
    if (type) { // problème de type contrainte mécanique
    
        c = coefficientElasticite / (1.0 - (coefficientPoisson * coefficientPoisson));

        D->contenu[0][0] = 1.0 * c;
        D->contenu[1][0] = coefficientPoisson * c;
        D->contenu[2][0] = 0.0;

        D->contenu[0][1] = coefficientPoisson * c;
        D->contenu[1][1] = 1.0 * c;
        D->contenu[2][1] = 0.0;

        D->contenu[0][2] = 0.0;
        D->contenu[1][2] = 0.0;
        D->contenu[2][2] = (1 - coefficientPoisson ) * 0.5 * c;
    } else { // problème de type déformation élastique
    
        c = coefficientElasticite / ((1 + coefficientPoisson) * (1.0 - 2 * coefficientPoisson));
        
        D->contenu[0][0] = (1.0 - coefficientPoisson) * c;
        D->contenu[1][0] = coefficientPoisson * c;
        D->contenu[2][0] = 0.0;

        D->contenu[0][1] = coefficientPoisson * c;
        D->contenu[1][1] = (1 - coefficientPoisson) * c;
        D->contenu[2][1] = 0.0;

        D->contenu[0][2] = 0.0;
        D->contenu[1][2] = 0.0;
        D->contenu[2][2] = (0.5 - coefficientPoisson) * c;
    }
    return D;
}

matrice* obtenir_matrice_Ke(element* monElement,bool type, double coefficientElasticite, double coefficientPoisson) {

    matrice* B = obtenir_matrice_B(monElement);
    matrice* B_transposee = transp_matrice(B);
    matrice* D = obtenir_matrice_D(type,coefficientElasticite,coefficientPoisson);

    matrice* produit_1 = mult_matrice(B_transposee,D);
    
    matrice* produit_2 = mult_matrice(produit_1,B);
    
    double coeff = monElement->z * obtenir_aire(monElement);
    matrice* Ke = dilatation_matrice(coeff,produit_2);
    
    supprimer_matrice(B);
    supprimer_matrice(B_transposee);
    supprimer_matrice(D);
    supprimer_matrice(produit_1);
    supprimer_matrice(produit_2);
    
    return Ke;
}

void assemble_Kg(matrice* Ke, matrice* Kg, element* monElement) {

    for (int i = 0; i < 3; i++) {
    
        int indiceEquivalentI = 2 * monElement->idPoints[i];
        
        for (int j = 0; j < 3; j++) {
        
            int indiceEquivalentJ    = 2 * monElement->idPoints[i];
            int differenceIndiceEqui = indiceEquivalentJ - indiceEquivalentI;
            
            if (differenceIndiceEqui  >= 0) {
                Kg->contenu[indiceEquivalentI][indiceEquivalentJ] = Kg->contenu[indiceEquivalentI][indiceEquivalentJ] + Ke->contenu[i][j];
            }
        }
    }
}

matrice* reduction_matrice(force* mesForces, matrice* ancienneKg, int nbrePoints) {
    
    int nombreDePoints = 0;
    for (int idPoint = 0; idPoint < 2*nbrePoints; idPoint++) {
        if (!(mesForces[idPoint].fixe)) {
            nombreDePoints++;
        }
    }
    
    matrice* nouvKg = creer_matrice(nombreDePoints, nombreDePoints);
    
    int idLigne = 0;
    for (int idPoint = 0; idPoint < 2*nbrePoints; idPoint++) {
        
        if (!(mesForces[idPoint].fixe)) {
            int idColonne = 0;
            
            for (int idCoPoint = 0; idCoPoint < 2*nbrePoints; idCoPoint++) {
                
                if(!(mesForces[idCoPoint].fixe)) {
                    
                    nouvKg->contenu[idLigne][idColonne] = ancienneKg->contenu[idPoints][idCoPoint];
                    
                    idColonne++;
                }
            }
            
            idLigne++;
        }
    }
    return nouvKg;
}

int obtenir_demi_bande(element** elements, int nbElements, int nbPoints) {

    int max     = elements[0]->idPoints[0];
    int min     = elements[0]->idPoints[0];
    int maxDiff = 0;
    
    for (int i = 0; i < nbElements; i++) {
        for (int id = 0; id < 3; id++) {
            if (elements[i]->idPoints[id] > max) {
                max = elements[i]->idPoints[id];
            }
            if (elements[i]->idPoints[id] < min) {
                min = elements[i]->idPoints[id]; 
            }
        }
        int diff = max - min;
        if (maxDiff < diff) {
            maxDiff = diff;
        }
    }
    
    // avec maxDiff on déduit la largeur de la bande à utiliser pour réduire la matrice
    int largeurBande = (maxDiff + 1) * 2;
    if (largeurBande > nbPoints * 2) {
        largeurBande = nbPoints * 2;
    }

    return largeurBande;
}

void fem(element** elements, int nbElements, force* mesForces, int nbPoints, bool type, double coefficientElasticite, double coefficientPoisson) {
    matrice* Kg = creer_matrice(2*nbPoints, 2*nbPoints);
    
    for (int id = 0; id < nbElements; id++) {
        matrice* Ke = obtenir_matrice_Ke(elements[id], type, coefficientElasticite, coefficientPoisson);
        assemble_Kg(Ke, Kg, elements[id]);
    }
    
    // inversion
    matrice* reducKg    = reduction_matrice(mesForces, Kg, nbrePoints);
    matrice* invReducKg = inv_matrice(reducKg, true);
    supprimer_matrice(reducKg);
    
    // matrice réduite des forces
    matrice* reducForce = creer_matrice(invReducKg->ligne, 1);
    int idCurrent = 0;
    for (int idPoint = 0; idPoint < nbrePoints*2; idPoint++) {
        if (!(mesForces[idPoint].fixe)) {
            reducForce->contenu[idCurrent][1] = mesForces[idPoint]->valeur;
            idCurrent++;
        }
    }
    
    // matrice de déplacement réduite
    matrice* reducDeplacements = mult_matrice(invReducKg, reducForce);
    supprimer_matrice(reducForce);
    supprimer_matrice(invReducKg);
    
    // matrice des déplacements totaux
    matrice* deplacements = creer_matrice(2*nbrePoints, 1);
    idCurrent = 0;
    for (int idPoint = 0; idPoint < nbrePoints*2; idPoint++) {
        if (mesForces[idPoint].fixe) {
            deplacements->contenu[idPoint][1] = 0;
        } else {
            deplacements->contenu[idPoint][1] = reducDeplacements->contenu[idCurrent][1];
            idCurrent++;
        }
    }
    supprimer_matrice(reducDeplacements);
    
    // matrice des forces totales
    matrice* forces = mult_matrice(Kg, deplacements);
    
    // formatage pour transfère
    FILE* fichier;
    fichier = open("resultat.femp", "w");
    
    // points dans elements puis points:
    // x, dx, fx
    // y, dy, fy
        
    return;
}


/* ~ main ~ */

int main() {
    
    element* triangle = malloc(sizeof(element));
    /*
    init_element(triangle, 0, 2, 1, 0, 2, 3, 2); //Plus valable, il faut une liste de points, et des indices

    affichage_matrice(obtenir_matrice_Ke(triangle,false,1,0.25));
    affichage_matrice(obtenir_matrice_Ke(triangle,true,1,0.25));
    */
    return EXIT_SUCCESS;
}
