FEM_triangles : FEM_triangles.o module_matrice.o
	gcc -o FEM_triangles FEM_triangles.o module_matrice.o 
	
FEM_triangles.o : FEM_triangles.c
	gcc -c FEM_triangles.c
	
module_matrice.o : module_matrice.c
	gcc -c module_matrice.c
	



