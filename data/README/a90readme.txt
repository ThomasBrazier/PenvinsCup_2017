a90
Mail responsable : s.donnier@gmail.com


ATTENTION 


Pour « a90ind.txt » :
→ Il nous manque les données biométriques (des individus) relatives au quadrat 3 de la station 0m et au quadrat 2 de la station 35m. → NA
Mais nous avons laissé les relevés sur ces quadrats (récoltés sur le terrain)


Pour « a90quad.txt » et « a90ind.txt » :
→ Pour l’importation des données sur R : nous n’arrivons pas à importer chacun des fichiers sur R, en une seule fois. Il faut donc importer les données du fichier « a90quad.txt » en deux parties puis les combiner sous un même tableau avec la fonction cbind. Il faut donc importer une première partie de tableau (« tab1 ») et une seconde partie (« tab2), puis faire tableau=cbind(tab1,tab2).