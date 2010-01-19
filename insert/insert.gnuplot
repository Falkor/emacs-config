################################################################################
# %f - Configuration file for Gnuplot (see http://www.gnuplot.info)
# Creation : %D %m %y
# Time-stamp: < >
#
# Copyright (c) %y %U %a
#               %o
# $Id$ 
#
# More infos and tips on Gnuplot: 
#    - http://t16web.lanl.gov/Kawano/gnuplot/index-e.html
#    - run gnuplot the type 'help gnuplot_command'
#   
# To run this script : gnuplot %f
################################################################################

set encoding iso_8859_15
#########################################################################################
# Formats de sortie
# - Par défaut : sur X11
# - LaTeX (permet d'utiliser le format LaTeX $expr$ pour les formules)
#set terminal latex
#set output "outputfile.tex"
# - EPS
#set terminal postscript eps enhanced color
#set output "outputfile.eps"
#########################################################################################

#########################################################################################
# Gestion de l'affichage des axes, des legendes etc....
#
set title  "Titre"     # Titre du graphique

# -- Gestion des axes --
set xlabel "X-AXIS"    # Label Axe X (avec 2 axes: set x{1|2}label; lettres grecs: cf Tab.5 plus bas)
set ylabel "Y-AXIS"    # Label Axe Y (avec 2 axes: set y{1|2}label)
set border 3           # format d'affichage des bordures (Valeur du paramètre: voir Tab.4 plus bas)
set xrange [0:1000]    # Intervalle Axe X 
set yrange [0:100]     # Intervalle Axe Y
#set logscale x        # Echelle logarithmique pour l'axe des x (set logscale <axes> <base>)
#set logscale y 2      # Echelle logarithmique en base 2 pour l'axe des y
#set format x "10^{%%L}"# Format d'affichage des valeurs de l'axe X (formats possibles: cf Tab.3 plus bas)
#set format y "%%3.2f"  # Format d'affichage des valeurs de l'axe Y (à la printf)
set xtics 1            # intervalle entre les graduations de l'axe X 
set ytics 1            # intervalle entre les graduations de l'axe Y
#set xtics nomirror    # Pas de reflexion de l'axe X en haut
#set ytics nomirror    # Pas de reflexion de l'axe Y à droite
#set xtics ("Avril" 1, "Mai" 2, "Juin" 3) # Changement du label pour l'axe x des graduations 1,2 et 3
#set mxtics 5          # intervalle des graduations majeures pour l'axe X
#set mytics 5          # intervalle des graduations majeures pour l'axe Y
#set grid              # affichage d'une grille pour les graduations majeures
#set sample 40         # écart entre 2 points sur une courbe

# -- Positionnement/affichage des legendes des courbes --
#set nokey             # Pas de légende
#set key left bottom   # Placé les légendes en bas à gauche
set key box            # Encadrer les légendes

#########################################################################################



####################################################################################################
# Dessin à partir d'un fichier de données
# Remarques : 
#  - les séparateur de champs sont nécessairement ' '* ou une tabulation
#  - les colonnes sont numérotées de 1 à n (cf clause using) 
plot "filename1.dat" using 1:2 title "Titre Courbe1" with lines, \
     "filename2.dat" using 1:2 title "Titre Courbe2" with points, \
     "filename3.dat" using 1:2 title "Titre Courbe3" with impulse
pause -1 "Hit return to continue" # pour faire une pause 

# Dessin d'une fonctions mathématique prédéfinie
a=10.25
b=102
f(x)=b/((x-a)**2+b)+a/sqrt(x)
plot f(x) title "f(x)" with lines


#i#################
# Gnuplot Infos:
# 
# [Tab. 1] Liste des fonctions reconnues par Gnuplot :
# ----------------------------------------------------
#   abs    valeur absolue
#   acos   arc cosinus
#   asin   arc sinus
#   atan   arc tangente
#   cos    cosinus
#   exp    exponentiel
#   int    renvoi la partie  entière de son argument
#   log    logarithme
#   log10  logarithme en base 10
#   rand   random (renvoi un nombre entre 0 et 1)
#   real   partie real
#   sgn    renvoi 1 si l'argument est positif, 0 s'il
#          est nulle, et -1 s'il est négatif
#   sin    sinus
#   sqrt   racine carré
#   tan    tangente
#
# [Tab. 2] Operateurs reconnues par Gnuplot :
# -------------------------------------------
#    Symbole      Exemple         Explication
#     **           a**b           exponentiation
#     *            a*b            multiplication
#     /            a/b            division
#     %%            a%%b            modulo
#     +            a+b            addition
#     -            a-b            soustraction
#     ==           a==b           égalité
#     !=           a!=b           inégalité
#     &            a&b            ET
#     ^            a^b            OU exclusif
#     |            a|b            OU inclusif
#     &&           a&&b           ET logique
#     ||           a||b           OU logique
#     ?:           a?b:c          opération ternaire
#
# [Tab. 3] Liste des formats reconnus (instructions 'set format') :
# -----------------------------------------------------------------
#       Format       Explanation
#       %%f           floating point notation
#       %%e or %%E     exponential notation; an "e" or "E" before the power
#       %%g or %%G     the shorter of %%e (or %%E) and %%f
#       %%x or %%X     hex
#       %%o or %%O     octal
#       %%t           mantissa to base 10
#       %%l           mantissa to base of current logscale
#       %%s           mantissa to base of current logscale; scientific power
#       %%T           power to base 10
#       %%L           power to base of current logscale
#       %%S           scientific power
#       %%c           character replacement for scientific power
#       %%P           multiple of pi
#
# [Tab. 4] Valeur attribuée aux bordure (instruction 'set border <sum>') :
# ------------------------------------------------------------------------
#        Bit   axe affiché      
#         1     bas         (x ou x1)
#         2     gauche      (y ou y1 )
#         4     haut        (x2)
#         8     droit       (y2)     
#
# [Tab. 5] Affichage des lettres grecs en mode Postcript Ex: {/Symbol a} => \alpha
# +------------------------+--------------------+
# |  ALPHABET 	SYMBOL     | alphabet 	symbol  |
# +------------------------+--------------------+
#	A 	Alpha 	   | 	a 	alpha	|
#	B 	Beta 	   |  	b 	beta 	|
#	C 	Chi 	   |  	c 	chi 	|
#	D 	Delta 	   |  	d 	delta 	|
#	E 	Epsilon    |  	e 	epsilon |
#	F 	Phi  	   |  	f 	phi 	|
#	G 	Gamma 	   | 	g 	gamma 	|
#	H 	Eta 	   |  	h 	eta 	|
#	I 	iota 	   | 	i 	iota 	|
#	K 	Kappa 	   | 	k 	kappa 	|
#	L 	Lambda 	   |  	l 	lambda 	|
#	M 	Mu 	   | 	m 	mu 	|
#	N     	Nu         |  	n       nu   	|
#	O     	Omicron    |    o       omicron |
#	P     	Pi   	   |    p       pi	|
#	Q     	Theta 	   |  	q       theta	|
#	R     	Rho  	   |   	r       rho     |
#	S     	Sigma 	   | 	s       sigma	|
#	T     	Tau 	   | 	t       tau	|
#	U     	Upsilon	   |	u       upsilon	|
#	W     	Omega 	   |	w       omega	|
#	X     	Xi 	   |	x       xi	|
#	Y     	Psi 	   |	y       psi	|
#	Z     	Zeta 	   |	z       zeta	|




