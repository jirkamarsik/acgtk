################################################################################
# AC_LIB_CHECKING
# permet le test de la présence d'une librairie Ocaml dans le repertoire
# standard d'installation des librairies (ocamlc -where), dans un repertoire 
# dédié (+...), ou installée avec ocamlfind
# @param $1 : le nom a afficher de la librairie
# @param $2 : le repertoire standard de la librairie (par ex : zen pour -I +zen)
# @param $3 : le nom du fichier contenant la librairie
# @param $4 : le nom de la librairie pour ocamlfind
# @param $5 : le module a tester pour vérifier la présence de la librairie
# @param $6 : les options de "compilation" pour la verification de la présence de la lib
# @param $7 : true si la librairie est obligatoire, false si elle est optionnelle
AC_DEFUN([AC_LIB_CHECKING], 

	MACRO="A"
	MACRO=$MACRO"C_LIB_CHECKING"
	

	NAME=$1
	STD_FOLDER=$2
	OFIND_NAME=$3
	LIB_FILE=$4
	MOD=$5
	OPTIONS=$6
	NO_OPTIONAL=$7
	
	# test au cas ou un argument obligatoire est manquant
	if test "$NAME" = "" || test "$STD_FOLDER" = "" || test "$OFIND_NAME" = "" || test "$LIB_FILE" = ""  || test "$MOD" = "" ; then
		echo ""
		echo "Can't check a library"
		echo "Missing argument in $MACRO($NAME,$STD_FOLDER,$OFIND_NAME,$LIB_FILE,$MOD,$OPTIONS,$NO_OPTIONAL)"
		echo ""
		exit 1
	fi
	
	# affichage du nom de la librairie a checker (avec details sur l'optionalité)
	if test "$NO_OPTIONAL" = true; then
		AC_MSG_CHECKING([for requested library : $NAME])
	else
		AC_MSG_CHECKING([for optional library  : $NAME])
	fi
	
	# si le fichier ml de test existe
	if test -f c_check_$STD_FOLDER.ml ; then
		# si on peut le supprimer
		if test -w c_check_$STD_FOLDER.ml ; then
			# on le supprimer
			rm c_check_$STD_FOLDER.ml >& /dev/null
		# sinon on arrete et on balance un message d'erreur (ie on a pas les droits sur le fichier
		else
			AC_MSG_FAILURE(Cannot remove c_check_$2.ml. Please change its right with chmod 666 c_check_$2.ml)
		fi
	fi
	
	# on prepare le fichier ml de test 
	echo "open $MOD;;" > c_check_$STD_FOLDER.ml
	
	# si on arrive à l'executer avec la librairie dans le rep de lib de caml
	if (ocamlc -c $OPTIONS c_check_$STD_FOLDER.ml >& /dev/null) ; then
		# pas besoin d'include
		LIB_INCLUDE=""
		AC_MSG_RESULT(Found in ocaml lib directory)
	
	# sinon on essaie avec un repertoire dedié à la lib (-I +xxx yyy.cma)
	elif (ocamlc -c $OPTIONS -I +$STD_FOLDER $LIB_FILE.cma c_check_$STD_FOLDER.ml >& /dev/null) ; then
		# si ca marche, on s'arrete et on precise le include
		LIB_INCLUDE="-I +$STD_FOLDER"
		AC_MSG_RESULT(Found in $STD_FOLDER directory => $LIB_INCLUDE)
		
	# sinon, on essaie avec ocamlfind
	else

		# si on n'a pas ocamlfind , on s'arrete (dans le cas d'une lib oblig.) ou
		# on lance un warning (dans le cas d'une lib optionelle)
		if test "ocamlfind" = no ; then
			if test "$NO_OPTIONAL" = "true"; then 
	  			AC_MSG_ERROR(The $NAME library is missing.)
	  			LIB_INCLUDE="no"
	  		else
	  			AC_MSG_RESULT(The $NAME library is missing.)
	  			LIB_INCLUDE="no"
	  		fi
	  		
	  	# sinon, ocamlfind est présent
		else	
			# on regarde déjà si la lib est installée avec ocamlfind
			if ocamlfind query $OFIND_NAME > /dev/null 2>&1 ; then
				# si c'est le cas, on recupere le repertoire d'installation et le include correspondant
			    LIB_INCLUDE=`ocamlfind query $OFIND_NAME`
			    LIB_INCLUDE="-I $LIB_INCLUDE"
			    
			    # on teste maintenant si on peut exectuer le fichier ml de test
			    if (ocamlc -c $OPTIONS $LIB_INCLUDE $LIB_FILE.cma c_check_$STD_FOLDER.ml >& /dev/null) ; then
			    	# on y arrive, on dit qu'on a trouvé la lib avec ocamlfind
					AC_MSG_RESULT(Found with ocamlfind => $LIB_INCLUDE)
				# si on y arrive pas
			    else 
			    	# suivant l'optionalité de la lib: un warning ou une erreur
			    	if test "$NO_OPTIONAL" = "true"; then 
			  			AC_MSG_ERROR(The $NAME library is missing.)
			  			LIB_INCLUDE="no"
			  		else
			  			AC_MSG_RESULT(The $NAME library is missing.)
			  			LIB_INCLUDE="no"
			  		fi
				fi
				
			# si la lib n'est pas installée avec ocamlfind
			else	
		    	# suivant l'optionalité de la lib: un warning ou une erreur
				if test "$NO_OPTIONAL" = "true"; then 
		  			AC_MSG_ERROR(The $NAME library is missing.)
		  			LIB_INCLUDE="no"
		  		else
		  			AC_MSG_RESULT(The $NAME library is missing.)
		  			LIB_INCLUDE="no"
		  		fi
			fi
		fi	
	fi
	
	# suppression du fichier ml de test
	rm c_check_$STD_FOLDER.ml >& /dev/null	
	rm c_check_$STD_FOLDER.cmo >& /dev/null
	rm c_check_$STD_FOLDER.cmi >& /dev/null	
)
################################################################################
