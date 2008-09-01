###############################################
#                                             #
# Makefile for directories with no executable #
#                                             #
###############################################

include ../Makefile.master

###############################
#                             #
# Set the following variables #
#                             #
###############################

# Used libraries
LIBS += dyp.cma

DYPPATH = /home/pogodall/import/ocaml/dypgen
DYPGEN_LIB = +dypgen
#DYPGEN_LIB = $(DYPPATH)/dyplib

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR = -I $(DYPGEN_LIB) -I +camlp4

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils ../logic

# Source files in the right order of dependance
#ML = error.ml abstract_syntax.ml interface.ml environment.ml entry.ml parser.ml lexer.ml data_parsing.ml
ML = error.ml interface.ml environment.ml entry.ml syntactic_data_structures.ml token.ml lexer.ml parser.ml  interactive.ml

DYP = parser.dyp
CAMLLEX = lexer.mll

EXE_SOURCES = test.ml

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

# #DYPGEN_EXE = dypgen.opt
# #DYPGEN_EXE = $(DYPPATH)/dypgen/dypgen --ocamlc "$(LIBDIR) $(I_PREVIOUS_DIRS)"
# DYPGEN_EXE = dypgen.opt --no-pp --no-obj-type --ocamlc "$(LIBDIR) $(I_PREVIOUS_DIRS)"


# parser.ml : parser.dyp
# 	 $(DYPGEN_EXE) --noemit-token-type $<

# #parser.ml : parser.dyp
# #	$(DYPGEN_EXE)  --noemit-token-type $<

# lexer.ml : lexer.mll
# 	ocamllex.opt $<

parser.ml: ../utils/utils.cmi token.cmi syntactic_data_structures.cmi \
    lexer.cmo error.cmi environment.cmi entry.cmi \
    ../logic/abstract_syntax.cmi

parser.cmo: ../utils/utils.cmi token.cmi syntactic_data_structures.cmi \
     lexer.cmo error.cmi environment.cmi entry.cmi \
     ../logic/abstract_syntax.cmi parser.cmi
parser.cmx: ../utils/utils.cmx token.cmx syntactic_data_structures.cmx \
     lexer.cmx error.cmx environment.cmx entry.cmx \
     ../logic/abstract_syntax.cmx parser.cmi
