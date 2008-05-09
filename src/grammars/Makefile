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

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR = -I +dypgen -I +camlp4

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils

# Source files in the right order of dependance
ML = error.ml abstract_syntax.ml entry.ml parser.ml lexer.ml data_parsing.ml
EXE_SOURCES = test.ml

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

parser.ml : parser.dyp
	dypgen.opt $<

lexer.ml : lexer.mll
	ocamllex.opt $<

