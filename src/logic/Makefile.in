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
#ML = error.ml abstract_syntax.ml interface.ml environment.ml entry.ml parser.ml lexer.ml data_parsing.ml
ML = abstract_syntax.ml lambda.ml

EXE_SOURCES = 

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common
