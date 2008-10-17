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
LIBS += dyp.cma str.cma

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex. -I  +campl4
LIBDIR = -I +dypgen -I +camlp4

# Directories to which the current source files depend on
PREVIOUS_DIRS = ../utils ../logic ../grammars

# Source files in the right order of dependance

ML = signature.ml lexicon.ml

EXE_SOURCES = acgc.ml

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common

