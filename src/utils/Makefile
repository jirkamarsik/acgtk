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
LIBS += gramlib.cma str.cma

# The corresponding directories
# (if not in the main ocaml lib directory,
# ex.  +campl4
LIBDIR = -I +camlp4

# Directories to which the current source files depend on
PREVIOUS_DIRS =

# Source files in the right order of dependance
ML = utils.ml table.ml tries.ml

# Uncomment the next line and put the name of the exe of this directory, if relevant
# EXE_SOURCES =

####################################
#                                  #
# End of the configuration section #
#                                  #
####################################

include ../Makefile.common
