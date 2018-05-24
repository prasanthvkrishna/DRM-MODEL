# Make file for DRM model

######## EDIT HERE ##########
    # Compiler and Flags
FC 		= gfortran
FFLAGS 		= -c #-Wall -fcheck=all
NETCDF 		= /home/pvk03/Cascades/local/netcdf-4.4.0/
NETCDFMOD	= -I$(NETCDF)/include
NETCDFLIB	= -L$(NETCDF)/lib -lnetcdf -lnetcdff
LINKER		= $(FC) -o

##################################################
######## DON' CHANGE ANYTHING BELOW###############
OBJS = drm.o module_solver.o module_drm_fun.o module_netcdf_io.o module_param.o drmtype.o module_listfile.o module_rd_namelist.o
#Programe Name
DRM = drm.exe

# Source File are in
VPATH = src

# Object folder
OBJDIR = objs

# Module Folder
MODDIR = mods

# Excecutable
EXEDIR=run


model : $(DRM)

# Create the model
$(DRM): $(OBJS)
	@echo "--------------------------------------"
	@echo "--------Compiling DRM Model-----------"
	@echo "--------------------------------------"
	$(LINKER) $(DRM) $(OBJS) $(NETCDFMOD) $(NETCDFLIB) 
	mv *.o   $(OBJDIR)
	mv *.mod $(MODDIR)
	mv *.exe $(EXEDIR)
	@echo "**************************************"
	@echo "*      Compiling DRM:Success         *"
	@echo "**************************************"
	
%.o:   %.f90
	@echo "--------------------------------------"
	@echo "--------Compiling files $<"
	@echo "--------------------------------------"
	$(FC) $(FFLAGS) $(NETCDFMOD) $(NETCDFLIB)  $<
	
# Clean compilation
clean:
	@echo "--------------------------------------"
	@echo "--------Make Clean Model--------------"
	@echo "--------------------------------------"
	rm -rf *~ *.exe *.o *.mod
	rm -rf $(OBJDIR)/*.o $(OBJDIR)/*~
	rm -rf $(MODDIR)/*.mod $(MODDIR)/*~
	rm -rf $(EXEDIR)/*.exe $(EXEDIR)/*~
	rm -rf $(VPATH)/*~

drm.o 			: drm.f90 module_solver.o module_netcdf_io.o module_param.o drmtype.o module_listfile.o module_rd_namelist.o
module_solver.o		: module_solver.f90 module_drm_fun.o module_netcdf_io.o module_param.o drmtype.o module_listfile.o module_rd_namelist.o
module_drm_fun.o 	: module_drm_fun.f90 module_netcdf_io.o module_param.o drmtype.o module_listfile.o module_rd_namelist.o
module_netcdf_io.o      : module_netcdf_io.f90 module_param.o drmtype.o module_listfile.o module_rd_namelist.o
module_param.o 		: module_param.f90 drmtype.o module_listfile.o module_rd_namelist.o
drmtype.o 		: drmtype.f90 module_listfile.o module_rd_namelist.o
module_listfile.o 	: module_listfile.f90 module_rd_namelist.o
module_rd_namelist.o    : module_rd_namelist.f90
