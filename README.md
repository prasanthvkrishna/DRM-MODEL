# DRM-MODEL
This is the Fortran90 program for estimating precipitation recycling
 based on dynamic precipitation recycling model proposed by Dominguez
et al. (2006)

The Main program src/: drm.f90 
Module files	 src/: module_listfile.f90    --> list files based on namelist.drm input
		       module_drm_fun.f90  	  --> subroutines for drm
		       module_netcdf_io.f90   --> read netcdf input files    
		       module_rd_namelist.f90 --> read namelist.drm
		       drmtype.f90  	  --> custom data type
		       module_solver.f90	  --> drm solver
		       module_param.f90	  --> Paramter file
Makefile	 : Makefile          

Prasanth Valayamkunnath Virginia Tech
pvk03@vt.edu	
