set(CMAKE_Fortran_COMPILER "/opt/cray/pe/mpich/8.1.27/ofi/intel/2022.1/bin/mpif90")
set(CMAKE_Fortran_COMPILER_ARG1 "")
set(CMAKE_Fortran_COMPILER_ID "Intel")
set(CMAKE_Fortran_COMPILER_VERSION "2021.10.0.20230609")
set(CMAKE_Fortran_COMPILER_WRAPPER "CrayPrgEnv")
set(CMAKE_Fortran_PLATFORM_ID "Linux")
set(CMAKE_Fortran_SIMULATE_ID "")
set(CMAKE_Fortran_COMPILER_FRONTEND_VARIANT "")
set(CMAKE_Fortran_SIMULATE_VERSION "")




set(CMAKE_AR "/usr/bin/ar")
set(CMAKE_Fortran_COMPILER_AR "")
set(CMAKE_RANLIB "/usr/bin/ranlib")
set(CMAKE_Fortran_COMPILER_RANLIB "")
set(CMAKE_COMPILER_IS_GNUG77 )
set(CMAKE_Fortran_COMPILER_LOADED 1)
set(CMAKE_Fortran_COMPILER_WORKS TRUE)
set(CMAKE_Fortran_ABI_COMPILED TRUE)

set(CMAKE_Fortran_COMPILER_ENV_VAR "FC")

set(CMAKE_Fortran_COMPILER_SUPPORTS_F90 1)

set(CMAKE_Fortran_COMPILER_ID_RUN 1)
set(CMAKE_Fortran_SOURCE_FILE_EXTENSIONS f;F;fpp;FPP;f77;F77;f90;F90;for;For;FOR;f95;F95)
set(CMAKE_Fortran_IGNORE_EXTENSIONS h;H;o;O;obj;OBJ;def;DEF;rc;RC)
set(CMAKE_Fortran_LINKER_PREFERENCE 20)
if(UNIX)
  set(CMAKE_Fortran_OUTPUT_EXTENSION .o)
else()
  set(CMAKE_Fortran_OUTPUT_EXTENSION .obj)
endif()

# Save compiler ABI information.
set(CMAKE_Fortran_SIZEOF_DATA_PTR "8")
set(CMAKE_Fortran_COMPILER_ABI "ELF")
set(CMAKE_Fortran_LIBRARY_ARCHITECTURE "")

if(CMAKE_Fortran_SIZEOF_DATA_PTR AND NOT CMAKE_SIZEOF_VOID_P)
  set(CMAKE_SIZEOF_VOID_P "${CMAKE_Fortran_SIZEOF_DATA_PTR}")
endif()

if(CMAKE_Fortran_COMPILER_ABI)
  set(CMAKE_INTERNAL_PLATFORM_ABI "${CMAKE_Fortran_COMPILER_ABI}")
endif()

if(CMAKE_Fortran_LIBRARY_ARCHITECTURE)
  set(CMAKE_LIBRARY_ARCHITECTURE "")
endif()





set(CMAKE_Fortran_IMPLICIT_INCLUDE_DIRECTORIES "/opt/cray/pe/mpich/8.1.27/ofi/intel/2022.1/include;/glade/u/apps/derecho/23.09/spack/opt/spack/netcdf/4.9.2/oneapi/2023.2.1/yzvj/include;/glade/u/apps/derecho/23.09/spack/opt/spack/hdf5/1.12.2/oneapi/2023.2.1/4ybq/include;/glade/u/apps/derecho/23.09/spack/opt/spack/esmf/8.6.0/cray-mpich/8.1.27/oneapi/2023.2.1/7haa/include;/glade/u/apps/derecho/23.09/opt/view/include;/opt/cray/pe/pmi/6.1.12/include;/opt/cray/pe/pals/1.2.12/include;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/compiler/2023.2.1/linux/lib/oclfpga/include;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/compiler/2023.2.1/linux/compiler/include/intel64;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/compiler/2023.2.1/linux/compiler/include/icc;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/compiler/2023.2.1/linux/compiler/include;/usr/local/include;/usr/lib64/gcc/x86_64-suse-linux/7/include;/usr/lib64/gcc/x86_64-suse-linux/7/include-fixed;/usr/include")
set(CMAKE_Fortran_IMPLICIT_LINK_LIBRARIES "mpifort_intel;mpi_intel;pmi;pmi2;pthread;pals;mpifort_intel;mpi_intel;rt;pthread;pmi;pmi2;imf;m;pthread;ifport;ifcoremt;imf;svml;m;ipgo;irc;pthread;svml;c;gcc;gcc_s;irc_s;dl;c")
set(CMAKE_Fortran_IMPLICIT_LINK_DIRECTORIES "/opt/cray/pe/mpich/8.1.27/ofi/intel/2022.1/lib;/glade/u/apps/derecho/23.09/spack/opt/spack/netcdf/4.9.2/oneapi/2023.2.1/yzvj/lib;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/lib;/glade/u/apps/derecho/23.09/spack/opt/spack/hdf5/1.12.2/oneapi/2023.2.1/4ybq/lib;/glade/u/apps/derecho/23.09/spack/opt/spack/esmf/8.6.0/cray-mpich/8.1.27/oneapi/2023.2.1/7haa/lib;/glade/u/apps/derecho/23.09/opt/view/lib64;/glade/u/apps/derecho/23.09/opt/view/lib;/opt/cray/pe/pmi/6.1.12/lib;/opt/cray/pe/pals/1.2.12/lib;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/compiler/2023.2.1/linux/compiler/lib/intel64_lin;/glade/u/apps/common/23.08/spack/opt/spack/intel-oneapi-compilers/2023.2.1/compiler/2023.2.1/linux/lib;/usr/lib64/gcc/x86_64-suse-linux/7;/usr/lib64;/lib64;/usr/x86_64-suse-linux/lib;/lib;/usr/lib")
set(CMAKE_Fortran_IMPLICIT_LINK_FRAMEWORK_DIRECTORIES "")
