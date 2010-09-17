This file describes working of GAMS interface with Matlab/R/Octave.

TO execute script "test" do from R:  source("test")

(It is assumed that all these software are already installed)

1. Matlab
In matlab, mex interface is used to create GAMS interface. For each function "rgdx/wgdx/gams/gdxInfo" a separate .mex file is being created for different operating system. Before using any of these function please make sure that it's corresponding mex file should be in Matlab PATH. Please read gdxmrw.pdf for detail. For further question please contact Prof. Ferris or Steve Dirkse. 

2. R
In R, GAMS interface will be distributed through R extension package. All these function will come in zipped R package which can be installed by executing "R CMD INSTALL /complete path of package/" from command prompt. There will be different package for different operating system. Once the package is installed please add gams directory path to "LD_LIBRARY_PATH". 

For further detail please read README file in R sub folder or contact Rishabh Jain(rishabhjain99@gmail.com)


3. Octave
Octave support mex API, thus all the source code that was written for Matlab will still work for Octave as well. But it need to be recompiled to create new mex files. To recompile mex function please execute  command similar to"mex -v -O files.c". Mex function in Octave can create multiple object copy in memory and it is not recommended. Instead create Octave specific "oct" fuction using Octave API.

For further detail please refer http://www.gnu.org/software/octave/doc/interpreter/Dynamically-Linked-Functions.html.
