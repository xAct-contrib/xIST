# xIST (linear Scalar Tensor)

**Contents**: This repository contains the following two packages: 1) *xIST (linear Scalar Tensor)* - a package implementing general scalar-tensor theories and tools for their investigation. 2) *GDE (General Dark Energy)* - a Mathematica notebook building on xIST to investigate these theories. Latest versions and release dates are:

*xIST (linear Scalar Tensor)*, version 0.7.3, {2016, 3, 29}, 
*GDE (General Dark Energy)*, version 0.8.0, {2016, 3, 29} 

CopyRight (C) 2016, *Johannes Noller*, under the General Public License. 


**Installation notes**: xIST and GDE require a working installation of Mathematica and xAct. For details, downloads and documentation for xAct please go to http://www.xact.es. After unzipping, the xIST folder should be copied into the xAct folder (i.e. into the same directory as, for example, the xTensor folder). Depending on the installation directory chosen for xAct this can vary. Common destinations are:

Linux:
   - system-wide: /usr/share/Mathematica/Applications/xAct
   - single-user:$HOME/.Mathematica/Applications/xAct

Mac OS:
   - system-wide:/Library/Mathematica/Applications/xAct
   - single-user: /Users/<user>/Library/Mathematica/Applications/xAct

MSWindows:
   - system-wide: C:\Documents and settings\All Users\Application data\Mathematica\Applications\xAct\
   - single-user: C:\Documents and settings\<user>\Application Data\Mathematica\Applications\xAct\
   
After a succeful installation the xIST package can be loaded by typing

<<xAct\`xIST\`

into a Mathematica notebook. Note that there are no spaces in this command. GDE automatically loads the xIST package and is self-contained, so all computations in the GDE notebook can be run once xIST is installed. xIST and GDE have been tested on Windows, Mac and Linux and on Mathematica 10.3.1.0 and 9.0.1.0. 


**Methodology and computation notes**: The GDE notebook takes the general ansatz for a quadratic action of perturbations for a tensor and a scalar in an FRW-like background as discussed in the accompanying paper A general theory of linear cosmological
perturbations: scalar-tensor and vector-tensor theories (arXiv:1504.xxxxx) and as is already contained in the xIST package. It then computes all Noether constraints from requiring this action to be diffeomorphism-invariant. Finally it solves all constraints and in that way obtains the true number of independent background functions/coefficients in the full quadratic perturbative action. We present the full actions derived in this way and expressions relating the free coefficients to the original ansatz.


**Comparison with accompanying paper "arXiv:1504.xxxxx"**: The Lagrangians used in the \`\`Lagrangian detup" sections throughout this file are imported from the xIST package and should be compared with equations 4.5-4.9 and G.1-G.2 of the accompanying arXiv:1504.xxxxx. Note that, while in the paper different letters (T's and L's) are used for background (time-dependent) coefficients in this action, here we stick with a uniform convention of labelling all coefficients in our starting action as L's. Otherwise our starting expressions are identical and the final answers below when expressed in terms of ``alpha'' coefficients are identical, see e.g. equation 4.15 as compared to the final action in the 3rd order Beyond Horndeski case discussed below or equation 3.30 in the paper as compared to the final action in the GR case computed below (here final actions are expressed in Fourier-space, whereas in the paper we have kept explicit spatial derivatives in the final actions).


**Known issues and bugs**: All the individual sections below can be computed without any issues/problems arising. However, when computing the \`\`Lagrangian setup'' sections of several of the following sections in succession (e.g. as part of evaluating the full notebook), a problem with the internal vbundles and index recognition appears. This is due to a bug in the MakeRule function and its vbundle handling in xAct (giving rise to a "VBundleOfIndex::unknown" error that aborts the computation). If this occurs, the simplest solutions is to re-start the kernel/only carry out the computations in one of the following subsections at a time and re-starting the kernel and loading xIST whenever one would like to move on. I'm working to resolve this issue fully so no such manual workaround is required. 
