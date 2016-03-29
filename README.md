# xIST

GDE (General Dark Energy), version 0.8.0, {2016, 3, 29}

The following information is also contained in the readme file contained in the distribution of the GDE and xIST packages on github. Please see: https://github.com/noller/xIST

Installation notes: xIST and GDE require a working installation of Mathematica and xAct. For details, downloads and documentation for xAct please go to http://www.xact.es. After unzipping, the xIST folder should be copied into the xAct folder (i.e. into the same directory as, for example, the xTensor folder). Depending on the installation directory chosen for xAct this can vary. Common destinations are:

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


Methodology and computation notes: The GDE notebook takes the general ansatz for a quadratic action of perturbations for a tensor and a scalar in an FRW-like background as discussed in the accompanying paper A general theory of linear cosmological
perturbations: scalar-tensor and vector-tensor theories (arXiv:1504.xxxxx) and as is already contained in the xIST package. It then computes all Noether constraints from requiring this action to be diffeomorphism-invariant. Finally it solves all constraints and in that way obtains the true number of independent background functions/coefficients in the full quadratic perturbative action. We present the full actions derived in this way and expressions relating the free coefficients to the original ansatz.


Comparison with "arXiv:1504.xxxxx": The Lagrangians used in the \`\`Lagrangian setup" sections throughout this file are imported from the xIST package and should be compared with equations ..... of the accompanying arXiv:1504.xxxxx. Note that, while in the paper different letters (T's and L's) are used for background (time-dependent) coefficients in this action, here we stick with a uniform convention of labelling all coefficients in our starting action as L's. Otherwise our starting expressions are identical and the final answers below when expressed in terms of ``alpha'' coefficients agree verbatim.


Known issues and bugs: When computing the ``Lagrangian setup'' sections of several of the following sections in succession (e.g. as part of evaluating the full notebook), a problem with the internal vbundles and index recognition sometimes appears. If this occurs, the simplest solutions is to re-start the kernel/only carry out the computations in one of the following subsections at a time. I'm working to resolve this issue fully so no workaround is required. 
