(* ::Package:: *)

(* ::Title:: *)
(*xIST: Iinearised Scalar Tensor*)


(* ::Section::Closed:: *)
(*Info, Copyright & Load xAct modules*)


xAct`xIST`$Version={"0.7.3",{2016,3,29}};


If[Unevaluated[xAct`xIST`Private`$xISTContext] === xAct`xIST`Private`$xISTContext,
	xAct`xIST`Private`$xISTContext = "xAct`xIST`"
];


Print["Package xAct`xIST`  version ",xAct`xIST`$Version[[1]],", ",xAct`xIST`$Version[[2]]];
Print["CopyRight (C) 2016, Johannes Noller, under the General Public License."]


Print["These packages come with ABSOLUTELY NO WARRANTY; for details type Disclaimer[]. This is free software, and you are welcome to redistribute it under certain conditions. See the General Public License for details."]


BeginPackage[xAct`xIST`Private`$xISTContext, {
	"xAct`xCore`",
	"xAct`xPerm`",
	"xAct`xTensor`",
	"xAct`xPert`",
	"xAct`Invar`",
	"xAct`xTras`",
	"xAct`SymManipulator`"
}]


(* ::Section::Closed:: *)
(*Definitions of fields, manifold and metric:*)


DefManifold[M3,3,{b,c,d,e,f,i,j,k,l,m,p,s,u,v,w,x,y}];

DefMetric[1,metricg[-c,-b],PD,SymbolOfCovD->{",","\[PartialD]"},FlatMetric->True,PrintAs-> "\[Delta]"];
SetOptions[SymmetryOf,ConstantMetric->True];

$Pre=ScreenDollarIndices;
Unprotect[IndexForm];
IndexForm[LI[x_]]:=ColorString[ToString[x],RGBColor[0,0,1]];
Protect[IndexForm];
(*
TexIndexForm[index_]:=Tex[ToString@IndexForm[index]];
xAct`TexAct`Private`TexUpIndex[index_]:=xAct`TexAct`Private`ExtraSpaceIfBackslash@TexIndexForm@index;
TexIndexForm[LI[index_]]:=StringJoin["(",Tex[ToString@index],")"];
*)
(* Building block fields and time parameter *)

DefParameter[t];
DefTensor[a[],{M3,t},PrintAs->"a"];
DefTensor[H[],{M3,t},PrintAs->"H"];
DefTensor[Dlapse[],{M3,t},PrintAs->"\[Delta]N"];
DefTensor[Dshift[b],{M3,t},PrintAs->"\[Delta]N"];
DefTensor[DRicciS[],{M3,t},PrintAs->"\[Delta]R"];
DefTensor[DExK[c,b],{M3,t},PrintAs->"\[Delta]K"];
DefTensor[DRicci[b,c],{M3,t},PrintAs->"\[Delta]R"];
DefTensor[deltah[b,c],{M3,t},PrintAs->"\[Delta]h"];
DefTensor[deltahTr[],{M3,t},PrintAs->"\[Delta]h"];
DefConstantSymbol[order,PrintAs->"\[Omega]"];
DefTensor[D2Ricci[],{M3,t},PrintAs->ToString[Subscript["\[Delta]","2"],StandardForm]<>"R"];
DefTensor[ddeth[],{M3,t},PrintAs->"\[Delta]"<>ToString[Sqrt[h],StandardForm]];
DefTensor[d2deth[],{M3,t},PrintAs->ToString[Subscript["\[Delta]","2"],StandardForm]<>ToString[Sqrt[h],StandardForm]];
DefTensor[D2lapse[],{M3,t},PrintAs->ToString[Subscript["\[Delta]","2"],StandardForm]<>"N"];



(* ::Section:: *)
(*Coefficients in the action:*)


(* ::Subsection::Closed:: *)
(*Defining coefficients:*)


DefTensor[LNN[],{M3,t},PrintAs->ToString[Subscript["L","NN"],StandardForm]];
DefTensor[LRR[],{M3,t},PrintAs->ToString[Subscript["L","RR"],StandardForm]];
DefTensor[LSS[],{M3,t},PrintAs->ToString[Subscript["L","SS"],StandardForm]];
DefTensor[LNR[],{M3,t},PrintAs->ToString[Subscript["L","NR"],StandardForm]];
DefTensor[LNS[],{M3,t},PrintAs->ToString[Subscript["L","NS"],StandardForm]];
DefTensor[LRS[],{M3,t},PrintAs->ToString[Subscript["L","RS"],StandardForm]];
DefTensor[LKK[],{M3,t},PrintAs->ToString[Subscript["L","KK"],StandardForm]];
DefTensor[LRTRT[],{M3,t},PrintAs->ToString[Subscript["L","RR2"],StandardForm]];
DefTensor[LKRT[],{M3,t},PrintAs->ToString[Subscript["L","KR2"],StandardForm]];
DefTensor[Ldhdh[],{M3,t},PrintAs->ToString[Subscript["L","hh"],StandardForm]];
DefTensor[LdhK[],{M3,t},PrintAs->ToString[Subscript["L","hK"],StandardForm]];
DefTensor[LdhRT[],{M3,t},PrintAs->ToString[Subscript["L","hR2"],StandardForm]];
DefTensor[Lhh1[],{M3,t},PrintAs->ToString[Subscript["L","hh+"],StandardForm]];
DefTensor[Lhh2[],{M3,t},PrintAs->ToString[Subscript["L","hhx"],StandardForm]];
DefTensor[Lh[],{M3,t},PrintAs->ToString[Subscript["L","h"],StandardForm]];
DefTensor[Lbar[],{M3,t},PrintAs->ToString[Subscript["L","bar"],StandardForm]];
DefTensor[LK[],{M3,t},PrintAs->ToString[Subscript["L","K"],StandardForm]];
DefTensor[LSSdot[],{M3,t},PrintAs->ToString[Subscript["L","S"<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[LNh[],{M3,t},PrintAs->ToString[Subscript["L","Nh"],StandardForm]];
DefTensor[LNdoth[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>"h"],StandardForm]];
DefTensor[LN[],{M3,t},PrintAs->ToString[Subscript["L","N"],StandardForm]];
DefTensor[LdSh1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]Sh+"],StandardForm]];
DefTensor[LdSh2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]Shx"],StandardForm]];
DefTensor[LdS[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]S"],StandardForm]];
DefTensor[LhK1[],{M3,t},PrintAs->ToString[Subscript["L","hK+"],StandardForm]];
DefTensor[LhK2[],{M3,t},PrintAs->ToString[Subscript["L","hKx"],StandardForm]];
DefTensor[LhR1[],{M3,t},PrintAs->ToString[Subscript["L","hR+"],StandardForm]];
DefTensor[LhR2[],{M3,t},PrintAs->ToString[Subscript["L","hRx"],StandardForm]];
DefTensor[LNK[],{M3,t},PrintAs->ToString[Subscript["L","NK"],StandardForm]];
DefTensor[LR[],{M3,t},PrintAs->ToString[Subscript["L","R"],StandardForm]];
DefTensor[LNdS[],{M3,t},PrintAs->ToString[Subscript["L","N\[PartialD]S"],StandardForm]];
DefTensor[LdNS[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]NS"],StandardForm]];
DefTensor[LNdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]],StandardForm]];
DefTensor[LNNdot[],{M3,t},PrintAs->ToString[Subscript["L","N"<>ToString[OverDot[N],StandardForm]],StandardForm]];
DefTensor[LNdotdS[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>"\[PartialD]S"],StandardForm]];
DefTensor[LdNdotS[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[N],StandardForm]<>"S"],StandardForm]];
DefTensor[LddotS[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[LddotSh1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[S],StandardForm]<>"h+"],StandardForm]];
DefTensor[LddotSh2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[S],StandardForm]<>"hx"],StandardForm]];
DefTensor[LdotSdotS[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[S],StandardForm]<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[LdSdS1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]S\[PartialD]S+"],StandardForm]];
DefTensor[LdSdS2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]S\[PartialD]Sx"],StandardForm]];
DefTensor[LdSddotS1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]S\[PartialD]"<>ToString[OverDot[N],StandardForm]<>"+"],StandardForm]];
DefTensor[LdSddotS2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]S\[PartialD]"<>ToString[OverDot[N],StandardForm]<>"x"],StandardForm]];
DefTensor[LSd2S1[],{M3,t},PrintAs->ToString[Subscript["L","S"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"S+"],StandardForm]];
DefTensor[LSd2S2[],{M3,t},PrintAs->ToString[Subscript["L","S"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"Sx"],StandardForm]];
DefTensor[LdotSd2S1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[S],StandardForm]<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"S+"],StandardForm]];
DefTensor[LdotSd2S2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[S],StandardForm]<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"Sx"],StandardForm]];
DefTensor[LdotNd2N[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"N"],StandardForm]];
DefTensor[LNd2Ndot[],{M3,t},PrintAs->ToString[Subscript["L","N"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>ToString[OverDot[N],StandardForm]],StandardForm]];
DefTensor[LSd2Sdot1[],{M3,t},PrintAs->ToString[Subscript["L","S"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>ToString[OverDot[S],StandardForm]<>"+"],StandardForm]];
DefTensor[LSd2Sdot2[],{M3,t},PrintAs->ToString[Subscript["L","S"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>ToString[OverDot[S],StandardForm]<>"x"],StandardForm]];
DefTensor[LNdSdot[],{M3,t},PrintAs->ToString[Subscript["L","N"<>"\[PartialD]"<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[LdSdot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[LdNSdot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>"N"<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[Ld2Ndot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]","2"],StandardForm]<>ToString[OverDot[N],StandardForm]],StandardForm]];
DefTensor[LdNdNdot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>"N"<>"\[PartialD]"<>ToString[OverDot[N],StandardForm]],StandardForm]];
DefTensor[Ld2N[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]","2"],StandardForm]<>"N"],StandardForm]];
DefTensor[LdNdN[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>"N"<>"\[PartialD]"<>"N"],StandardForm]];
DefTensor[LNd2N[],{M3,t},PrintAs->ToString[Subscript["L","N"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"N"],StandardForm]];
DefTensor[Lhd2N1[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"N+"],StandardForm]];
DefTensor[Lhd2N2[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"Nx"],StandardForm]];
DefTensor[LdSK1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]SK+"],StandardForm]];
DefTensor[LdSK2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]SKx"],StandardForm]];
DefTensor[LNdotNdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>ToString[OverDot[N],StandardForm]],StandardForm]];
DefTensor[LNdotK[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>"K"],StandardForm]];
DefTensor[LKK1[],{M3,t},PrintAs->ToString[Subscript["L","KK+"],StandardForm]];
DefTensor[LKK2[],{M3,t},PrintAs->ToString[Subscript["L","KKx"],StandardForm]];

DefTensor[Lhd3S2[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","3"],StandardForm]<>"Sx"],StandardForm]];
DefTensor[Lhd3S1[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","3"],StandardForm]<>"S+"],StandardForm]];
DefTensor[Lhd3S3[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","3"],StandardForm]<>"So"],StandardForm]];
DefTensor[Ld3S[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]","3"],StandardForm]<>"S"],StandardForm]];
DefTensor[Lhd2Ndot1[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>ToString[OverDot[N],StandardForm]<>"+"],StandardForm]];
DefTensor[Lhd2Ndot2[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>ToString[OverDot[N],StandardForm]<>"x"],StandardForm]];
DefTensor[LdSR1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]SR+"],StandardForm]];
DefTensor[LdSR2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]SRx"],StandardForm]];
DefTensor[LRR1[],{M3,t},PrintAs->ToString[Subscript["L","RR+"],StandardForm]];
DefTensor[LRR2[],{M3,t},PrintAs->ToString[Subscript["L","RRx"],StandardForm]];
DefTensor[LKR1[],{M3,t},PrintAs->ToString[Subscript["L","KR+"],StandardForm]];
DefTensor[LKR2[],{M3,t},PrintAs->ToString[Subscript["L","KRx"],StandardForm]];
DefTensor[LNdotR[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>"R"],StandardForm]];
DefTensor[LKd2N1[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"N+"],StandardForm]];
DefTensor[LKd2N2[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"Nx"],StandardForm]];
DefTensor[LdSdotK1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[S],StandardForm]<>"K+"],StandardForm]];
DefTensor[LdSdotK2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[S],StandardForm]<>"Kx"],StandardForm]];
DefTensor[LNdotdSdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[N],StandardForm]<>"\[PartialD]"<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[LdNdotSdot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot[N],StandardForm]<>ToString[OverDot[S],StandardForm]],StandardForm]];
DefTensor[Ld3SN[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]","3"],StandardForm]<>"SN"],StandardForm]];
DefTensor[LdNd2S[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]N"<>ToString[Superscript["\[PartialD]","2"],StandardForm]<>"S"],StandardForm]];
DefTensor[Ld2NdS[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]","2"],StandardForm]<>"N\[PartialD]S"],StandardForm]];

DefTensor[Ld2deth[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Subscript["\[PartialD]","2"],StandardForm]<>ToString[Sqrt[h],StandardForm]],StandardForm]];

DefTensor[Lchichi[],{M3,t},PrintAs->ToString[Subscript["L","\[Chi]\[Chi]"],StandardForm]];
DefTensor[Lchih[],{M3,t},PrintAs->ToString[Subscript["L","\[Chi]h"],StandardForm]];
DefTensor[LchiN[],{M3,t},PrintAs->ToString[Subscript["L","\[Chi]N"],StandardForm]];
DefTensor[Lchidoth[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[\[Chi]],StandardForm]<>"h"],StandardForm]];

DefTensor[LchiK[],{M3,t},PrintAs->ToString[Subscript["L","\[Chi]K"],StandardForm]];
DefTensor[LchidS[],{M3,t},PrintAs->ToString[Subscript["L","\[Chi]\[PartialD]S"],StandardForm]];
DefTensor[LchidotN[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[\[Chi]],StandardForm]<>"N"],StandardForm]];

DefTensor[LchiR[],{M3,t},PrintAs->ToString[Subscript["L","\[Chi]R"],StandardForm]];
DefTensor[Ld2chih1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]h+"],StandardForm]];
DefTensor[Ld2chih2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]hx"],StandardForm]];

DefTensor[Lchidotchidot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[\[Chi]],StandardForm]<>ToString[OverDot[\[Chi]],StandardForm]],StandardForm]];
DefTensor[LKchidot[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[OverDot[\[Chi]],StandardForm]],StandardForm]];


DefTensor[Ldchidchi[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]\[Chi]\[PartialD]\[Chi]"],StandardForm]];
DefTensor[LdchidN[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]\[Chi]\[PartialD]N"],StandardForm]];

DefTensor[LchidotNdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[\[Chi]],StandardForm]<>ToString[OverDot["N"],StandardForm]],StandardForm]];
DefTensor[LchidotdS[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot[\[Chi]],StandardForm]<>"\[PartialD]S"],StandardForm]];



DefTensor[LRchidot[],{M3,t},PrintAs->ToString[Subscript["L","R"<>ToString[OverDot["\[Chi]"],StandardForm]],StandardForm]];

DefTensor[Ld2chidoth1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["\[Chi]"],StandardForm]<>"h+"],StandardForm]];


DefTensor[Ld2chidoth2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["\[Chi]"],StandardForm]<>"hx"],StandardForm]];


DefTensor[Ld2chidotN[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["\[Chi]"],StandardForm]<>"N"],StandardForm]];


DefTensor[LchidotdSdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[OverDot["\[Chi]"],StandardForm]<>"\[PartialD]"<>ToString[OverDot["S"],StandardForm]],StandardForm]];


DefTensor[Ld2chiK1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]K+"],StandardForm]];

DefTensor[Ld2chiK2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]Kx"],StandardForm]];

DefTensor[Ld2chidS[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]\[PartialD]S"],StandardForm]];

DefTensor[LRdSdot1[],{M3,t},PrintAs->ToString[Subscript["L","R\[PartialD]"<>ToString[OverDot["S"],StandardForm]<>"+"],StandardForm]];

DefTensor[LRdSdot2[],{M3,t},PrintAs->ToString[Subscript["L","R\[PartialD]"<>ToString[OverDot["S"],StandardForm]<>"x"],StandardForm]];

DefTensor[LdSdotdSdot1[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot["S"],StandardForm]<>"\[PartialD]"<>ToString[OverDot["S"],StandardForm]<>"+"],StandardForm]];

DefTensor[LdSdotdSdot2[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot["S"],StandardForm]<>"\[PartialD]"<>ToString[OverDot["S"],StandardForm]<>"x"],StandardForm]];

DefTensor[Ld2NdSdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"N\[PartialD]"<>ToString[OverDot["S"],StandardForm]],StandardForm]];

DefTensor[Lhd3Sdot1[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]",3],StandardForm]<>ToString[OverDot["S"],StandardForm]<>"+"],StandardForm]];

DefTensor[Lhd3Sdot2[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]",3],StandardForm]<>ToString[OverDot["S"],StandardForm]<>"x"],StandardForm]];

DefTensor[Lhd3Sdot3[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]",3],StandardForm]<>ToString[OverDot["S"],StandardForm]<>"o"],StandardForm]];

DefTensor[Ld2Nd2N[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"N"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>"N"],StandardForm]];

DefTensor[Ld2Sd2S1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"S"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>"S+"],StandardForm]];

DefTensor[Ld2Sd2S2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"S"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>"Sx"],StandardForm]];

DefTensor[Lhd4N1[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]",4],StandardForm]<>"N+"],StandardForm]];

DefTensor[Lhd4N2[],{M3,t},PrintAs->ToString[Subscript["L","h"<>ToString[Superscript["\[PartialD]",4],StandardForm]<>"Nx"],StandardForm]];

DefTensor[LKd3S1[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",3],StandardForm]<>"S+"],StandardForm]];

DefTensor[LKd3S2[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",3],StandardForm]<>"Sx"],StandardForm]];

DefTensor[LKd3S3[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",3],StandardForm]<>"So"],StandardForm]];

DefTensor[LKd2Ndot1[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["N"],StandardForm]<>"+"],StandardForm]];

DefTensor[LKd2Ndot2[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["N"],StandardForm]<>"x"],StandardForm]];

DefTensor[LdNdotdNdot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot["N"],StandardForm]<>"\[PartialD]"<>ToString[OverDot["N"],StandardForm]],StandardForm]];

DefTensor[Ld2chiR1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]R+"],StandardForm]];

DefTensor[Ld2chiR2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]Rx"],StandardForm]];

DefTensor[Ld4chih1[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",4],StandardForm]<>"\[Chi]h+"],StandardForm]];

DefTensor[Ld4chih2[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",4],StandardForm]<>"\[Chi]hx"],StandardForm]];

DefTensor[Ld2chid2chi[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]"],StandardForm]];

DefTensor[Ldchidotdchidot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot["\[Chi]"],StandardForm]<>"\[PartialD]"<>ToString[OverDot["\[Chi]"],StandardForm]],StandardForm]];

DefTensor[Ld2Nd2chi[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"N"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]"],StandardForm]];

DefTensor[Ld2chidSdot[],{M3,t},PrintAs->ToString[Subscript["L",ToString[Superscript["\[PartialD]",2],StandardForm]<>"\[Chi]\[PartialD]"<>ToString[OverDot["S"],StandardForm]],StandardForm]];

DefTensor[LdchidotdNdot[],{M3,t},PrintAs->ToString[Subscript["L","\[PartialD]"<>ToString[OverDot["\[Chi]"],StandardForm]<>"\[PartialD]"<>ToString[OverDot["N"],StandardForm]],StandardForm]];

DefTensor[LKd2chidot1[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["\[Chi]"],StandardForm]<>"+"],StandardForm]];

DefTensor[LKd2chidot2[],{M3,t},PrintAs->ToString[Subscript["L","K"<>ToString[Superscript["\[PartialD]",2],StandardForm]<>ToString[OverDot["\[Chi]"],StandardForm]<>"x"],StandardForm]];





L02orderList={LNN[],LRR[],LSS[],LNR[],LNS[],LRS[],LKK[],LRTRT[],LKRT[],Ldhdh[],LdhK[],LdhRT[],Lhh1[],Lhh2[],Lh[],Lbar[],LK[],LSSdot[],LNh[],LNdoth[],LN[],LdSh1[],LdSh2[],LdS[],LhK1[],LhK2[],LhR1[],LhR2[],LNK[],LR[],LNdS[],LdNS[],LNdot[],LNNdot[],LNdotdS[],LdNdotS[],LddotS[],LddotSh1[],LddotSh2[],LdotSdotS[],LdSdS1[],LdSdS2[],LdSddotS1[],LdSddotS2[],LSd2S1[],LSd2S2[],LdotSd2S1[],LdotSd2S2[],LdotNd2N[],LNd2Ndot[],LSd2Sdot1[],LSd2Sdot2[],LNdSdot[],LdSdot[],LdNSdot[],Ld2Ndot[],LdNdNdot[],Ld2N[],LdNdN[],LNd2N[],Lhd2N1[],Lhd2N2[],LdSK1[],LdSK2[],LNdotNdot[],LNdotK[],LKK1[],LKK2[],Lhd3S2[],Lhd3S1[],Lhd3S3[],Ld3S[],Lhd2Ndot1[],Lhd2Ndot2[],LdSR1[],LdSR2[],LRR1[],LRR2[],LKR1[],LKR2[],LNdotR[],LKd2N1[],LKd2N2[],LdSdotK1[],LdSdotK2[],LNdotdSdot[],
LdNdotSdot[],Ld3SN[],LdNd2S[],Ld2NdS[],Ld2deth[]};

L34orderList={Lchichi[],Lchih[],LchiN[],Lchidoth[],LchiK[],LchidS[],LchidotN[],LchiR[],Ld2chih1[],Ld2chih2[],Lchidotchidot[],LKchidot[],Ldchidchi[],LdchidN[],LchidotNdot[],LchidotdS[],LRchidot[],Ld2chidoth1[],Ld2chidoth2[],Ld2chidotN[],LchidotdSdot[],Ld2chiK1[],Ld2chiK2[],Ld2chidS[],Lhd3S1[],Lhd3S2[],Lhd3S3[],Lhd2Ndot1[],Lhd2Ndot2[],LdSR1[],LdSR2[],LKR1[],LKR2[],LNdotR[],LKd2N1[],LKd2N2[],LdSdotK1[],LdSdotK2[],LNdotdSdot[],Ld2NdS[],LRdSdot1[],LRdSdot2[],LdSdotdSdot1[],LdSdotdSdot2[],Ld2NdSdot[],Lhd3Sdot1[],Lhd3Sdot2[],Lhd3Sdot3[],Ld2Nd2N[],Ld2Sd2S1[],Ld2Sd2S2[],Lhd4N1[],Lhd4N2[],LKd3S1[],LKd3S2[],LKd3S3[],LKd2Ndot1[],LKd2Ndot2[],LdNdotdNdot[],Ld2chiR1[],Ld2chiR2[],Ld4chih1[],Ld4chih2[],Ld2chid2chi[],Ldchidotdchidot[],Ld2Nd2chi[],Ld2chidSdot[],LdchidotdNdot[],LKd2chidot1[],LKd2chidot2[]};


(* ::Subsection::Closed:: *)
(*Rules for L coefficients:*)


LNNrule=MakeRule[{PD[b]@LNN[],0},MetricOn->All,ContractMetrics->True];
LRRrule=MakeRule[{PD[b]@LRR[],0},MetricOn->All,ContractMetrics->True];
LSSrule=MakeRule[{PD[b]@LSS[],0},MetricOn->All,ContractMetrics->True];
LNRrule=MakeRule[{PD[b]@LNR[],0},MetricOn->All,ContractMetrics->True];
LNSrule=MakeRule[{PD[b]@LNS[],0},MetricOn->All,ContractMetrics->True];
LRSrule=MakeRule[{PD[b]@LRS[],0},MetricOn->All,ContractMetrics->True];

LKKrule=MakeRule[{PD[b]@LKK[],0},MetricOn->All,ContractMetrics->True];
LRTRTrule=MakeRule[{PD[b]@LRTRT[],0},MetricOn->All,ContractMetrics->True];
LKRTrule=MakeRule[{PD[b]@LKRT[],0},MetricOn->All,ContractMetrics->True];
Lhhrule=MakeRule[{PD[b]@Ldhdh[],0},MetricOn->All,ContractMetrics->True];
LhKrule=MakeRule[{PD[b]@LdhK[],0},MetricOn->All,ContractMetrics->True];
LhRrule=MakeRule[{PD[b]@LdhRT[],0},MetricOn->All,ContractMetrics->True];

Lhh1rule=MakeRule[{PD[b]@Lhh1[],0},MetricOn->All,ContractMetrics->True];
Lhh2rule=MakeRule[{PD[b]@Lhh2[],0},MetricOn->All,ContractMetrics->True];
Lhrule=MakeRule[{PD[b]@Lh[],0},MetricOn->All,ContractMetrics->True];
Lbarrule=MakeRule[{PD[b]@Lbar[],0},MetricOn->All,ContractMetrics->True];
LKrule=MakeRule[{PD[b]@LK[],0},MetricOn->All,ContractMetrics->True];
LSSdotrule=MakeRule[{PD[b]@LSSdot[],0},MetricOn->All,ContractMetrics->True];
LNhrule=MakeRule[{PD[b]@LNh[],0},MetricOn->All,ContractMetrics->True];
LNdothrule=MakeRule[{PD[b]@LNdoth[],0},MetricOn->All,ContractMetrics->True];
LNrule=MakeRule[{PD[b]@LN[],0},MetricOn->All,ContractMetrics->True];
LdSh1rule=MakeRule[{PD[b]@LdSh1[],0},MetricOn->All,ContractMetrics->True];
LdSh2rule=MakeRule[{PD[b]@LdSh2[],0},MetricOn->All,ContractMetrics->True];
LdSrule=MakeRule[{PD[b]@LdS[],0},MetricOn->All,ContractMetrics->True];
LhK1rule=MakeRule[{PD[b]@LhK1[],0},MetricOn->All,ContractMetrics->True];
LhK2rule=MakeRule[{PD[b]@LhK2[],0},MetricOn->All,ContractMetrics->True];
LhR1rule=MakeRule[{PD[b]@LhR1[],0},MetricOn->All,ContractMetrics->True];
LhR2rule=MakeRule[{PD[b]@LhR2[],0},MetricOn->All,ContractMetrics->True];
LNKrule=MakeRule[{PD[b]@LNK[],0},MetricOn->All,ContractMetrics->True];
LRrule=MakeRule[{PD[b]@LR[],0},MetricOn->All,ContractMetrics->True];
LNdSrule=MakeRule[{PD[b]@LNdS[],0},MetricOn->All,ContractMetrics->True];
LdNSrule=MakeRule[{PD[b]@LdNS[],0},MetricOn->All,ContractMetrics->True];
LNdotrule=MakeRule[{PD[b]@LNdot[],0},MetricOn->All,ContractMetrics->True];
LNNdotrule=MakeRule[{PD[b]@LNNdot[],0},MetricOn->All,ContractMetrics->True];
LNdotdSrule=MakeRule[{PD[b]@LNdotdS[],0},MetricOn->All,ContractMetrics->True];
LdNdotSrule=MakeRule[{PD[b]@LdNdotS[],0},MetricOn->All,ContractMetrics->True];
LddotSrule=MakeRule[{PD[b]@LddotS[],0},MetricOn->All,ContractMetrics->True];
LddotSh1rule=MakeRule[{PD[b]@LddotSh1[],0},MetricOn->All,ContractMetrics->True];
LddotSh2rule=MakeRule[{PD[b]@LddotSh2[],0},MetricOn->All,ContractMetrics->True];
LdotSdotSrule=MakeRule[{PD[b]@LdotSdotS[],0},MetricOn->All,ContractMetrics->True];
LdSdS1rule=MakeRule[{PD[b]@LdSdS1[],0},MetricOn->All,ContractMetrics->True];
LdSdS2rule=MakeRule[{PD[b]@LdSdS2[],0},MetricOn->All,ContractMetrics->True];
LdSddotS1rule=MakeRule[{PD[b]@LdSddotS1[],0},MetricOn->All,ContractMetrics->True];
LdSddotS2rule=MakeRule[{PD[b]@LdSddotS2[],0},MetricOn->All,ContractMetrics->True];
LSd2S1rule=MakeRule[{PD[b]@LSd2S1[],0},MetricOn->All,ContractMetrics->True];
LSd2S2rule=MakeRule[{PD[b]@LSd2S2[],0},MetricOn->All,ContractMetrics->True];
LdotSd2S1rule=MakeRule[{PD[b]@LdotSd2S1[],0},MetricOn->All,ContractMetrics->True];
LdotSd2S2rule=MakeRule[{PD[b]@LdotSd2S2[],0},MetricOn->All,ContractMetrics->True];
LdotNd2Nrule=MakeRule[{PD[b]@LdotNd2N[],0},MetricOn->All,ContractMetrics->True];
LNd2Ndotrule=MakeRule[{PD[b]@LNd2Ndot[],0},MetricOn->All,ContractMetrics->True];
LSd2Sdot1rule=MakeRule[{PD[b]@LSd2Sdot1[],0},MetricOn->All,ContractMetrics->True];
LSd2Sdot2rule=MakeRule[{PD[b]@LSd2Sdot2[],0},MetricOn->All,ContractMetrics->True];
LNdSdotrule=MakeRule[{PD[b]@LNdSdot[],0},MetricOn->All,ContractMetrics->True];
LdSdotrule=MakeRule[{PD[b]@LdSdot[],0},MetricOn->All,ContractMetrics->True];
LdNSdotrule=MakeRule[{PD[b]@LdNSdot[],0},MetricOn->All,ContractMetrics->True];
Ld2Ndotrule=MakeRule[{PD[b]@Ld2Ndot[],0},MetricOn->All,ContractMetrics->True];
LdNdNdotrule=MakeRule[{PD[b]@LdNdNdot[],0},MetricOn->All,ContractMetrics->True];
Ld2Nrule=MakeRule[{PD[b]@Ld2N[],0},MetricOn->All,ContractMetrics->True];
LdNdNrule=MakeRule[{PD[b]@LdNdN[],0},MetricOn->All,ContractMetrics->True];
LNd2Nrule=MakeRule[{PD[b]@LNd2N[],0},MetricOn->All,ContractMetrics->True];
Lhd2N1rule=MakeRule[{PD[b]@Lhd2N1[],0},MetricOn->All,ContractMetrics->True];
Lhd2N2rule=MakeRule[{PD[b]@Lhd2N2[],0},MetricOn->All,ContractMetrics->True];
LdSK1rule=MakeRule[{PD[b]@LdSK1[],0},MetricOn->All,ContractMetrics->True];
LdSK2rule=MakeRule[{PD[b]@LdSK2[],0},MetricOn->All,ContractMetrics->True];
LNdotNdotrule=MakeRule[{PD[b]@LNdotNdot[],0},MetricOn->All,ContractMetrics->True];
LNdotKrule=MakeRule[{PD[b]@LNdotK[],0},MetricOn->All,ContractMetrics->True];
LKK1rule=MakeRule[{PD[b]@LKK1[],0},MetricOn->All,ContractMetrics->True];
LKK2rule=MakeRule[{PD[b]@LKK2[],0},MetricOn->All,ContractMetrics->True];

Lhd3S2rule=MakeRule[{PD[b]@Lhd3S2[],0},MetricOn->All,ContractMetrics->True];
Lhd3S1rule=MakeRule[{PD[b]@Lhd3S1[],0},MetricOn->All,ContractMetrics->True];
Lhd3S3rule=MakeRule[{PD[b]@Lhd3S3[],0},MetricOn->All,ContractMetrics->True];
Ld3Srule=MakeRule[{PD[b]@Ld3S[],0},MetricOn->All,ContractMetrics->True];
Lhd2Ndot1rule=MakeRule[{PD[b]@Lhd2Ndot1[],0},MetricOn->All,ContractMetrics->True];
Lhd2Ndot2rule=MakeRule[{PD[b]@Lhd2Ndot2[],0},MetricOn->All,ContractMetrics->True];
LdSR1rule=MakeRule[{PD[b]@LdSR1[],0},MetricOn->All,ContractMetrics->True];
LdSR2rule=MakeRule[{PD[b]@LdSR2[],0},MetricOn->All,ContractMetrics->True];
LRR1rule=MakeRule[{PD[b]@LRR1[],0},MetricOn->All,ContractMetrics->True];
LRR2rule=MakeRule[{PD[b]@LRR2[],0},MetricOn->All,ContractMetrics->True];
LKR1rule=MakeRule[{PD[b]@LKR1[],0},MetricOn->All,ContractMetrics->True];
LKR2rule=MakeRule[{PD[b]@LKR2[],0},MetricOn->All,ContractMetrics->True];
LNdotRrule=MakeRule[{PD[b]@LNdotR[],0},MetricOn->All,ContractMetrics->True];
LKd2N1rule=MakeRule[{PD[b]@LKd2N1[],0},MetricOn->All,ContractMetrics->True];
LKd2N2rule=MakeRule[{PD[b]@LKd2N2[],0},MetricOn->All,ContractMetrics->True];
LdSdotK1rule=MakeRule[{PD[b]@LdSdotK1[],0},MetricOn->All,ContractMetrics->True];
LdSdotK2rule=MakeRule[{PD[b]@LdSdotK2[],0},MetricOn->All,ContractMetrics->True];
LNdotdSdotrule=MakeRule[{PD[b]@LNdotdSdot[],0},MetricOn->All,ContractMetrics->True];
LdNdotSdotrule=MakeRule[{PD[b]@LdNdotSdot[],0},MetricOn->All,ContractMetrics->True];
Ld3SNrule=MakeRule[{PD[b]@Ld3SN[],0},MetricOn->All,ContractMetrics->True];
LdNd2Srule=MakeRule[{PD[b]@LdNd2S[],0},MetricOn->All,ContractMetrics->True];
Ld2NdSrule=MakeRule[{PD[b]@Ld2NdS[],0},MetricOn->All,ContractMetrics->True];
Ld2dethrule=MakeRule[{PD[b]@Ld2deth[],0},MetricOn->All,ContractMetrics->True];

Lchichirule=MakeRule[{PD[b]@Lchichi[],0},MetricOn->All,ContractMetrics->True];
Lchihrule=MakeRule[{PD[b]@Lchih[],0},MetricOn->All,ContractMetrics->True];
LchiNrule=MakeRule[{PD[b]@LchiN[],0},MetricOn->All,ContractMetrics->True];
Lchidothrule=MakeRule[{PD[b]@Lchidoth[],0},MetricOn->All,ContractMetrics->True];
LchiKrule=MakeRule[{PD[b]@LchiK[],0},MetricOn->All,ContractMetrics->True];
LchidSrule=MakeRule[{PD[b]@LchidS[],0},MetricOn->All,ContractMetrics->True];
LchidotNrule=MakeRule[{PD[b]@LchidotN[],0},MetricOn->All,ContractMetrics->True];
LchiRrule=MakeRule[{PD[b]@LchiR[],0},MetricOn->All,ContractMetrics->True];
Ld2chih1rule=MakeRule[{PD[b]@Ld2chih1[],0},MetricOn->All,ContractMetrics->True];
Ld2chih2rule=MakeRule[{PD[b]@Ld2chih2[],0},MetricOn->All,ContractMetrics->True];
Lchidotchidotrule=MakeRule[{PD[b]@Lchidotchidot[],0},MetricOn->All,ContractMetrics->True];
LKchidotrule=MakeRule[{PD[b]@LKchidot[],0},MetricOn->All,ContractMetrics->True];
Ldchidchirule=MakeRule[{PD[b]@Ldchidchi[],0},MetricOn->All,ContractMetrics->True];
LdchiNrule=MakeRule[{PD[b]@LdchidN[],0},MetricOn->All,ContractMetrics->True];
LchidotNdotrule=MakeRule[{PD[b]@LchidotNdot[],0},MetricOn->All,ContractMetrics->True];
LchidotdSrule=MakeRule[{PD[b]@LchidotdS[],0},MetricOn->All,ContractMetrics->True];
LRchidotrule=MakeRule[{PD[b]@LRchidot[],0},MetricOn->All,ContractMetrics->True];
Ld2chidoth1rule=MakeRule[{PD[b]@Ld2chidoth1[],0},MetricOn->All,ContractMetrics->True];
Ld2chidoth2rule=MakeRule[{PD[b]@Ld2chidoth2[],0},MetricOn->All,ContractMetrics->True];
Ld2chidotNrule=MakeRule[{PD[b]@Ld2chidotN[],0},MetricOn->All,ContractMetrics->True];
LchidotdSdotrule=MakeRule[{PD[b]@LchidotdSdot[],0},MetricOn->All,ContractMetrics->True];
Ld2chiK1rule=MakeRule[{PD[b]@Ld2chiK1[],0},MetricOn->All,ContractMetrics->True];
Ld2chiK2rule=MakeRule[{PD[b]@Ld2chiK2[],0},MetricOn->All,ContractMetrics->True];
Ld2chidSrule=MakeRule[{PD[b]@Ld2chidS[],0},MetricOn->All,ContractMetrics->True];
Lhd3S1rule=MakeRule[{PD[b]@Lhd3S1[],0},MetricOn->All,ContractMetrics->True];
Lhd3S2rule=MakeRule[{PD[b]@Lhd3S2[],0},MetricOn->All,ContractMetrics->True];
Lhd3S3rule=MakeRule[{PD[b]@Lhd3S3[],0},MetricOn->All,ContractMetrics->True];
Lhd2Ndot1rule=MakeRule[{PD[b]@Lhd2Ndot1[],0},MetricOn->All,ContractMetrics->True];
Lhd2Ndot2rule=MakeRule[{PD[b]@Lhd2Ndot2[],0},MetricOn->All,ContractMetrics->True];
LdSR1rule=MakeRule[{PD[b]@LdSR1[],0},MetricOn->All,ContractMetrics->True];
LdSR2rule=MakeRule[{PD[b]@LdSR2[],0},MetricOn->All,ContractMetrics->True];
LKR1rule=MakeRule[{PD[b]@LKR1[],0},MetricOn->All,ContractMetrics->True];
LKR2rule=MakeRule[{PD[b]@LKR2[],0},MetricOn->All,ContractMetrics->True];
LNdotRrule=MakeRule[{PD[b]@LNdotR[],0},MetricOn->All,ContractMetrics->True];
LKd2N1rule=MakeRule[{PD[b]@LKd2N1[],0},MetricOn->All,ContractMetrics->True];
LKd2N2rule=MakeRule[{PD[b]@LKd2N2[],0},MetricOn->All,ContractMetrics->True];
LdSdotK1rule=MakeRule[{PD[b]@LdSdotK1[],0},MetricOn->All,ContractMetrics->True];
LdSdotK2rule=MakeRule[{PD[b]@LdSdotK2[],0},MetricOn->All,ContractMetrics->True];
LNdotdSdotrule=MakeRule[{PD[b]@LNdotdSdot[],0},MetricOn->All,ContractMetrics->True];
Ld2NdSrule=MakeRule[{PD[b]@Ld2NdS[],0},MetricOn->All,ContractMetrics->True];
LRdSdot1rule=MakeRule[{PD[b]@LRdSdot1[],0},MetricOn->All,ContractMetrics->True];
LRdSdot2rule=MakeRule[{PD[b]@LRdSdot2[],0},MetricOn->All,ContractMetrics->True];
LdSdotdSdot1rule=MakeRule[{PD[b]@LdSdotdSdot1[],0},MetricOn->All,ContractMetrics->True];
LdSdotdSdot2rule=MakeRule[{PD[b]@LdSdotdSdot2[],0},MetricOn->All,ContractMetrics->True];
Ld2NdSdotrule=MakeRule[{PD[b]@Ld2NdSdot[],0},MetricOn->All,ContractMetrics->True];
Lhd3Sdot1rule=MakeRule[{PD[b]@Lhd3Sdot1[],0},MetricOn->All,ContractMetrics->True];
Lhd3Sdot2rule=MakeRule[{PD[b]@Lhd3Sdot2[],0},MetricOn->All,ContractMetrics->True];
Lhd3Sdot3rule=MakeRule[{PD[b]@Lhd3Sdot3[],0},MetricOn->All,ContractMetrics->True];
Ld2Nd2Nrule=MakeRule[{PD[b]@Ld2Nd2N[],0},MetricOn->All,ContractMetrics->True];
Ld2Sd2S1rule=MakeRule[{PD[b]@Ld2Sd2S1[],0},MetricOn->All,ContractMetrics->True];
Ld2Sd2S2rule=MakeRule[{PD[b]@Ld2Sd2S2[],0},MetricOn->All,ContractMetrics->True];
Lhd4N1rule=MakeRule[{PD[b]@Lhd4N1[],0},MetricOn->All,ContractMetrics->True];
Lhd4N2rule=MakeRule[{PD[b]@Lhd4N2[],0},MetricOn->All,ContractMetrics->True];
LKd3S1rule=MakeRule[{PD[b]@LKd3S1[],0},MetricOn->All,ContractMetrics->True];
LKd3S2rule=MakeRule[{PD[b]@LKd3S2[],0},MetricOn->All,ContractMetrics->True];
LKd3S3rule=MakeRule[{PD[b]@LKd3S3[],0},MetricOn->All,ContractMetrics->True];
LKd2Ndot1rule=MakeRule[{PD[b]@LKd2Ndot1[],0},MetricOn->All,ContractMetrics->True];
LKd2Ndot2rule=MakeRule[{PD[b]@LKd2Ndot2[],0},MetricOn->All,ContractMetrics->True];
LdNdotdNdotrule=MakeRule[{PD[b]@LdNdotdNdot[],0},MetricOn->All,ContractMetrics->True];
Ld2chiR1rule=MakeRule[{PD[b]@Ld2chiR1[],0},MetricOn->All,ContractMetrics->True];
Ld2chiR2rule=MakeRule[{PD[b]@Ld2chiR2[],0},MetricOn->All,ContractMetrics->True];
Ld4chih1rule=MakeRule[{PD[b]@Ld4chih1[],0},MetricOn->All,ContractMetrics->True];
Ld4chih2rule=MakeRule[{PD[b]@Ld4chih2[],0},MetricOn->All,ContractMetrics->True];
Ld2chid2chirule=MakeRule[{PD[b]@Ld2chid2chi[],0},MetricOn->All,ContractMetrics->True];
Ldchidotdchidotrule=MakeRule[{PD[b]@Ldchidotdchidot[],0},MetricOn->All,ContractMetrics->True];
Ld2Nd2chirule=MakeRule[{PD[b]@Ld2Nd2chi[],0},MetricOn->All,ContractMetrics->True];
Ld2chidSdotrule=MakeRule[{PD[b]@Ld2chidSdot[],0},MetricOn->All,ContractMetrics->True];
LdchidotdNdotrule=MakeRule[{PD[b]@LdchidotdNdot[],0},MetricOn->All,ContractMetrics->True];
LKd2chidot1rule=MakeRule[{PD[b]@LKd2chidot1[],0},MetricOn->All,ContractMetrics->True];
LKd2chidot2rule=MakeRule[{PD[b]@LKd2chidot2[],0},MetricOn->All,ContractMetrics->True];

arule=MakeRule[{PD[b]@a[],0},MetricOn->All,ContractMetrics->True];

Hrule=MakeRule[{ParamD[t]@a[],a[]H[]},MetricOn->All,ContractMetrics->True];
Hinvrule=MakeRule[{H[],ParamD[t]@a[]/a[]},MetricOn->All,ContractMetrics->True];

AutomaticRules[LNN,LNNrule];
AutomaticRules[LRR,LRRrule];
AutomaticRules[LSS,LSSrule];
AutomaticRules[LNR,LNRrule];
AutomaticRules[LNS,LNSrule];
AutomaticRules[LRS,LRSrule];
AutomaticRules[LKK,LKKrule];
AutomaticRules[LRTRT,LRTRTrule];
AutomaticRules[LKRT,LKRTrule];
AutomaticRules[Ldhdh,Lhhrule];
{
 {AutomaticRules[LdhK,LhKrule];},
 {AutomaticRules[LdhRT,LhRrule];}
}

AutomaticRules[Lhh1,Lhh1rule];
AutomaticRules[Lhh2,Lhh2rule];
AutomaticRules[Lh,Lhrule];
AutomaticRules[Lbar,Lbarrule];
AutomaticRules[LK,LKrule];
AutomaticRules[LSSdot,LSSdotrule];
AutomaticRules[LNh,LNhrule];
AutomaticRules[LNdoth,LNdothrule];
AutomaticRules[LN,LNrule];
AutomaticRules[LdSh1,LdSh1rule];
AutomaticRules[LdSh2,LdSh2rule];
AutomaticRules[LdS,LdSrule];
AutomaticRules[LhK1,LhK1rule];
AutomaticRules[LhK2,LhK2rule];
AutomaticRules[LhR1,LhR1rule];
AutomaticRules[LhR2,LhR2rule];
AutomaticRules[LNK,LNKrule];
AutomaticRules[LR,LRrule];
AutomaticRules[LNdS,LNdSrule];
AutomaticRules[LdNS,LdNSrule];
AutomaticRules[LNdot,LNdotrule];
AutomaticRules[LNNdot,LNNdotrule];
AutomaticRules[LNdotdS,LNdotdSrule];
AutomaticRules[LdNdotS,LdNdotSrule];
AutomaticRules[LddotS,LddotSrule];
AutomaticRules[LddotSh1,LddotSh1rule];
AutomaticRules[LddotSh2,LddotSh2rule];
AutomaticRules[LdotSdotS,LdotSdotSrule];
AutomaticRules[LdSdS1,LdSdS1rule];
AutomaticRules[LdSdS2,LdSdS2rule];
AutomaticRules[LdSddotS1,LdSddotS1rule];
AutomaticRules[LdSddotS2,LdSddotS2rule];
AutomaticRules[LSd2S1,LSd2S1rule];
AutomaticRules[LSd2S2,LSd2S2rule];
AutomaticRules[LdotSd2S1,LdotSd2S1rule];
AutomaticRules[LdotSd2S2,LdotSd2S2rule];
AutomaticRules[LdotNd2N,LdotNd2Nrule];
AutomaticRules[LNd2Ndot,LNd2Ndotrule];
AutomaticRules[LSd2Sdot1,LSd2Sdot1rule];
AutomaticRules[LSd2Sdot2,LSd2Sdot2rule];
AutomaticRules[LNdSdot,LNdSdotrule];
AutomaticRules[LdSdot,LdSdotrule];
AutomaticRules[LdNSdot,LdNSdotrule];
AutomaticRules[Ld2Ndot,Ld2Ndotrule];
AutomaticRules[LdNdNdot,LdNdNdotrule];
AutomaticRules[Ld2N,Ld2Nrule];
AutomaticRules[LdNdN,LdNdNrule];
AutomaticRules[LNd2N,LNd2Nrule];
AutomaticRules[Lhd2N1,Lhd2N1rule];
AutomaticRules[Lhd2N2,Lhd2N2rule];
AutomaticRules[LdSK1,LdSK1rule];
AutomaticRules[LdSK2,LdSK2rule];
AutomaticRules[LNdotNdot,LNdotNdotrule];
AutomaticRules[LNdotK,LNdotKrule];
AutomaticRules[LKK1,LKK1rule];
AutomaticRules[LKK2,LKK2rule];

AutomaticRules[Lhd3S2,Lhd3S2rule];
AutomaticRules[Lhd3S1,Lhd3S1rule];
AutomaticRules[Lhd3S3,Lhd3S3rule];
AutomaticRules[Ld3S,Ld3Srule];
AutomaticRules[Lhd2Ndot1,Lhd2Ndot1rule];
AutomaticRules[Lhd2Ndot2,Lhd2Ndot2rule];
AutomaticRules[LdSR1,LdSR1rule];
AutomaticRules[LdSR2,LdSR2rule];
AutomaticRules[LRR1,LRR1rule];
AutomaticRules[LRR2,LRR2rule];
AutomaticRules[LKR1,LKR1rule];
AutomaticRules[LKR2,LKR2rule];
AutomaticRules[LNdotR,LNdotRrule];
AutomaticRules[LKd2N1,LKd2N1rule];
AutomaticRules[LKd2N2,LKd2N2rule];
AutomaticRules[LdSdotK1,LdSdotK1rule];
AutomaticRules[LdSdotK2,LdSdotK2rule];
AutomaticRules[LNdotdSdot,LNdotdSdotrule];
AutomaticRules[LdNdotSdot,LdNdotSdotrule];
AutomaticRules[Ld3SN,Ld3SNrule];
AutomaticRules[LdNd2S,LdNd2Srule];
AutomaticRules[Ld2NdS,Ld2NdSrule];
AutomaticRules[Ld2deth,Ld2dethrule];


AutomaticRules[Lchichi,Lchichirule];
AutomaticRules[Lchih,Lchihrule];
AutomaticRules[LchiN,LchiNrule];
AutomaticRules[Lchidoth,Lchidothrule];
AutomaticRules[LchiK,LchiKrule];
AutomaticRules[LchidS,LchidSrule];
AutomaticRules[LchidotN,LchidotNrule];
AutomaticRules[LchiR,LchiRrule];
AutomaticRules[Ld2chih1,Ld2chih1rule];
AutomaticRules[Ld2chih2,Ld2chih2rule];
AutomaticRules[Lchidotchidot,Lchidotchidotrule];
AutomaticRules[LKchidot,LKchidotrule];
AutomaticRules[Ldchidchi,Ldchidchirule];
AutomaticRules[LdchidN,LdchiNrule];
AutomaticRules[LchidotNdot,LchidotNdotrule];
AutomaticRules[LchidotdS,LchidotdSrule];
AutomaticRules[LRchidot,LRchidotrule];
AutomaticRules[Ld2chidoth1,Ld2chidoth1rule];
AutomaticRules[Ld2chidoth2,Ld2chidoth2rule];
AutomaticRules[Ld2chidotN,Ld2chidotNrule];
AutomaticRules[LchidotdSdot,LchidotdSdotrule];
AutomaticRules[Ld2chiK1,Ld2chiK1rule];
AutomaticRules[Ld2chiK2,Ld2chiK2rule];
AutomaticRules[Ld2chidS,Ld2chidSrule];
AutomaticRules[Lhd3S1,Lhd3S1rule];
AutomaticRules[Lhd3S2,Lhd3S2rule];
AutomaticRules[Lhd3S3,Lhd3S3rule];
AutomaticRules[Lhd2Ndot1,Lhd2Ndot1rule];
AutomaticRules[Lhd2Ndot2,Lhd2Ndot2rule];
AutomaticRules[LdSR1,LdSR1rule];
AutomaticRules[LdSR2,LdSR2rule];
AutomaticRules[LKR1,LKR1rule];
AutomaticRules[LKR2,LKR2rule];
AutomaticRules[LNdotR,LNdotRrule];
AutomaticRules[LKd2N1,LKd2N1rule];
AutomaticRules[LKd2N2,LKd2N2rule];
AutomaticRules[LdSdotK1,LdSdotK1rule];
AutomaticRules[LdSdotK2,LdSdotK2rule];
AutomaticRules[LNdotdSdot,LNdotdSdotrule];
AutomaticRules[Ld2NdS,Ld2NdSrule];
AutomaticRules[LRdSdot1,LRdSdot1rule];
AutomaticRules[LRdSdot2,LRdSdot2rule];
AutomaticRules[LdSdotdSdot1,LdSdotdSdot1rule];
AutomaticRules[LdSdotdSdot2,LdSdotdSdot2rule];
AutomaticRules[Ld2NdSdot,Ld2NdSdotrule];
AutomaticRules[Lhd3Sdot1,Lhd3Sdot1rule];
AutomaticRules[Lhd3Sdot2,Lhd3Sdot2rule];
AutomaticRules[Lhd3Sdot3,Lhd3Sdot3rule];
AutomaticRules[Ld2Nd2N,Ld2Nd2Nrule];
AutomaticRules[Ld2Sd2S1,Ld2Sd2S1rule];
AutomaticRules[Ld2Sd2S2,Ld2Sd2S2rule];
AutomaticRules[Lhd4N1,Lhd4N1rule];
AutomaticRules[Lhd4N2,Lhd4N2rule];
AutomaticRules[LKd3S1,LKd3S1rule];
AutomaticRules[LKd3S2,LKd3S2rule];
AutomaticRules[LKd3S3,LKd3S3rule];
AutomaticRules[LKd2Ndot1,LKd2Ndot1rule];
AutomaticRules[LKd2Ndot2,LKd2Ndot2rule];
AutomaticRules[LdNdotdNdot,LdNdotdNdotrule];
AutomaticRules[Ld2chiR1,Ld2chiR1rule];
AutomaticRules[Ld2chiR2,Ld2chiR2rule];
AutomaticRules[Ld4chih1,Ld4chih1rule];
AutomaticRules[Ld4chih2,Ld4chih2rule];
AutomaticRules[Ld2chid2chi,Ld2chid2chirule];
AutomaticRules[Ldchidotdchidot,Ldchidotdchidotrule];
AutomaticRules[Ld2Nd2chi,Ld2Nd2chirule];
AutomaticRules[Ld2chidSdot,Ld2chidSdotrule];
AutomaticRules[LdchidotdNdot,LdchidotdNdotrule];
AutomaticRules[LKd2chidot1,LKd2chidot1rule];
AutomaticRules[LKd2chidot2,LKd2chidot2rule];




(* ::Subsection::Closed:: *)
(*Potential and matter background pieces:*)


DefTensor[Phi0[],{M3,t},PrintAs->ToString[Subscript["\[Phi]","0"],StandardForm]];
DefTensor[ScalarPhi[],{M3,t},PrintAs->"\[Delta]\[CurlyPhi]"];

DefTensor[Chi0[],{M3,t},PrintAs->ToString[Subscript["\[Chi]","0"],StandardForm]];
DefTensor[ScalarChi[],{M3,t},PrintAs->"\[Delta]\[Chi]"];

DefTensor[V0[],{M3,t},PrintAs->ToString[Subscript["V","0"],StandardForm]];
DefTensor[V0p[],{M3,t},PrintAs->ToString[Subsuperscript["V","0","'"],StandardForm]];
DefTensor[V0pp[],{M3,t},PrintAs->ToString[Subsuperscript["V","0","''"],StandardForm]];

Phi0rule=MakeRule[{PD[b]@Phi0[],0},MetricOn->All,ContractMetrics->True];
AutomaticRules[Phi0,Phi0rule];

Chi0rule=MakeRule[{PD[b]@Chi0[],0},MetricOn->All,ContractMetrics->True];
AutomaticRules[Chi0,Chi0rule];

V0rule=MakeRule[{PD[b]@V0[],0},MetricOn->All,ContractMetrics->True];
AutomaticRules[V0,V0rule];


V0prule=MakeRule[{PD[b]@V0p[],0},MetricOn->All,ContractMetrics->True];
AutomaticRules[V0p,V0prule];


V0pprule=MakeRule[{PD[b]@V0pp[],0},MetricOn->All,ContractMetrics->True];
AutomaticRules[V0pp,V0pprule];


AutomaticRules[a,arule];


(* ::Section:: *)
(*Perturbations, Gauge fields and associated rules:*)


(* ::Subsection::Closed:: *)
(*Perturbations fields:*)


DefTensor[Phi[],{M3,t},PrintAs->"\[CapitalPhi]"]
DefTensor[B[],{M3,t},PrintAs->"B"]
DefTensor[Psi[],{M3,t},PrintAs->"\[CapitalPsi]"]
DefTensor[EE[],{M3,t},PrintAs->"E"]


(* ::Subsection::Closed:: *)
(*Gauge fields:*)


DefTensor[pi[],{M3,t},PrintAs->"\[Pi]"];
orderpi=MakeRule[{pi[],order pi[]}];
DefTensor[eps[],{M3,t},PrintAs->"\[Epsilon]"];
ordereps=MakeRule[{eps[],order eps[]}];


(* ::Subsection::Closed:: *)
(*Perturbation rules:*)


lapserule=MakeRule[{Dlapse[],Phi[]}];

shiftrule=MakeRule[{Dshift[b],1/(a[]^2)PD[b]@B[]},MetricOn->All,ContractMetrics->True];

hrule=MakeRule[{deltah[-c,-b],a[]^2(-2Psi[]metricg[-c,-b]+2 PD[-c]@PD[-b]@EE[])},MetricOn->All,ContractMetrics->True];

hTrrule=MakeRule[{deltahTr[],(-2Psi[]metricg[-x,x]+2 PD[-x]@PD[x]@EE[])},MetricOn->All,ContractMetrics->True];

lapse2rule=MakeRule[{D2lapse[],(-1/2Phi[]^2+1/(2a[]^2)PD[-c]@B[]PD[c]@B[])},MetricOn->All,ContractMetrics->True];

dethrule=MakeRule[{ddeth[],a[]^3(PD[b]@PD[-b]@EE[]-3Psi[])},MetricOn->All,ContractMetrics->True];

d2dethrule=MakeRule[{d2deth[],a[]^3(3/2Psi[]^2-1/2PD[-b]@PD[b]@EE[]PD[-c]@PD[c]@EE[]-Psi[]PD[-c]@PD[c]@EE[])},MetricOn->All,ContractMetrics->True];

Krule=MakeRule[{DExK[c,-b],-(ParamD[t]@Psi[]+H[]Phi[])metricg[c,-b]+PD[c]@PD[-b]@ParamD[t]@EE[]-(1/a[]^2)PD[c]@PD[-b]@B[]},MetricOn->All,ContractMetrics->True];

riccirule=MakeRule[{DRicci[c,-b], 1/(a[]^2)(PD[c]@PD[-b]@Psi[]+PD[d]@PD[-d]@Psi[]metricg[c,-b])},MetricOn->All,ContractMetrics->True];

D2Riccirule=MakeRule[{D2Ricci[],(2/(a[]^2))(-PD[-b]@Psi[]PD[b]@Psi[]+4Psi[]PD[-b]@PD[b]@Psi[])-(4/(a[]^2))PD[b]@PD[-b]@Psi[]PD[c]@PD[-c]@EE[]},MetricOn->All,ContractMetrics->True];

pertRules[x_]:=(x/.lapserule/.lapse2rule/.shiftrule/.hrule/.hTrrule/.dethrule/.d2dethrule/.Krule/.riccirule/.D2Riccirule)//ToCanonical//CollectTensors;


(* ::Subsection::Closed:: *)
(*Gauge transformation rules:*)


FPhirule=MakeRule[{Phi[],Phi[]-ParamD[t]@pi[]}];
FBrule=MakeRule[{B[],B[]+pi[]-a[]^2ParamD[t]@eps[]}];
FPsirule=MakeRule[{Psi[],Psi[]+ParamD[t]@a[]/a[] pi[]}];
FErule=MakeRule[{EE[],EE[]-eps[]}];
FScalarPhirule=MakeRule[{ScalarPhi[],ScalarPhi[]-ParamD[t]@Phi0[]pi[]}];
FScalarChirule=MakeRule[{ScalarChi[],ScalarChi[]-ParamD[t]@Chi0[]pi[]}];

FgaugeRules[x_]:=x/.FPhirule/.FBrule/.FPsirule/.FErule/.FScalarPhirule/.FScalarChirule;

TPhirule=MakeRule[{Phi[],Phi[]-ParamD[t]@pi[]}];
TBrule=MakeRule[{B[],B[]+pi[]}];
TPsirule=MakeRule[{Psi[],Psi[]+ParamD[t]@a[]/a[] pi[]}];
TErule=MakeRule[{EE[],EE[]}];
TScalarPhirule=MakeRule[{ScalarPhi[],ScalarPhi[]-ParamD[t]@Phi0[]pi[]}];
TScalarChirule=MakeRule[{ScalarChi[],ScalarChi[]-ParamD[t]@Chi0[]pi[]}];

TgaugeRules[x_]:=x/.TPhirule/.TBrule/.TPsirule/.TErule/.TScalarPhirule/.TScalarChirule;

SPhirule=MakeRule[{Phi[],Phi[]}];
SBrule=MakeRule[{B[],B[]-a[]^2ParamD[t]@eps[]}];
SPsirule=MakeRule[{Psi[],Psi[]}];
SErule=MakeRule[{EE[],EE[]-eps[]}];
SScalarPhirule=MakeRule[{ScalarPhi[],ScalarPhi[]}];
SScalarChirule=MakeRule[{ScalarChi[],ScalarChi[]}];

SgaugeRules[x_]:=x/.SPhirule/.SBrule/.SPsirule/.SErule/.SScalarPhirule/.SScalarChirule;


(* ::Subsection::Closed:: *)
(*Fourier symbol:*)


DefConstantSymbol[FourierK,PrintAs->"\[Kappa]"];



(* ::Section:: *)
(*Lagrangians:*)


(* ::Subsection::Closed:: *)
(*Gravitational Lagrangians:*)


GLag0F=a[]^3/2(Lhh1[]deltahTr[] deltahTr[] +2a[]^(-4)Lhh2[]deltah[b,-c]deltah[c,-b])+Ld2deth[]d2deth[]+a[]^3(1/2LSS[]a[]^2Dshift[-b]Dshift[b]+1/2LNN[]Dlapse[]Dlapse[]+LNh[]Dlapse[]deltahTr[]+LN[](D2lapse[]+Dlapse[]ddeth[]/a[]^3));

GLag1F=a[]^3(LdSh1[]PD[-b]@Dshift[b]deltahTr[]+
2LdSh2[]a[]^(-2)deltah[-b,-c]PD[b]@Dshift[c]+
2LhK2[]a[]^(-2)deltah[-b,c]DExK[b,-c]+
LhK1[]DExK[c,-c]deltahTr[]+LNK[]Dlapse[]DExK[c,-c]+
LNdS[]Dlapse[]PD[-b]@Dshift[b]);


GLag2F=
a[]^3(2LhR2[]a[]^(-2)deltah[-b,c]DRicci[b,-c]+
LhR1[]DRicci[v,-v]deltahTr[]+
LR[]D2Ricci[]+LddotSh1[]PD[-b]@ParamD[t]@Dshift[b]deltahTr[]+
2LddotSh2[]a[]^(-2)deltah[-b,-c]PD[b]@ParamD[t]@Dshift[c]+
1/2LdotSdotS[]a[]^2 ParamD[t]@Dshift[-d]ParamD[t]@Dshift[d]+
1/2LdSdS1[]PD[b]@Dshift[-b]PD[c]@Dshift[-c]+
1/2LdSdS2[]PD[b]@Dshift[c]PD[-b]@Dshift[-c]+
1/2LKK1[]DExK[b,-b]DExK[c,-c]+
LKK2[]DExK[b,-c]DExK[c,-b]+
1/2LNdotNdot[](ParamD[t]@Dlapse[])^2+
1/2LdNdN[]a[]^(-2)PD[-b]@Dlapse[]PD[b]@Dlapse[]+
Lhd2N1[]a[]^(-2)deltahTr[]PD[c]@PD[-c]@Dlapse[]+
2Lhd2N2[]a[]^(-4)deltah[-b,-c]PD[b]@PD[c]@Dlapse[]+
LNR[]Dlapse[]DRicci[v,-v]+
LNdotK[]DExK[-b,b]ParamD[t]@Dlapse[]+
LNdSdot[]Dlapse[]PD[-b]@ParamD[t]@Dshift[b]+
LdSK1[]DExK[b,-b]PD[-c]@Dshift[c]+
2LdSK2[]DExK[c,-b]PD[-c]@Dshift[b]);

GLag3F=a[]^3(Lhd3S1[](1/a[]^2)deltahTr[]PD[-b]@PD[b]@PD[-c]@Dshift[c]+2Lhd3S2[](1/a[]^4)deltah[-b,-c]PD[-d]@PD[d]@PD[b]@Dshift[c]+2Lhd3S3[](1/a[]^4)deltah[-b,-c]PD[b]@PD[c]@PD[-d]@Dshift[d]+Lhd2Ndot1[](1/a[]^2)deltahTr[]PD[-b]@PD[b]@ParamD[t]@Dlapse[]+2Lhd2Ndot2[](1/a[]^4)deltah[-b,-c]PD[b]@PD[c]@ParamD[t]@Dlapse[]+LdSR1[]DRicci[b,-b]PD[-c]@Dshift[c]+2LdSR2[]DRicci[b,-c]PD[-b]@Dshift[c]+LKR1[]DExK[b,-b]DRicci[c,-c]+2LKR2[]DExK[b,-c]DRicci[c,-b]+LNdotR[]DRicci[b,-b]ParamD[t]@Dlapse[]+LKd2N1[](1/a[]^2)DExK[b,-b]PD[-c]@PD[c]@Dlapse[]+2(1/a[]^2)LKd2N2[]DExK[b,-c]PD[-b]@PD[c]@Dlapse[]+LdSdotK1[]DExK[b,-b]PD[-c]@ParamD[t]@Dshift[c]+2LdSdotK2[]DExK[b,-c]PD[-b]@ParamD[t]@Dshift[c]+LNdotdSdot[]ParamD[t]@Dlapse[]PD[-b]@ParamD[t]@Dshift[b]+(1/a[]^2)Ld2NdS[]PD[-b]@PD[b]@Dlapse[]PD[-c]@Dshift[c]);

GLag4F=a[]^3(1/2LRR1[]DRicci[b,-b]DRicci[c,-c]+LRR2[]DRicci[b,-c]DRicci[c,-b]+LRdSdot1[]DRicci[b,-b]PD[-c]@ParamD[t]@Dshift[c]+LRdSdot2[]DRicci[b,-c]PD[-b]@ParamD[t]@Dshift[c]+LdSdotdSdot1[]PD[-b]@ParamD[t]@Dshift[b]PD[-c]@ParamD[t]@Dshift[c]+LdSdotdSdot2[]PD[c]@ParamD[t]@Dshift[-b]PD[-c]@ParamD[t]@Dshift[b]+
(1/a[]^2)Ld2NdSdot[]PD[-c]@PD[c]@Dlapse[]PD[-d]@ParamD[t]@Dshift[d]+
(1/a[]^2)Lhd3Sdot1[]deltahTr[]PD[-c]@PD[c]@PD[-b]@ParamD[t]@Dshift[b]+(1/a[]^4)Lhd3Sdot2[]deltah[-b,-d]PD[-c]@PD[c]@PD[b]@ParamD[t]@Dshift[d]+(1/a[]^4)Lhd3Sdot3[]deltah[-b,-d]PD[b]@PD[d]@PD[-c]@ParamD[t]@Dshift[c]+(1/a[]^4)(1/2)Ld2Nd2N[]PD[-b]@PD[b]@Dlapse[]PD[-c]@PD[c]@Dlapse[]+(1/a[]^2)Ld2Sd2S1[]PD[-b]@PD[-c]@Dshift[b]PD[c]@PD[-d]@Dshift[d]+(1/a[]^2)Ld2Sd2S2[]PD[-c]@PD[c]@Dshift[d]PD[-b]@PD[b]@Dshift[-d]+(1/a[]^4)Lhd4N1[]deltahTr[]PD[-b]@PD[b]@PD[-c]@PD[c]@Dlapse[]+(1/a[]^6)Lhd4N2[]deltah[-b,-c]PD[b]@PD[c]@PD[-d]@PD[d]@Dlapse[]+(1/a[]^2)LKd3S1[]DExK[b,-b]PD[-c]@PD[c]@PD[-d]@Dshift[d]+(1/a[]^2)LKd3S2[]DExK[b,-d]PD[-c]@PD[c]@PD[-b]@Dshift[d]+(1/a[]^2)LKd3S3[]DExK[b,-d]PD[-b]@PD[d]@PD[-c]@Dshift[c]+(1/a[]^2)LKd2Ndot1[]DExK[b,-b]PD[-c]@PD[c]@ParamD[t]@Dlapse[]+(1/a[]^2)LKd2Ndot2[]DExK[b,-c]PD[c]@PD[-b]@ParamD[t]@Dlapse[]+(1/2)(1/a[]^2)LdNdotdNdot[]PD[-b]@ParamD[t]@Dlapse[]PD[b]@ParamD[t]@Dlapse[]);



(* ::Subsection::Closed:: *)
(*Minimally coupled matter scalar Lagrangians:*)


LagM1F=-(1/2(ParamD[t]@Phi0[])^2-V0[])d2deth[]+a[]^3 (1/2(ParamD[t]@Phi0[])^2+V0[])D2lapse[]+(1/2(ParamD[t]@Phi0[])^2+V0[])Dlapse[]ddeth[]-a[]^3/2((ParamD[t]@Phi0[])^2)Dlapse[]^2;

LagM2F=a[]^3(1/2 V0pp[]ScalarPhi[]^2+(V0p[]ScalarPhi[]+ParamD[t]@ScalarPhi[]ParamD[t]@Phi0[])Dlapse[] + 1/(2 a[]^2)PD[-b]@ScalarPhi[]PD[b]@ScalarPhi[] -1/2ParamD[t]@ScalarPhi[]ParamD[t]@ScalarPhi[] +ParamD[t]@Phi0[]PD[-b]@ScalarPhi[]Dshift[b]);

LagM3F=ddeth[](V0p[]ScalarPhi[]-ParamD[t]@ScalarPhi[]ParamD[t]@Phi0[]);

LagMF = -LagM1F-LagM2F-LagM3F;



(* ::Subsection::Closed:: *)
(*Extra Scalar(-Tensor) Lagrangians:*)


LagST0=a[]^3/2(Lchichi[] (ScalarChi[])^2  +2Lchih[]ScalarChi[]deltahTr[]  +LchiN[]ScalarChi[]Dlapse[] );

LagST1=a[]^3(Lchidoth[]ParamD[t]@ScalarChi[]deltahTr[]+ LchiK[]ScalarChi[]DExK[b,-b]+LchidS[]ScalarChi[]PD[-b]@Dshift[b]+LchidotN[]ParamD[t]@ScalarChi[]Dlapse[]);

LagST2=a[]^3( LchiR[]ScalarChi[]DRicci[b,-b]+Ld2chih1[](1/a[]^2)deltahTr[]PD[-b]@PD[b]@ScalarChi[]+2(1/a[]^4)Ld2chih2[]deltah[-b,-c]PD[b]@PD[c]@ScalarChi[]+(1/2)Lchidotchidot[]ParamD[t]@ScalarChi[]ParamD[t]@ScalarChi[]+LKchidot[]DExK[b,-b]ParamD[t]@ScalarChi[]+(1/2)(1/a[]^2)Ldchidchi[]PD[-b]@ScalarChi[]PD[b]@ScalarChi[]+LchidotNdot[]ParamD[t]@ScalarChi[]ParamD[t]@Dlapse[]+LchidotdS[]ParamD[t]@ScalarChi[]PD[-b]@Dshift[b]+(1/a[]^2)LdchidN[]PD[-b]@ScalarChi[]PD[b]@Dlapse[]);

LagST3=a[]^3(LRchidot[]DRicci[b,-b]ParamD[t]@ScalarChi[]+(1/a[]^2)Ld2chidoth1[]PD[-b]@PD[b]@ParamD[t]@ScalarChi[]deltahTr[]+2(1/a[]^4)Ld2chidoth2[]deltah[-b,-c]PD[b]@PD[c]@ParamD[t]@ScalarChi[]+(1/a[]^2)Ld2chidotN[]Dlapse[]PD[-b]@PD[b]@ParamD[t]@ScalarChi[]+LchidotdSdot[]ParamD[t]@ScalarChi[]PD[-b]@ParamD[t]@Dshift[b]+(1/a[]^2)Ld2chiK1[]DExK[b,-b]PD[-c]@PD[c]@ScalarChi[]+2(1/a[]^2)Ld2chiK2[]DExK[b,-c]PD[c]@PD[-b]@ScalarChi[]+(1/a[]^2)Ld2chidS[]PD[-b]@PD[b]@ScalarChi[]PD[-c]@Dshift[c]);

LagST4=a[]^3(Ld2chiR1[](1/a[]^2)DRicci[b,-b]PD[-c]@PD[c]@ScalarChi[]+2Ld2chiR2[](1/a[]^2)DRicci[b,-c]PD[-b]@PD[c]@ScalarChi[]+Ld4chih1[](1/a[]^4)deltahTr[]PD[-b]@PD[b]@PD[-c]@PD[c]@ScalarChi[]+4Ld4chih2[](1/a[]^6)deltah[-b,-d]PD[b]@PD[d]@PD[-c]@PD[c]@ScalarChi[]+1/2Ld2chid2chi[](1/a[]^4)PD[-b]@PD[b]@ScalarChi[]PD[-c]@PD[c]@ScalarChi[]+1/2Ldchidotdchidot[](1/a[]^2)PD[-b]@ParamD[t]@ScalarChi[]PD[b]@ParamD[t]@ScalarChi[]+Ld2Nd2chi[](1/a[]^4)PD[-c]@PD[c]@Dlapse[]PD[-b]@PD[b]@ScalarChi[]+Ld2chidSdot[](1/a[]^2)PD[-b]@PD[b]@ScalarChi[]PD[-c]@ParamD[t]@Dshift[c]+LdchidotdNdot[](1/a[]^2)PD[-c]@ParamD[t]@ScalarChi[]PD[c]@ParamD[t]@Dlapse[]+LKd2chidot1[](1/a[]^2)DExK[b,-b]PD[-c]@PD[c]@ParamD[t]@ScalarChi[]+LKd2chidot2[](1/a[]^2)DExK[b,-c]PD[c]@PD[-b]@ParamD[t]@ScalarChi[]);



(* ::Section:: *)
(*Commutation, Integration-by-parts, Fourier and Potential rules*)


(* ::Subsection::Closed:: *)
(*Commutators:*)


HoldParamD[x_]:=x/.PD[YY_]@ParamD[t,t]@XX_->Hold[PD[YY]@ParamD[t]@ParamD[t]@XX]/.ParamD[t,t]@XX_->Hold[ParamD[t]@ParamD[t]@XX]/.PD[YY_]@ParamD[t,t,t]@XX_->Hold[PD[YY]@ParamD[t]@ParamD[t]@ParamD[t]@XX]/.ParamD[t,t,t]@XX_->Hold[ParamD[t]@ParamD[t]@ParamD[t]@XX]/.PD[YY_]@ParamD[t,t,t,t]@XX_->Hold[PD[YY]@ParamD[t]@ParamD[t]@ParamD[t]@ParamD[t]@XX]/.ParamD[t,t,t,t]@XX_->Hold[ParamD[t]@ParamD[t]@ParamD[t]@ParamD[t]@XX]/.ParamD[t,t,t,t,t]@XX_->Hold[ParamD[t]@ParamD[t]@ParamD[t]@ParamD[t]@ParamD[t]@XX];


comRight1=MakeRule[{ParamD[t]@PD[b]@XX_,PD[b]@ParamD[t]@XX },MetricOn->None,ContractMetrics->False];
comRight2=MakeRule[{ParamD[t]@PD[-b]@XX_,PD[-b]@ParamD[t]@XX },MetricOn->None,ContractMetrics->False];
comLeft1=MakeRule[{PD[b]@ParamD[t]@XX_,ParamD[t]@PD[b]@XX },MetricOn->None,ContractMetrics->False];
comLeft2=MakeRule[{PD[-b]@ParamD[t]@XX_,ParamD[t]@PD[-b]@XX },MetricOn->None,ContractMetrics->False];

RightCOM[x_]:=x/.comRight1/.comRight2/.comRight1/.comRight2/.comRight1/.comRight2/.comRight1/.comRight2;

LeftCOM[x_]:=x/.comLeft1/.comLeft2/.comLeft1/.comLeft2/.comLeft1/.comLeft2/.comLeft1/.comLeft2;

AtoH[x_]:=x/.ParamD[t,t,t]@a[]->ParamD[t,t]@(a[]H[])/.ParamD[t,t]@a[]-> ParamD[t]@(a[]H[])/.ParamD[t]@a[]->a[]H[];




(* ::Subsection::Closed:: *)
(*Integration-by-parts module:*)


IBP[Lag_,field_]:=Module[{Laglength,jj,term,one,two,length,ii,sign,temp,LagReturn,LagCopy,extra,tempprint},

LagCopy=Lag;
LagReturn=0;
If[field===pi[],
kk=1,
oo=1]

If[field===pi[],
Print[Dynamic[kk]," / "<>ToString[Length[LagCopy]]],
Print[Dynamic[oo]," / "<>ToString[Length[LagCopy]]]]


If[!StringFreeQ[ToString[LagCopy,InputForm],"+"],
Laglength=LagCopy//Length;

If[field===pi[],
kk=1,
oo=1];


For[jj=1,jj<Laglength+1,jj++,

If[field===pi[],
kk=jj,
oo=jj];

term=LagCopy[[jj]];

sign=1;
length=term//Length;
one=1;
two=1;

For[ii=1,ii<length+1,ii++,
If[FreeQ[term[[ii]],field],one=one term[[ii]],two=two term[[ii]]];]

If[two===1,LagReturn+=one;Continue[],sign=sign];


If[!(two===field),
While[!(two===field),temp=1;If[MatchQ[two,PD[ZZZ__]@YY_],temp=two/.PD[ZZZ__]@YY_->PD[ZZZ];two=two/.PD[ZZZ__]@YY_->YY;one=-temp@one;,temp=1];temp=1;If[MatchQ[two,ParamD[t]@YY_],temp=ParamD[t];two=two/.ParamD[t]@YY_->YY;one=-temp@one;,temp=1];
temp=1;If[MatchQ[two,ParamD[t,t]@YY_],temp=ParamD[t,t];two=two/.ParamD[t,t]@YY_->YY;one=temp@one;,temp=1;];
temp=1;],sign=sign];

one ;
two;
extra=sign one two//ToCanonical;
LagReturn=LagReturn+extra;
],

term=LagCopy;
sign=1;
length=term//Length;
one=1;
two=1;

For[ii=1,ii<length+1,ii++,
If[FreeQ[term[[ii]],field],one=one term[[ii]],two=two term[[ii]]];]

If[two===1,LagReturn=one;,

While[!(two===field),temp=1;If[MatchQ[two,PD[ZZZ__]@YY_],temp=two/.PD[ZZZ__]@YY_->PD[ZZZ];two=two/.PD[ZZZ__]@YY_->YY;one=-temp@one;,temp=1];temp=1;If[MatchQ[two,ParamD[t]@YY_],temp=ParamD[t];two=two/.ParamD[t]@YY_->YY;one=-temp@one;,temp=1];
temp=1;
If[MatchQ[two,ParamD[t,t]@YY_],temp=ParamD[t,t];two=two/.ParamD[t,t]@YY_->YY;one=temp@one;,temp=1;];temp=1;If[MatchQ[two,ParamD[t,t,t]@YY_],temp=ParamD[t,t,t];two=two/.ParamD[t,t,t]@YY_->YY;one=-temp@one;,temp=1;];
temp=1;]

one ;
two;
LagReturn=(sign one two)//ToCanonical;
];

];

Return[LagReturn];

]


(* ::Subsection::Closed:: *)
(*Fourier rules:*)


FourierHack[Lag_]:=Module[{LagOut},
LagOut=Lag;
While[!FreeQ[LagOut,PD[_]],
LagOut=LagOut/.PD[b_]@YY_->YY I FourierK;];
LagOut=LagOut//ToCanonical;
Return[LagOut];
]


FourierRule[Lag_]:=Module[{LagOut},
LagOut=Lag;
LagOut=LagOut/.MakeRule[{PD[c]@PD[-c]@XX_,-FourierK^2 XX},MetricOn->All,ContractMetrics->True]/.MakeRule[{PD[-c]@PD[c]@XX_,-FourierK^2 XX},MetricOn->All,ContractMetrics->True];
LagOut=LagOut//ToCanonical;
Return[LagOut];
]


DefTensor[Kappa[],{M3,t},PrintAs->"\[Kappa]"]
FourierReplace[x_]:=x/.FourierK->Kappa[];
FourierReplaceInv[x_]:=x/.Kappa[]->FourierK;



(* ::Subsection::Closed:: *)
(*Potential rule:*)


Vrules[x_]:=x/.ParamD[t]@V0[]->V0p[]ParamD[t]@Phi0[]/.ParamD[t]@V0p[]->V0pp[]ParamD[t]@Phi0[];


(* ::Section::Closed:: *)
(*Main (Noether constraint) modules:*)


Con[Lag_]:=Module[{constraints,param,perts,lagpi,lageps},

lagpi=Coefficient[Lag,pi[],1];
lageps=Coefficient[Lag,eps[],1];

param=0;
constraints={};

perts={Phi[],Psi[],EE[],B[],
ParamD[t]@Phi[],ParamD[t]@Psi[],ParamD[t]@EE[],ParamD[t]@B[],
ParamD[t,t]@Phi[],ParamD[t,t]@Psi[],ParamD[t,t]@EE[],ParamD[t,t]@B[],ParamD[t,t,t]@Phi[],ParamD[t,t,t]@Psi[],ParamD[t,t,t]@EE[],ParamD[t,t,t]@B[],ScalarPhi[],ParamD[t]@ScalarPhi[],ParamD[t,t]@ScalarPhi[],ParamD[t,t,t]@ScalarPhi[]};

For[ii=1,ii<Length[perts]+1,ii++,
For[jj=0,jj<7,jj++,
If[Coefficient[Coefficient[lagpi,perts[[ii]],1],FourierK,jj]===0,param=param+1,constraints=Append[constraints,Coefficient[Coefficient[lagpi,perts[[ii]],1],FourierK,jj]];]
]
];

For[ii=1,ii<Length[perts]+1,ii++,
For[jj=0,jj<7,jj++,
If[Coefficient[Coefficient[lageps,perts[[ii]],1],FourierK,jj]===0,param=param+1,constraints=Append[constraints,Coefficient[Coefficient[lageps,perts[[ii]],1],FourierK,jj]];]
]
];
Return[constraints];
]

NoetherCoefficient[Lag_,DiffField_,PertField_,FourierOrder_]:=Coefficient[Coefficient[Coefficient[Lag,PertField,1],DiffField,1],FourierK,FourierOrder];

PreNoether[Lag_]:=Module[{obj},

obj=Lag//NoScalar;

Print["Integrating by parts for \[Pi]"];
obj=IBP[obj,pi[]]/.H[]->ParamD[t]@a[]/a[];
Print["Integrating by parts for \[Epsilon]"];
obj=IBP[obj,eps[]]/.H[]->ParamD[t]@a[]/a[];
obj=obj//ToCanonical;
Print["Applying commutation relations II and Fourier transforming"];

obj=obj//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//LeftCOM//Release//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//LeftCOM//Release//ToCanonical;
obj=obj//HoldParamD//RightCOM//Release//HoldParamD//RightCOM//Release//HoldParamD//RightCOM//Release//RightCOM//Release//HoldParamD//RightCOM//Release//HoldParamD//RightCOM//Release//HoldParamD//RightCOM//Release//RightCOM//Release//ToCanonical;

Off[ToCanonical::noident];
obj=obj//FourierRule//ToCanonical//FourierRule//ToCanonical;
On[ToCanonical::noident];

obj=obj//Vrules//Vrules//ToCanonical;

Return[obj];]



PertGaugeOrder[Lag_]:=Module[{LagTemp,output},
Print["Perturbing and Gauge-transforming"];
LagTemp=Lag//pertRules//FgaugeRules;
LagTemp=LagTemp/.orderpi/.ordereps;
LagTemp=LagTemp/.H[]->(ParamD[t]@a[])/a[];
Print["Re-ordering Lagrangians"];
LagTemp=LagTemp//ToCanonical;
output=Coefficient[LagTemp,order,1]//ToCanonical//NoScalar;
Print["Applying commutation relations I"];
output=output//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//HoldParamD//LeftCOM//Release//ToCanonical;
Return[output];
]


(* ::Section:: *)
(*Symbol setup and extras:*)


(* ::Subsection::Closed:: *)
(*List of original Lagrangian coefficients:*)


G12List={Lhh1[],Lhh2[],Ld2deth[],LSS[],LNN[],LNh[],LN[],LdSh1[],LdSh2[],LhK2[],LhK1[],LNK[],LNdS[],LhR2[],LhR1[],LR[],LddotSh1[],LddotSh2[],LdotSdotS[],LdSdS1[],LdSdS2[],LKK1[],LKK2[],LNdotNdot[],LdNdN[],Lhd2N1[],Lhd2N2[],LNR[],LNdotK[],LNdSdot[],LdSK1[],LdSK2[]};

G34List={Lhd3S1[],Lhd3S2[],Lhd3S3[],Lhd2Ndot1[],Lhd2Ndot2[],LdSR1[],LdSR2[],LKR1[],LKR2[],LNdotR[],LKd2N1[],LKd2N2[],LdSdotK1[],LdSdotK2[],LNdotdSdot[],Ld2NdS[],
LRR1[],LRdSdot1[],LRdSdot2[],LdSdotdSdot1[],LdSdotdSdot2[],Ld2NdSdot[],Lhd3Sdot1[],Lhd3Sdot2[],Lhd3Sdot3[],Ld2Nd2N[],Ld2Sd2S1[],Ld2Sd2S2[],Lhd4N1[],Lhd4N2[],LKd3S1[],LKd3S2[],LKd3S3[],LKd2Ndot1[],LKd2Ndot2[],LdNdotdNdot[]};

GList={Lhh1[],Lhh2[],Ld2deth[],LSS[],LNN[],LNh[],LN[],LdSh1[],LdSh2[],LhK2[],LhK1[],LNK[],LNdS[],LhR2[],LhR1[],LR[],LddotSh1[],LddotSh2[],LdotSdotS[],LdSdS1[],LdSdS2[],LKK1[],LKK2[],LNdotNdot[],LdNdN[],Lhd2N1[],Lhd2N2[],LNR[],LNdotK[],LNdSdot[],LdSK1[],LdSK2[],Lhd3S1[],Lhd3S2[],Lhd3S3[],Lhd2Ndot1[],Lhd2Ndot2[],LdSR1[],LdSR2[],LKR1[],LKR2[],LNdotR[],LKd2N1[],LKd2N2[],LdSdotK1[],LdSdotK2[],LNdotdSdot[],Ld2NdS[],LRR1[],LRdSdot1[],LRdSdot2[],LdSdotdSdot1[],LdSdotdSdot2[],Ld2NdSdot[],Lhd3Sdot1[],Lhd3Sdot2[],Lhd3Sdot3[],Ld2Nd2N[],Ld2Sd2S1[],Ld2Sd2S2[],Lhd4N1[],Lhd4N2[],LKd3S1[],LKd3S2[],LKd3S3[],LKd2Ndot1[],LKd2Ndot2[],LdNdotdNdot[]};


ST12List={Lchichi[], Lchih[],LchiN[],Lchidoth[],LchiK[],LchidS[],LchidotN[],LchiR[],Ld2chih1[],Ld2chih2[],Lchidotchidot[],LKchidot[],Ldchidchi[],LchidotNdot[],LchidotdS[],LdchidN[]};

ST34List={LRchidot[],Ld2chidoth1[],Ld2chidoth2[],Ld2chidotN[],LchidotdSdot[],Ld2chiK1[],Ld2chiK2[],Ld2chidS[],Ld2chiR1[],Ld2chiR2[],Ld4chih1[],Ld4chih2[],Ld2chid2chi[],Ldchidotdchidot[],Ld2Nd2chi[],Ld2chidSdot[],LdchidotdNdot[],LKd2chidot1[],LKd2chidot2[]};

STList={Lchichi[], Lchih[],LchiN[],Lchidoth[],LchiK[],LchidS[],LchidotN[],LchiR[],Ld2chih1[],Ld2chih2[],Lchidotchidot[],LKchidot[],Ldchidchi[],LchidotNdot[],LchidotdS[],LdchidN[],LRchidot[],Ld2chidoth1[],Ld2chidoth2[],Ld2chidotN[],LchidotdSdot[],Ld2chiK1[],Ld2chiK2[],Ld2chidS[],Ld2chiR1[],Ld2chiR2[],Ld4chih1[],Ld4chih2[],Ld2chid2chi[],Ldchidotdchidot[],Ld2Nd2chi[],Ld2chidSdot[],LdchidotdNdot[],LKd2chidot1[],LKd2chidot2[]};



GSTList={Lhh1[],Lhh2[],Ld2deth[],LSS[],LNN[],LNh[],LN[],LdSh1[],LdSh2[],LhK2[],LhK1[],LNK[],LNdS[],LhR2[],LhR1[],LR[],LddotSh1[],LddotSh2[],LdotSdotS[],LdSdS1[],LdSdS2[],LKK1[],LKK2[],LNdotNdot[],LdNdN[],Lhd2N1[],Lhd2N2[],LNR[],LNdotK[],LNdSdot[],LdSK1[],LdSK2[],Lhd3S1[],Lhd3S2[],Lhd3S3[],Lhd2Ndot1[],Lhd2Ndot2[],LdSR1[],LdSR2[],LKR1[],LKR2[],LNdotR[],LKd2N1[],LKd2N2[],LdSdotK1[],LdSdotK2[],LNdotdSdot[],Ld2NdS[],LRR1[],LRdSdot1[],LRdSdot2[],LdSdotdSdot1[],LdSdotdSdot2[],Ld2NdSdot[],Lhd3Sdot1[],Lhd3Sdot2[],Lhd3Sdot3[],Ld2Nd2N[],Ld2Sd2S1[],Ld2Sd2S2[],Lhd4N1[],Lhd4N2[],LKd3S1[],LKd3S2[],LKd3S3[],LKd2Ndot1[],LKd2Ndot2[],LdNdotdNdot[],Lchichi[], Lchih[],LchiN[],Lchidoth[],LchiK[],LchidS[],LchidotN[],LchiR[],Ld2chih1[],Ld2chih2[],Lchidotchidot[],LKchidot[],Ldchidchi[],LchidotNdot[],LchidotdS[],LdchidN[],LRchidot[],Ld2chidoth1[],Ld2chidoth2[],Ld2chidotN[],LchidotdSdot[],Ld2chiK1[],Ld2chiK2[],Ld2chidS[],Ld2chiR1[],Ld2chiR2[],Ld4chih1[],Ld4chih2[],Ld2chid2chi[],Ldchidotdchidot[],Ld2Nd2chi[],Ld2chidSdot[],LdchidotdNdot[],LKd2chidot1[],LKd2chidot2[]};


(* ::Subsection::Closed:: *)
(*Initialise additional coefficient rules and Extras:*)


SolToEqn[sollist_]:=Module[{eqnlist,solLength,element,split,newelement},
solLength=sollist//Length;
eqnlist={};
For[iii=1,iii<solLength+1,iii++,
element=ToString[sollist[[iii]],InputForm];
split=StringSplit[element,"->"];
newelement=ToExpression[split[[1]]]==ToExpression[split[[2]]];
eqnlist=Append[eqnlist,newelement];];
Return[eqnlist];
];


SetupLIBPs[list_]:=Module[{LDerivativeIBPrules,rule1},
LDerivativeIBPrules={};
For[ii=1,ii<Length[list]+1,ii++,
rule1=ParamD[t]@list[[ii]]XX_->-list[[ii]]ParamD[t]@XX;
LDerivativeIBPrules=Append[LDerivativeIBPrules,rule1];
];
Return[LDerivativeIBPrules];
]


rulez=SetupLIBPs[GSTList];


(* ::Subsection::Closed:: *)
(*Final physical coefficients*)


DefTensor[M[],{M3,t},PrintAs->"M"];
DefTensor[\[Alpha]B[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","B"],StandardForm]];
DefTensor[\[Alpha]K[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","K"],StandardForm]];
DefTensor[\[Alpha]T[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","T"],StandardForm]];
DefTensor[\[Alpha]H[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","H"],StandardForm]];

DefTensor[\[Alpha]Q1[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","Q1"],StandardForm]];
DefTensor[\[Alpha]Q2[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","Q2"],StandardForm]];
DefTensor[\[Alpha]Q3[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","Q3"],StandardForm]];
DefTensor[\[Alpha]Q4[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","Q4"],StandardForm]];
DefTensor[\[Alpha]Q5[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","Q5"],StandardForm]];
DefTensor[\[Alpha]P[],{M3,t},PrintAs->ToString[Subscript["\[Alpha]","P"],StandardForm]];


alphas={\[Alpha]B[],\[Alpha]K[],\[Alpha]T[],\[Alpha]H[],\[Alpha]Q1[],\[Alpha]Q2[],\[Alpha]Q3[],\[Alpha]Q4[],\[Alpha]Q5[],\[Alpha]P[]};
Alpharulez=SetupLIBPs[alphas];


EndPackage[]
