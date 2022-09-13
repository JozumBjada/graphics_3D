(* ::Package:: *)

(* ::Title:: *)
(*Spin vs. OAM*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "OAM_vs_spin_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:OAM_vs_spin_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Text:: *)
(*Description: Comparison of the action of spin and orbital angular momentum on massive particles*)


(* ::Section:: *)
(*Preliminaries*)


(* ::Input::Initialization:: *)
fontSize=50;
fontFamily="CMU Serif";


(* ::Input::Initialization:: *)
omegaSpin=8;
omegaOAM=4;


(* ::Input::Initialization:: *)
num=30;
numStages=6;
\[CapitalDelta]t=1/(num numStages);


(* ::Input::Initialization:: *)
opts={Boxed->False,ViewPoint->{-1.551586529122855`,-2.4910493736907173`,1.6844145156343122`},ViewVertical->{-1,0,0},PlotRange->{{-3.7,3.6},{-3.5`,3.5`},{-4.8`,2.1`}}};


(* ::Section:: *)
(*Intensity profiles 3D*)


(* ::Input::Initialization:: *)
Module[{l=2,w0=2,\[Lambda]=1,w0f,plot,zR,w,fun},
{w0f,zR}={0.05w0,(\[Pi] w0^2)/\[Lambda]};
w[z_]:=w0 Sqrt[1+(z/zR)^2];
fun[x_,y_,z_,l_,w0_]:=Module[{r=Sqrt[x^2+y^2]},Sqrt[2./(\[Pi] Abs[l]!)] w0/w[z] ((r Sqrt[2])/w[z])^Abs[l] Exp[-(r/w[z])^2]];

plot=DensityPlot3D[Abs[fun[x,y,z,l,w0]/fun[w0 Sqrt[l/2],0,0,l,w0]]^2,{x,-4,4},{y,-4,4},{z,0,5},OpacityFunction->Function[x,.07(1-Exp[-25 x^2])],ColorFunction->(Blend[{Purple,Orange},#]&),PlotPoints->70];
rastShaft=First@Cases[InputForm[plot],_Raster3D,Infinity,1];

plot=DensityPlot3D[Abs[fun[x,y,z,l,w0f]/fun[w0f Sqrt[l/2],0,0,l,w0f]]^2,{x,-4,4},{y,-4,4},{z,0,2},OpacityFunction->Function[x,.07(1-Exp[-25 x^2])],ColorFunction->(Blend[{Purple,Orange},#]&),PlotPoints->50];
rastFlange=First@Cases[InputForm[plot],_Raster3D,Infinity,1];
]


(* ::Section:: *)
(*Components*)


(* ::Input::Initialization:: *)
{helixFun[2],helixFun[-2]}=Module[{aux},
aux=ParametricPlot3D[{u Cos[# 2\[Pi] t],u Sin[# 2\[Pi] t],t},{t,0,5},{u,1,3},PlotPoints->50,Mesh->None,Boxed->False,Axes->False];
First@Cases[InputForm[aux],_GraphicsComplex,Infinity,1]
]&/@{-1/2,+1/2};


(* ::Input::Initialization:: *)
hb=BoundaryDiscretizeRegion@RegionIntersection[Ball[{0,0,0},1],Cuboid[-1.1{1,1,1},1.1{1,1,0}]];
hb=First@Cases[InputForm[Show[hb]],_GraphicsComplex,Infinity,1];
hb=GraphicsComplex[hb[[1]],{EdgeForm[],hb[[2,2]]}];
ball=Scale[Rotate[{Green,hb,Red,Rotate[hb,\[Pi],{1,0,0}]}, \[Pi]/2,{0,1,0}],0.8];


(* ::Input::Initialization:: *)
arrow=Table[2{Cos[x],Sin[x],0},{x,0,2\[Pi] 0.95,0.1}];
AppendTo[arrow,With[{x=2\[Pi] 0.98},2.1{Cos[x],Sin[x],0}]];
{arrowSpinR,arrowSpinL}=
{Black,Arrowheads[0.05],Arrow[Tube[(RotationTransform[-\[Pi]/2,{0,0,1}]@*ScalingTransform[0.45{1,1,1}]@*TranslationTransform[{0,0,0.2}])[If[#,RotationTransform[-(\[Pi]/5),{0,0,1}]@RotationTransform[\[Pi],{1,0,0}][arrow],arrow]],0.04]]}&/@{True,False};
{arrowOAMR,arrowOAML}=
{Black,Arrowheads[0.05],Arrow[Tube[(RotationTransform[-\[Pi]/2,{0,0,1}]@*TranslationTransform[{0,0,0.1}])[If[#,RotationTransform[-(\[Pi]/5),{0,0,1}]@RotationTransform[\[Pi],{1,0,0}][arrow],arrow]],0.04]]}&/@{True,False};


(* ::Section:: *)
(*Insets*)


(* ::Input::Initialization:: *)
label[text_,opts_:Plain]:=Style[text,opts,fontSize,FontFamily->fontFamily]


(* ::Input::Initialization:: *)
dashedLine={Lighting->"Neutral",Gray,Dashing[{.1,.04}],Thickness[0.005],Line[{{0,0,-6},{0,0,2}}]};


(* ::Input::Initialization:: *)
insetOAM[t_,l_]:=Graphics3D[{dashedLine,{Rotate[Translate[helixFun[l],{0,0,-5}],2\[Pi] Sign[l]omegaOAM t,{0,0,1}]}},Sequence@@opts]


(* ::Input::Initialization:: *)
vecplot=VectorPlot[{1,0},{x,-1,1},{y,-1,1},RegionFunction->(0.5<=#1^2+#2^2<=1.2&),VectorPoints->{8,8},VectorScale->Small,VectorStyle->Purple];
pos=Cases[InputForm[vecplot],Arrow[pts_]:>Append[ScalingTransform[2.5{1,1}][First[pts]],-2],Infinity];
arrowsFun[t_,0]:=Module[{len=Cos[2\[Pi] t]},If[Abs@len<0.5,Line[{#,#+{len,0,0}}]&/@pos,Arrow[{#,#+{len,0,0}}]&/@pos]];
arrowsFun[t_,\[Sigma]_]:=Arrow@Tube[{#,#+Append[AngleVector[2\[Pi] \[Sigma] t],0]}]&/@pos;


(* ::Input::Initialization:: *)
insetSpin[t_,\[Sigma]_]:=Graphics3D[{dashedLine,{Purple,arrowsFun[t,\[Sigma]]}},Sequence@@opts]


(* ::Input::Initialization:: *)
With[{pt={0.15,0.1},tots={-3,-2,-1,1,2,3},\[CapitalDelta]=.4,num=6},
buttons=Table[{EdgeForm[{Purple,Thickness[0.005]}],Blend[{White,Purple},.5],Rectangle[-pt+{\[CapitalDelta] idx,0},pt+{\[CapitalDelta] idx,0},RoundingRadius->.05],Text[Style[tots[[idx]],Bold,Black,fontSize,FontFamily->fontFamily],{\[CapitalDelta] idx,0}]},{idx,num}];
]
insetCount[l_,\[Sigma]_]:=Module[{count=l+\[Sigma],buttons=buttons,active},
If[count<0,count+=4,count+=3];
buttons=MapAt[ReplaceAll[col_?ColorQ:>Blend[{White,col},0.3]],buttons,Transpose[{Drop[Range[Length[buttons]],{count}]}]];
Prepend[buttons,Text[label["\[ScriptL]\[ThinSpace]+\[ThinSpace]\[Sigma]:"],{0,0}]]
]


(* ::Section:: *)
(*Scene*)


(* ::Input::Initialization:: *)
scene[t_,tloc_,l_,\[Sigma]_]:=Graphics[{
Inset[Graphics3D[{
Translate[rastShaft,{0,0,-5}],
Translate[rastFlange,{0,0,-1.5}],
Rotate[Translate[helixFun[l],{0,0,-5}],Sign[l]2\[Pi] omegaOAM t,{0,0,1}],
Switch[l,2,arrowOAML,-2,arrowOAMR],
Translate[{
Rotate[ball,If[\[Sigma]!=0,2\[Pi] \[Sigma] omegaSpin t,2\[Pi] (-1)omegaSpin (\[CapitalDelta]t num)If[l>0,4,1]],{0,0,1}],
Switch[\[Sigma],0,{},1,arrowSpinL,-1,arrowSpinR]
},2{Cos[Sign[l]2\[Pi] omegaOAM t],Sin[Sign[l]2\[Pi] omegaOAM t],0}]
},Sequence@@opts]
,{-0.3,0.0},ImageScaled[{1,1}/2],2.7]
,
Inset[insetOAM[t,l],{1.1,0.3},ImageScaled[{1,1}/2],1.1],
Inset[insetSpin[tloc,\[Sigma]],{1.1,-0.55},ImageScaled[{1,1}/2],1.1],
Translate[insetCount[l,\[Sigma]],{-1.25,-1.15}],
Text[
TextGrid[{
{label["OAM",Bold],label[":",Bold],label["\[ScriptL]"],label["= "<>ToString[l]]},
{label["spin",Bold],label[":",Bold],label["\[Sigma]"],label["= "<>ToString[\[Sigma]]]}
},Alignment->{{Right,Center,Right,Left},Center},Spacings->{{0.1,0.1,0.5},25}],
{0.5,0.9},{-1,1}]
},PlotRange->{1.5{-1,1},{-1.3,1}},ImageSize->1000]


(* ::Input::Initialization:: *)
animationStages[tglob_,t_,stage_]:=Module[{l,\[Sigma]},
{l,\[Sigma]}=Switch[stage-1,0,{-2,-1},1,{-2,0},2,{-2,1},3,{2,-1},4,{2,0},5|6,{2,1}];
scene[tglob,t,l,\[Sigma]]
]


(* ::Section:: *)
(*Rasterization and export*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*seq=Module[{tglob=0},Flatten[Table[animationStages[\[CapitalDelta]t tglob++,t,s],{s,1,numStages},{t,N@Subdivide[num-1]}],1]];*)


(* ::Input:: *)
(*AbsoluteTiming[frames=Rasterize[#,RasterSize->600]&/@seq;]*)


(* ::Input:: *)
(*Export["anim.gif",frames,AnimationRepetitions->Infinity,"DisplayDurations"->0.22]*)
