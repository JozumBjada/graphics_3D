(* ::Package:: *)

(* ::Title:: *)
(*Quantum Zeno effect*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Quantum_Zeno_effect_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Quantum_Zeno_effect_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Chapter:: *)
(*Routines*)


(* ::Input::Initialization:: *)
indicatorMeas[t_,num_:7]:=Module[{tloc,measpos,m=1.9,h=1,line},
line=Line;
tloc=Rescale[t,{0,1},{-m,m}];
measpos=Round[tloc,2m/num]-Boole[OddQ[num]]m/num;
{
{Thickness[0.01],Translate[line[{{0,0,0},{0,0,h}}],{{0,-m,0},{0,m,0}}]},
Translate[line[{{0,0,0},{0,0,h}}],{0,#,0}&/@Subdivide[-m,m,num]],
line[{{0,-m,h/2},{0,m,h/2}}],
{Gray,Thickness[.012],line[{{0,tloc,0},{0,tloc,h}}]},
If[Abs[tloc-measpos]<.05,{Red,Thickness[.015],line[{{0,measpos,0},{0,measpos,h}}]},{}]
}
]


(* ::Input::Initialization:: *)
measAxes=Module[{list,polFun},
polFun[y_]:=With[{x=2,z=.8},Polygon[{{-x,y,0},{x,y,0},{x,y,z},{-x,y,z}}]];
list=polFun/@(2{1,1/3,-1/3,-1});
{EdgeForm[],Red,list,Rotate[#,\[Pi]/2,{0,0,1}]&/@list}
];


(* ::Input::Initialization:: *)
zrange={-.1,1.01/(2\[Pi] (0.2)^2)};


(* ::Input::Initialization:: *)
ClearAll[plot]
plot[t_,evollist_]:=Module[{m=2,plot,\[Sigma],tloc,meas=measAxes,x0,y0,num=Length[evollist],idx,corrfac=.95,meshlist,fac,max\[Sigma]=.6,min\[Sigma]=.2},

fac=2m/3;
{idx,tloc}=QuotientRemainder[t,1/num];

\[Sigma]=Rescale[tloc,{0,1},{corrfac min\[Sigma],max\[Sigma]}];
\[Sigma]=Clip[\[Sigma],{min\[Sigma],max\[Sigma]}];

{x0,y0}=evollist[[Clip[idx+1,{1,Length[evollist]}]]];

meshlist={{Automatic,Automatic,Automatic},{Automatic,Automatic,Automatic},{Automatic,Automatic,Automatic}};
If[tloc<0.03,meshlist=ReplacePart[meshlist,{y0+2,x0+2}->Red]];

plot=Plot3D[1/(2\[Pi] \[Sigma]^2)Exp[-((x-fac x0)^2+(y-fac y0)^2)/(2\[Sigma]^2)],{x,-m,m},{y,-m,m},
Filling->If[\[Sigma]>1,Bottom,None],PlotRange->{{-m,m},{-m,m},zrange},Axes->False,Boxed->False,
Mesh->2,MeshFunctions->{#1&,#2&},MeshShading->meshlist,
PlotPoints->30,ViewCenter->{{0.5,.5,.5},{0.5,0.6}}];
plot=First@Cases[plot,_GraphicsComplex,Infinity,1];
plot={plot,Translate[indicatorMeas[t,num],{-m-.3,0,0}]};

If[tloc<0.03&&idx!=0,{plot,meas},{plot}]
]


(* ::Input::Initialization:: *)
animation[ti_]:=Module[{elist1,elist2,elist3,t=Clip[ti,{0,0.99}],grid},

elist1={{0,0}};
elist2={{0,0},{0,1},{0,1},{1,1},{1,0}};
elist3=Table[{0,0},21];

grid=Show[
Graphics3D[plot[t,elist1]],
Graphics3D[Translate[plot[t,elist2],{5,0,0}]],
Graphics3D[Translate[plot[t,elist3],{10,0,0}]],
Boxed->False,PlotRange->{{-2.3,12},{-2,2},zrange},BoxRatios->{3,1,.5}
];

Graphics[{Inset[grid,ImageScaled[{0.5,0.62}],ImageScaled[{0.5,0.5}],2]},PlotRange->{{-1,1},.5{-1,1}},ImageSize->800]
];


(* ::Chapter:: *)
(*Generation and export*)


(* ::Input:: *)
(*(*Manipulate[animation[t],{{t,0.808},0,1}]*)*)


(* ::Input:: *)
(*numOfFrames=130;*)
(*rasterSize=700;*)
(*{time,frames}=AbsoluteTiming[ParallelMap[Rasterize[#,RasterSize->rasterSize]&,Table[animation[t],{t,Subdivide[numOfFrames-1]}]]];*)
(*Print["The calculation took ",time/60.," minutes."];*)


(* ::Input:: *)
(*(*Echo[numOfFrames Times@@ImageDimensions[frames[[1]]]]\[LessEqual]100*^6*)*)


(* ::Input:: *)
(*(*ListAnimate[frames,AnimationRate\[Rule]5]*)*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)
(*Export["zeno_anim.gif",frames,AnimationRepetitions->Infinity]*)
(*FileSize[%]*)


(* ::Input:: *)
(*(*SystemOpen[%%]*)*)
