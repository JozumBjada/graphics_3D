(* ::Package:: *)

(* ::Title:: *)
(*Q-Plate*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "Q-plate_for_OAM_1.svg"*)
(*https://commons.wikimedia.org/wiki/File:Q-plate_for_OAM_1.svg*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Text:: *)
(*Description: q-plate with q = 1/2*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
curve[a_,y_]:=-(1/2) E^(-2 a) (E^(4 a)-y^2)
avals=(1/2. Log[-2#])&/@{-1.5,-1.4,-1.3,-1.2,-1.1,-1.,-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,-4*^-2,-8*^-3,-1*^-5};
plot=Plot[Evaluate@Flatten@Table[curve[a,y],{a,avals}],{y,-1,1},PlotStyle->Darker@LightBlue,PlotRange->{-1,1},RegionFunction->(#1^2+#2^2<=1&),AspectRatio->1,Axes->False];


(* ::Input::Initialization:: *)
lines=Cases[InputForm[plot],_Line,Infinity];
lines=Show@RegionProduct[DiscretizeGraphics[lines],MeshRegion[{{0},{.2}},Line[{1,2}]]];
surfaces=GraphicsComplex[lines[[1,1]],lines[[1,2,-1]]];


(* ::Input::Initialization:: *)
plottubes=With[{f=1/2 ArcTan[y,-x]},VectorPlot[{Cos[f],Sin[f]},{x,-1,1},{y,-1,1},VectorMarkers->None,RegionFunction->(#1^2+#2^2<=1&),Frame->None,PlotRangePadding->None,VectorStyle->Thick,VectorScale->0.02,VectorPoints->30]];
tubes=Cases[InputForm[plottubes],_Line,Infinity];
tubes=tubes/.{x_?NumericQ,y_?NumericQ}:>{x,y,0.1}/.Line[l_]:>Tube[l,0.005];


(* ::Input::Initialization:: *)
qplate=Graphics3D[{
Lighter@Blue,tubes,EdgeForm[],Opacity[.4],Rotate[surfaces,- \[Pi],{0,0,1}],Opacity[.2],EdgeForm[Blue],Cylinder[{{0,0,0},{0,0,0.2}},1.05]
},ImageSize->2000,Boxed->False,Method->{"CylinderPoints"->80},ViewPoint->{-0.871,-2.1271,2.4871},ViewVertical->{-0.23839,-0.073117,0.96841}
];


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)
(*Export["qplate.svg",gr]*)
