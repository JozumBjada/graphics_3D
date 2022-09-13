(* ::Package:: *)

(* ::Title:: *)
(*Dove prisms*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "Rotated_Dove_prism.svg"*)
(*https://commons.wikimedia.org/wiki/File:Rotated_Dove_prism.svg*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
fontFamily="Times";
fontSize=64;


(* ::Input::Initialization:: *)
arc[ang_]:=Module[{c=1,ampl=2.5,aux},
If[ang==0,Return[{}]];
aux=ParametricPlot3D[{0,-ampl Cos[c t],ampl Sin[c t]},{t,0,-ang}];
aux=First@Cases[InputForm[aux],_Line,Infinity];
If[ang>15Degree,Arrow[Tube[Reverse@First@aux,.02]],Tube[aux,.02]]
];


(* ::Input::Initialization:: *)
arrow[ang_]:={Darker@Gray,
Tube[Line[{{0,0,0},{0,-3,0}}],.03],
Tube[Line[{{0,0,0},RotationMatrix[ang,{1,0,0}] . {0,-3,0}}],.03],
Arrowheads[.04],arc[ang],
Text[Style["\[Alpha]",fontSize,Black,FontFamily->fontFamily],RotationMatrix[ang/2,{1,0,0}] . {0,-3.5,0},{-.0,-0.3}]
};


(* ::Input::Initialization:: *)
dove=Module[{pts,pts0,pts1,d=2,len=8},
pts={{-len/2,-d/2},{len/2,-d/2},{len/2-d,d/2},{-len/2+d,d/2}};
pts0={##,-d/2}&@@@pts;
pts1={##,d/2}&@@@pts;
Polyhedron[Join[pts0,pts1],{Reverse@{1,2,3,4},Reverse@{5,6,7,8},{1,2,6,5},{3,4,8,7},{2,6,7,3},{4,8,5,1}}]
];


(* ::Input::Initialization:: *)
ray=Tube[{{-5,0,0},{-2.9,0,0},{0,-.8,0},{2.9,0,0},{5,0,0}},.4];


(* ::Input::Initialization:: *)
grDove=With[{ang=30Degree},
Graphics3D[{
Translate[Scale[arrow[ang],{1,-1,-1}],{1,3.4,0.2}],
{Lighting->{{"Spot",White,{3.2,8.3,12.1},{100Degree,20}},{"Spot",White,{11.9,-9.7,18.3},{100Degree,20}},{"Directional",GrayLevel[0.8],{10,10,5}}},
Lighter[Blue,.6],Opacity[.7],Rotate[dove,ang,{1,0,0}],Translate[dove,{0,0,5}]},
{CapForm["Square"],Opacity[.5,Orange],Glow[Red],Rotate[ray,ang,{1,0,0}],Translate[ray,{0,0,5}]},
MapThread[Text[Style[ToString[#1,TraditionalForm],fontSize,FontFamily->"Times",Black],#2]&,{{"\!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\[ImaginaryI]\\\ \\\ l \((\\\ \[CurlyPhi]\[ThinSpace] + \[ThinSpace]2  \[Alpha])\)\)]\)","\!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\(-\[ImaginaryI]\)\\\ l\\\ \[CurlyPhi]\)]\)","\!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\(-\[ImaginaryI]\)\\\ l\\\ \[CurlyPhi]\)]\)","\!\(\*SuperscriptBox[\(\[ExponentialE]\), \(\[ImaginaryI]\\\ l\\\ \[CurlyPhi]\)]\)"},{{6.8,-.6,0},{-6,.8,0},{-6,.8,5},{6,-.8,5}}}]
},
Lighting->"Neutral",Boxed->False,ViewVertical->{0, 1, 0},PlotRange->All,ViewPoint->.6{3.5,1.29,3.248},ImageSize->2000
]
]


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*Export["grDoveEdge.svg",grDove]*)


(*Export["grDoveEdge.png",grDove,ImageResolution->200]*)
