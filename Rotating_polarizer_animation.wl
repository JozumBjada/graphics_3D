(* ::Package:: *)

(* ::Title:: *)
(*Rotating polarizer*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Rotating_polarizer_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Rotating_polarizer_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Constants*)


(* ::Input::Initialization:: *)
innerrad=0.8;
outerthick=.3;
innerthick=.2;
dist=2;



(* ::Input::Initialization:: *)
black=GrayLevel[0.3];



(* ::Input::Initialization:: *)
fontFamily="Times New Roman"(*"Adobe Devanagari"*);
fontSize=25;



(* ::Section:: *)
(*Polarizer*)


(* ::Input::Initialization:: *)
rim=ChartElementData["CylindricalSector3D"][{{0,2Pi},{innerrad,1},{-outerthick/2,outerthick/2}},1];
 (*courtesy: suggestion by user 'kglr' at: https://mathematica.stackexchange.com/questions/153536/thick-annulus-ring-in-3d*)



(* ::Input::Initialization:: *)
coating=DensityPlot[y ,{x,-1,1},{y,-1,1},RegionFunction->(#1^2+#2^2<=1&),ColorFunction->(ColorData["Rainbow"][Mod[2#1,1]]&),PlotStyle->Opacity[.5],Frame->False];
coating=First@Cases[InputForm[coating],_GraphicsComplex,Infinity];
coating=Rasterize[Graphics@coating,ImageResolution->50,Background->None];
coating={Texture[coating],EdgeForm[None],Polygon[{{-1,-1,0},{1,-1,0},{1,1,0},{-1,1,0}},VertexTextureCoordinates->{{0,0},{1,0},{1,1},{0,1}}]};



(* ::Input::Initialization:: *)
filter={EdgeForm[None],
{Opacity[.5],LightBlue,Cylinder[{{0,0,-innerthick/2},{0,0,innerthick/2}},innerrad-.001]},
{coating},
{Gray,rim,Scale[rim,{1.03,1.03,.6}]},
{Darker[black],Cuboid[{innerrad,-0.02,0},{1.01,0.02,1.3outerthick/2}],Cuboid[{-innerrad,-0.02,0},{-1.01,0.02,1.3outerthick/2}]}
};



(* ::Section:: *)
(*Rest*)


(* ::Input::Initialization:: *)
arc[ang_]:=Module[{c=1,ampl=1.5,aux},
If[ang==0,Return[{}]];
aux=ParametricPlot3D[{ampl Cos[c t],ampl Sin[c t],0},{t,0,-ang}];
aux=First@Cases[InputForm[aux],_Line,Infinity];
If[ang>15Degree,Arrow[Tube[Reverse@First@aux,.02]],Tube[aux,.02]]
];



(* ::Input::Initialization:: *)
reticule[ang_]:={black,
Tube[Line[{{1,0,0},{2,0,0}}],.03],
Tube[Line[{{Cos[ang],-Sin[ang],0},2{Cos[ang],-Sin[ang],0}}],.03],
Arrowheads[.04],arc[ang],
Text[Style[ToString[NumberForm[N@ang/Degree,{3,1}]]<>"\[Degree]",fontSize,Black,Background->GrayLevel[1,.8],FontFamily->fontFamily],1.7{Cos[ang/2],-Sin[ang/2],0},{-.5,-.5}]
};



(* ::Input::Initialization:: *)
wave[ampl_,phase_]:=Module[{c=2\[Pi] /dist 5,aux},
aux=ParametricPlot3D[{ampl Sin[c t-phase],0,t},{t,0,dist}];
aux=First@Cases[InputForm[aux],_Line,Infinity];
{Red,Tube[aux,.05]}
];



(* ::Input::Initialization:: *)
label[ang_]:=Text[Style[Row[{"\!\(\*
StyleBox[\"I\",\nFontSlant->\"Italic\"]\) = ",ToString[NumberForm[Round[Cos[ang]^2,0.01],{3,2}],TraditionalForm]}],fontSize,FontFamily->fontFamily],ImageScaled@{0.7,0.85},{-1,0}]



(* ::Section:: *)
(*Scene*)


(* ::Input::Initialization:: *)
scene[ang_,phase_:0]:=Graphics3D[{
{(*rear wave*)
{Red,Opacity[.5],EdgeForm[None],Cylinder[{{0,0,-dist},{0,0,-1.01innerthick/2}},.6innerrad]},
Translate[wave[.55innerrad,phase],{{0,0,-dist}}]
},

(*polarizer*)
reticule[ang],
Rotate[{filter,{Opacity[Cos[ang]],wave[.55innerrad Cos[ang],phase]}},-ang,{0,0,1}],

{(*front wave*)
{Red,Opacity[0.5Cos[ang]^2],EdgeForm[None],Cylinder[{{0,0,1.01innerthick/2},{0,0,dist}},.6innerrad]},
{Opacity[Cos[ang]],Rotate[wave[.55innerrad Cos[ang],phase],-ang,{0,0,1}]}
}
},
(*intensity label*)
Epilog->label[ang],

(*options*)
Lighting->"Neutral",Boxed->False,PlotRange->{2.1{-.6,1},2{-1,.6},1.05dist{-1,1}},
ViewVertical->{1,0,0},ViewPoint->0.8{.5,1,1},ViewCenter->{{0.`,0.`,0.`},{0.36,0.4}},ViewAngle->0.83`
]



(* ::Input:: *)
(*Manipulate[scene[ang,phase],{ang,0,\[Pi]/2},{phase,0,2\[Pi]}]*)


(* ::Section:: *)
(*Export*)


(* ::Input::Initialization:: *)
angleTimeEvolution[t0_,t1_,t2_,t3_]:=Piecewise[{{0,#<t0},{Rescale[#,{t0,t1},{0,\[Pi]/4}],t0<=#<t1},{\[Pi]/4,t1<=#<t2},{Rescale[#,{t2,t3},{\[Pi]/4,\[Pi]/2}],t2<=#<t3},{\[Pi]/2,True}}]&
(*Plot[angleTimeEvolution[.2,.4,.6,.8][x],{x,0,1}]*)



(* ::Input:: *)
(*{time,frames}=AbsoluteTiming[ParallelTable[Rasterize[*)
(*scene[angleTimeEvolution[.2,.4,.6,.8][t],Rescale[t,{0,1},{0,4\[Pi]}]],*)
(*ImageResolution->100],{t,0,1,.02}]];*)
(*Print[time];*)


(* ::Input:: *)
(*file=Export["test6.gif",frames,AnimationRepetitions->Infinity,"DisplayDurations"->.3]*)


(* ::Input:: *)
(*FileSize[file]*)


(* ::Input:: *)
(*SystemOpen[file]*)
