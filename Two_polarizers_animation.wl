(* ::Package:: *)

(* ::Title:: *)
(*Two polarizers*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Two_polarizers_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Two_polarizers_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Parameters*)


(* ::Input::Initialization:: *)
innerrad=0.8;
outerthick=.3;
innerthick=.2;
dist=2;



(* ::Input::Initialization:: *)
black=GrayLevel[0.3];



(* ::Input::Initialization:: *)
fontFamily="Times New Roman";
fontSize=25;



(* ::Input::Initialization:: *)
ang1=0.619;
ang2=0.772;
ang12=0.843;



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



(* ::Input::Initialization:: *)
arc[ang_]:=Module[{c=1,ampl=1.5,aux},
If[ang==0,Return[{}]];
aux=ParametricPlot3D[{ampl Cos[c t],ampl Sin[c t],0},{t,0,-ang}];
aux=First@Cases[InputForm[aux],_Line,Infinity];
If[ang>25Degree,Arrow[Tube[Reverse@First@aux,.02]],Tube[aux,.02]]
];



(* ::Input::Initialization:: *)
reticule[ang_]:=With[{len=1.8},
{black,
Tube[Line[{{1,0,0},{len,0,0}}],.03],
Tube[Line[{{Cos[ang],-Sin[ang],0},len{Cos[ang],-Sin[ang],0}}],.03],
Arrowheads[.04],arc[ang],
Text[Style[ToString[NumberForm[N@ang/Degree,{3,1}]]<>"\[Degree]",fontSize,Black,Background->GrayLevel[1,.8],FontFamily->fontFamily],1.7{Cos[ang/2],-Sin[ang/2],0},{-.5,-.5}]
}
];



(* ::Section:: *)
(*Waves*)


(* ::Input::Initialization:: *)
waveCircular[ampl_,phase_,length_:dist]:=Module[{c=2\[Pi] /dist 5,aux},
aux=ParametricPlot3D[{ampl Sin[c t-phase],ampl Cos[c t-phase],t},{t,0,length}];
aux=First@Cases[InputForm[aux],_Line,Infinity];
{Red,Tube[aux,.05]}
];



(* ::Input::Initialization:: *)
wave[ampl_,phase_,length_:dist]:=Module[{c=2\[Pi] /dist 5,aux},
aux=ParametricPlot3D[{ampl Sin[c t-phase],0,t},{t,0,length}];
aux=First@Cases[InputForm[aux],_Line,Infinity];
{Red,Tube[aux,.05]}
];



(* ::Section:: *)
(*Time evolution*)


(* ::Input::Initialization:: *)
evolAngleRezoom=Interpolation[{{0,ang1},{.5,ang12},{1,ang2}},InterpolationOrder->2];



(* ::Input:: *)
(*(*Plot[evolAngleRezoom[x],{x,0,1}]*)*)


(* ::Input::Initialization:: *)
timeEvolution[t_]:=Module[{ang,pt,ctr,len,ypos,rot,lena,
t1=0.1,t2=0.3,t3=0.5,t4=0.7,t5=0.9,ypos1=-2.1,ypos2=0,rot1=0,rot2=90Degree,pt1={2,4,4},pt2={2,2,4}+{0.18,0,2dist},ctr1={0,0,0.3},ctr2={0.18,0,2dist},len1=dist,len2=2dist-outerthick,len3=3dist,len4=2dist-innerthick-0.01,lena1=0,lena2=dist
},

Which[
t<t1,{ang1,pt1,ctr1,len1,ypos1,rot1,lena1},

t1<=t<t2,{
evolAngleRezoom[Rescale[t,{t1,t2},{0,1}]],
pt1+Rescale[t,{t1,t2},{0,1}](pt2-pt1),
ctr1+Rescale[t,{t1,t2},{0,1}](ctr2-ctr1),
Rescale[t,{t1,t2},{len1,len2}],
ypos1,
rot1,
lena1
},

t2<=t<t3,{ang2,pt2,ctr2,len2,Rescale[t,{t2,t3},{ypos1,ypos2}],rot1,lena1},

t3<=t<t4,{ang2,pt2,ctr2,Rescale[t,{t3,t4},{len2,len3}],ypos2,rot1,lena1},

t4<=t<t5,{ang2,pt2,ctr2,len4,ypos2,Rescale[t,{t4,t5},{rot1,rot2}],lena2},

t5<=t,{ang2,pt2,ctr2,len4,ypos2,rot2,lena2}
]

]



(* ::Section:: *)
(*Scene*)


(* ::Input::Initialization:: *)
scene[t_]:=Module[{ang,pt,ctr,len,ypos,rot,lena},

{ang,pt,ctr,len,ypos,rot,lena}=timeEvolution[t];
Graphics3D[{
{
filter,
{Red,EdgeForm[None],
{Opacity[.5],Cylinder[{{0,0,-dist},{0,0,-1.01innerthick/2}},.6innerrad]},
{Opacity[0.25],Cylinder[{{0,0,1.01innerthick/2},{0,0,len}},.6innerrad]}
},
Translate[waveCircular[.55innerrad ,-5t],{0,0,-dist}],
Translate[wave[1/Sqrt[2] .55innerrad ,-5t,len],{0,0,0}]
},
{
Translate[reticule[rot],{0,ypos,2dist}],
Translate[Rotate[filter,-rot,{0,0,1}],{0,ypos,2dist}],
If[lena!=0,{Red,EdgeForm[None],
Opacity[0.25Cos[rot]^2],Cylinder[{{0,0,2dist+1.01innerthick/2},{0,0,2dist+lena}},.6innerrad],
Opacity[Cos[rot]],Translate[Rotate[wave[1/Sqrt[2] .55innerrad Cos[rot],-5t,len/2],-rot,{0,0,1}],{0,0,2dist}]
},{}]

}
},
Boxed->False,Lighting->"Neutral",
PlotRange->{2.1{-.6,1},2{-2,0.6},1.05dist{-1,4}},
ViewVector->{pt,ctr},ViewAngle->ang,ViewVertical->{1,0,0}
]]



(* ::Section:: *)
(*Export*)


(* ::Input:: *)
(*(*Manipulate[scene[t],{{t,.878},0,1,Appearance\[Rule]"Open"}]*)*)


(* ::Input:: *)
(*{time,frames}=AbsoluteTiming[ParallelTable[Rasterize[scene[t],RasterSize->500],{t,0,1,.01}]];*)
(*time*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)
(*Export["scene.gif",frames,AnimationRepetitions->Infinity,"DisplayDurations"->.1]*)


(* ::Input:: *)
(*SystemOpen[%]*)
