(* ::Package:: *)

(* ::Title:: *)
(*Quantum measurement of polarization*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Quantum_measurement_of_polarization.gif"*)
(*https://commons.wikimedia.org/wiki/File:Quantum_measurement_of_polarization.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section::Closed:: *)
(*Constants*)


(* ::Input::Initialization:: *)
gray=GrayLevel[0.75];
partCol=Lighter[Blue,.5] (*Orange*);


(* ::Input::Initialization:: *)
hmax=4.4;


(* ::Section::Closed:: *)
(*Particle*)


(* ::Input::Initialization:: *)
getWave[off_,cutL_:-2\[Pi],cutH_:2\[Pi]]:=Module[{plot,line,line3D,rad=0.04,a=1*10^(-0.5),b=7,c=1.4\[Pi],sf=.2},
plot=Plot[sf Exp[-a (x/sf)^2]Sin[(b x+off)/sf],{x,sf Max[-c,cutL],sf Min[c,cutH]}];
(*line=Cases[InputForm[plot],_Line,Infinity,1];*)
line=InputForm[plot][[1,1,1,1,3,1,2]];
line3D=line[[1]]/.{a_?NumericQ,b_}:>{a,b,0};
Tube[line3D,rad]
]


(* ::Input::Initialization:: *)
particle[off_,cL_:-2\[Pi],cH_:2\[Pi]]:={partCol,Opacity[.5],Ball[{0,0,0},.3],Rotate[getWave[6\[Pi] off,cL,cH],\[Pi]/2,{0,0,1}]}


(* ::Section::Closed:: *)
(*PBS*)


(* ::Input::Initialization:: *)
beamsplitter=Module[{p1={0, 0, 0},p2={1, 0, 0},p3={0, 1, 0},p4={0, 0, 1},p5={1, 0, 1},p6={0, 1, 1},prism},
prism=Translate[Prism[{p1, p2, p3, p4, p5, p6}], {-.505, -.505, -.5}];
{
EdgeForm[None],Specularity[Lighter[Blue,0.2],200],
{
Opacity[.8, Lighter[Purple, .7]],FaceForm[Opacity[.8, Lighter[Blend[{Blue, Purple}, .9], .5]]],
prism
},
        {
Opacity[.9, Lighter[Purple, .7]],FaceForm[Opacity[.85, Lighter[Blend[{Blue, Purple}, .1], .5]]],
Rotate[prism, \[Pi], {0, 0, 1}]
}
        }
      ];


(* ::Section::Closed:: *)
(*Detectors*)


(* ::Input::Initialization:: *)
detector=RevolutionPlot3D[.2x^2 Exp[x],{x,0,1.65},BoxRatios->1,Mesh->False,PlotStyle->Thickness[.2]];
detector=First@Cases[InputForm[detector],_GraphicsComplex,Infinity,1];
detector=detector/.{(Lighting->_):>Nothing,_RGBColor->Nothing,_Specularity->Nothing};


(* ::Input::Initialization:: *)
Module[{col1=gray,col2=gray,ang=35,dist=17,vert=8},
detector1=Translate[#,{0,4,0}]&@Rotate[#,\[Pi]/2,{0,1,0}]&@Rotate[Scale[detector,0.3],\[Pi]/2,{1,0,0}];
detector2=Translate[#,{-3.8,0,0}]&@Rotate[#,\[Pi]/2,{0,0,1}]&@Rotate[#,\[Pi]/2,{0,1,0}]&@Rotate[Scale[detector,0.3],\[Pi]/2,{1,0,0}];
]


(* ::Input::Initialization:: *)
subsceneDetectors[r_,partup_:True]:=Module[{col1=gray,col2=gray},
If[r>.9,If[partup,col1=Orange,col2=Orange]];
{{col1,detector1},{col2,detector2}}
]


(* ::Section::Closed:: *)
(*Scenes*)


(* ::Input::Initialization:: *)
trajectoryH[r_]:=Module[{rcollapse=0.95},
Translate[#,{0,Rescale[r,{0,1},{-2,hmax-2}],0}]&@particle[r,-2\[Pi],If[r>rcollapse,\[Pi],2\[Pi]]]
]


(* ::Input::Initialization:: *)
trajectoryV[r_]:=Module[{rcollapse=0.95,rrefl=2/hmax,rdelta=0.1,dd,ang=\[Pi]/2},

If[r<rrefl,
dd=If[r>rrefl-rdelta,Rescale[r,{rrefl-2rdelta,rrefl},{0,3\[Pi]/2}],0];
Translate[#,{0,Rescale[r,{0,rrefl},{-2,0}],0}]&@Rotate[particle[r,-2\[Pi]+dd,2\[Pi]-dd],ang,{0,1,0}]
,
dd=If[r<rrefl+rdelta,Rescale[r,{rrefl,rrefl+2rdelta},{3\[Pi]/2,0}],0];

Translate[#,{Rescale[r,{rrefl,1},{0,-2}],0,0}]&@Rotate[Rotate[particle[r,-2\[Pi]+dd,2\[Pi]-dd+If[r>rcollapse,-\[Pi],0]],\[Pi]/2,{0,1,0}],\[Pi]/2,{0,0,1}]
]

]


(* ::Input::Initialization:: *)
trajectoryD[r_]:=Module[{rcollapse=0.9,rrefl=2/hmax,rdelta=0.1,dd,ang=\[Pi]/4},

If[r<rrefl,
dd=If[r>rrefl-rdelta,Rescale[r,{rrefl-2rdelta,rrefl},{0,3\[Pi]/2}],0];
Translate[#,{0,Rescale[r,{0,rrefl},{-2,0}],0}]&@Rotate[particle[r,-2\[Pi]+dd,2\[Pi]-dd],ang,{0,1,0}]
,
dd=If[r<rrefl+rdelta,Rescale[r,{rrefl,rrefl+2rdelta},{3\[Pi]/2,0}],0];

{
If[r>rcollapse,{},
Translate[#,{Rescale[r,{rrefl,1},{0,-2}],0,0}]&@Rotate[Rotate[particle[r,-2\[Pi]+dd,2\[Pi]-dd],\[Pi]/2,{0,1,0}],\[Pi]/2,{0,0,1}]
],
Translate[#,{0,Rescale[r,{rrefl,1},{0,hmax-2}],0}]&@particle[r,-2\[Pi]+dd,2\[Pi]-dd+If[r>rcollapse,-\[Pi],0]]
}
]

]


(* ::Input::Initialization:: *)
animation[r_]:=Module[{r1b=0.05,r1e=0.34,r2b=0.35,r2e=0.64,r3b=0.65,r3e=0.999,mode,rloc,subsceneParticle,rightdet,opts,fontFamily=(*"Devanagari"*)"Times",fontSize=40},
{mode,rloc}=Piecewise[{
{{"H",Rescale[r,{r1b,r1e},{0,1}]},r1b<r<r1e},
{{"V",Rescale[r,{r2b,r2e},{0,1}]},r2b<r<r2e},
{{"D",Rescale[r,{r3b,r3e},{0,1}]},r3b<r<r3e}
},{"0",0}];

{rightdet,subsceneParticle}=Switch[mode,
"H",{True,trajectoryH},
"V",{False,trajectoryV},
"D",{True,trajectoryD},
"0",(rloc=0;{True,{}&})
];

opts={ViewVector->{{150,-200,150},{0,0,0}},ViewAngle->0.018,PlotRange->{{-3,1},{-4,4},{-1,1}},Lighting->"Neutral",Boxed->False};

Graphics[{
Inset[Graphics3D[{beamsplitter,subsceneDetectors[rloc,rightdet],subsceneParticle[rloc]},Sequence@@opts],ImageScaled[{.5,.5}],ImageScaled[{.5,.5}],2]
},PlotRange->{{-1,1},.7{-1,1}},
Epilog->If[mode==="0",{},Text[Style[mode,fontSize,partCol,Bold,FontFamily->fontFamily],ImageScaled[{.1,.1}]]]
]
]


(* ::Section::Closed:: *)
(*Preview*)


(* ::Input:: *)
(*Manipulate[animation[r],{r,0,1,Appearance->"Open"}]*)


(* ::Section:: *)
(*Export*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*{time,frames}=AbsoluteTiming[ParallelTable[Rasterize[animation[r],ImageSize->350],{r,0,1,.007}]];*)
(*Print@time;*)
(*Export["anim.gif",frames,AnimationRepetitions->Infinity,"DisplayDurations"->.18]*)


(* ::Input:: *)
(*SystemOpen[%]*)
