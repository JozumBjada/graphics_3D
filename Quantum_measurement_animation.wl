(* ::Package:: *)

(* ::Title:: *)
(*Quantum measurement*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Quantum_measurement_animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Quantum_measurement_animation.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section::Closed:: *)
(*Colors*)


(* ::Input::Initialization:: *)
gray=GrayLevel[0.75];
lightBlue=Lighter[Blue,.5];


(* ::Section::Closed:: *)
(*Particle*)


(* ::Input::Initialization:: *)
stretch[pt1_,pt2_,rad_:1]:=Module[{len=Norm[pt2-pt1],tube,w0,coef},
If[len==0,Return[{}]];
{w0,coef}={rad/2,2 rad/len^2};
tube=RevolutionPlot3D[w0+coef x^2,{x,-len/2,len/2},RevolutionAxis->{1, 0, 0},Mesh->None];
tube=First@Cases[InputForm[tube],_GraphicsComplex,Infinity,1];
tube=tube/.{(Lighting->_):>Nothing,_RGBColor->Nothing,_Specularity->Nothing};
tube=Translate[tube,{len/2,0,0}];
tube=Rotate[tube,{{1,0,0},pt2-pt1}];
tube=Translate[tube,pt1];
tube
]



(* ::Input::Initialization:: *)
trajectory[r_,pt1_,pt2_,pt3_]:=Piecewise[{{pt1+r/(1/3) ((pt2-pt1)/2),0<=r<1/3},{BSplineFunction[{(pt2+pt1)/2,pt2,(pt2+pt3)/2}][Rescale[r,{1/3,2/3},{0,1}]],1/3<=r<2/3},{(pt2+pt3)/2+(r-2/3)/(1/3) ((pt3-pt2)/2),2/3<=r<=1}}]
trajectoryUp[r_]:=trajectory[r,{0,0,0},{8,0,0},{16,0,8}]
trajectoryDown[r_]:=trajectory[r,{0,0,0},{8,0,0},{16,0,-8}]



(* ::Input::Initialization:: *)
subsceneParticle[r_,pathup_,sup_]:=Which[
r>.95,{},
r>.85,{lightBlue,Ball[If[pathup,trajectoryUp,trajectoryDown][r],.9]},
sup,{
{lightBlue,Opacity[.5],Ball[trajectoryUp[r],.9],Ball[trajectoryDown[r],.9]},
{Blue,Opacity[.5],stretch[trajectoryUp[r],trajectoryDown[r],.9]}
},
True,{lightBlue,Ball[If[pathup,trajectoryUp,trajectoryDown][r],.9]}
]



(* ::Section::Closed:: *)
(*Detectors*)


(* ::Input::Initialization:: *)
detector=RevolutionPlot3D[.2x^2 Exp[x],{x,0,1.65},BoxRatios->1,Mesh->False,PlotStyle->Thickness[.2]];
detector=First@Cases[InputForm[detector],_GraphicsComplex,Infinity,1];
detector=detector/.{(Lighting->_):>Nothing,_RGBColor->Nothing,_Specularity->Nothing};



(* ::Input::Initialization:: *)
Module[{col1=gray,col2=gray,ang=35,dist=17,vert=8},
detector1=Translate[Rotate[detector,(180+90-ang)Degree,{0,1,0}],{dist,0,vert}];
detector2=Translate[Rotate[detector,(-90+ang)Degree,{0,1,0}],{{dist,0,-vert}}];
]



(* ::Input::Initialization:: *)
subsceneDetectors[r_,partup_:True]:=Module[{col1=gray,col2=gray},
If[r>.9,If[partup,col1=Orange,col2=Orange]];
{{col1,detector1},{col2,detector2}}
]



(* ::Section::Closed:: *)
(*Magnet*)


(* ::Input::Initialization:: *)
magnets=Module[{vertsize=5,mag,dist=5,pts},
pts={{0,0},{1,0},{1.5,.5},{1,1},{0,1}};
pts=Join[{##,0}&@@@pts,{##,vertsize}&@@@pts];
mag={EdgeForm[None],Polyhedron[pts,{{1,2,3,4,5},{6,7,8,9,10},{1,2,7,6},{1,6,10,5},{2,3,8,7},{3,4,9,8},{4,5,10,9}}]};
mag=Scale[Translate[Rotate[mag,\[Pi]/2,{0,0,1}],{.5,-0.75,-vertsize/2}],1.5];
Rotate[{Translate[mag,{dist,-4,0}],Translate[Rotate[mag,\[Pi],{0,0,1}],{dist,4,0}]},\[Pi]/2,{1,0,0}]
];



(* ::Input::Initialization:: *)
subsceneMagnets[r_]:={EdgeForm[None],If[.2<=r<=.4,Red,gray],magnets}



(* ::Section::Closed:: *)
(*Labels*)


(* ::Input::Initialization:: *)
arrow=Graphics3D[{Blue,Arrowheads[0.5],Arrow@Tube[{{0,0,-1},{0,0,1}},0.12]},Boxed->False,ViewPoint->{5,0,0},PlotRange->{0.5{-1,1},0.5{-1,1},1.1{-1,1}}];
arrowOriented[x_,dir_]:=Inset[arrow,ImageScaled[{x,0.15}],ImageScaled[{0.5,0.5}],.2,dir]



(* ::Input::Initialization:: *)
labelsUpDown[r_,c1_,c2_,up_]:=Module[{fontSize=30},
Which[
r<c1,arrowOriented[.5,{0,If[up,1,-1]}],
r<c2,arrowOriented[Rescale[r,{c1,c2},{0.5,.3}],{0,If[up,1,-1]}],
True,{
arrowOriented[.3,{0,If[up,1,-1]}],
Text[Style["\[Implies]",fontSize],ImageScaled[{0.5,.15}]],
Text[Style[Subscript["D",If[up,"\[LeftArrow]","\[Rule]"]],Orange,fontSize],ImageScaled[{0.67,.15}],{0,0}]
}
]
]



(* ::Input::Initialization:: *)
labelsSup[r_,c1_,c2_,c3_,c4_]:=Module[{fontSize=30},
Which[
r<c1,arrowOriented[.5,{1,0}],
r<c2,arrowOriented[Rescale[r,{c1,c2},{0.5,.22}],{1,0}],
r<c3,{
arrowOriented[.22,{1,0}],
Text[Style["=",fontSize],ImageScaled[{0.32,.15}]],
arrowOriented[.45,{0,1}],
Text[Style["+",fontSize],ImageScaled[{0.6,.15}]],
arrowOriented[.75,{0,-1}]
},
r<c4,{
arrowOriented[Rescale[r,{c3,c4},{0.45,.2}],{0,1}],
	Text[Style["+",fontSize],ImageScaled[{Rescale[r,{c3,c4},{0.6,.35}],.15}]],
arrowOriented[Rescale[r,{c3,c4},{0.75,.5}],{0,-1}]
},
True,{
arrowOriented[.2,{0,1}],
	Text[Style["+",fontSize],ImageScaled[{.35,.15}]],
arrowOriented[.5,{0,-1}],
	Text[Style["\[Implies]",fontSize],ImageScaled[{.66,.15}]],
	Text[Style[Subscript["D","?"],Orange,fontSize],ImageScaled[{0.799,.15}],{0,0}]
	}
]
]



(* ::Section::Closed:: *)
(*Animation*)


(* ::Input::Initialization:: *)
animation[r_]:=Module[{state,subr,rup,rdown,epilog,c1=0.3,c2=0.5,sc1=.2,sc2=.4,sc3=.6,sc4=.8,fontSize=30,partup,sup},

(*choose state and rescale the time parameter*)
{rup,rdown}={2,4}/7.;
{subr,state}=Which[
r<rup,{Rescale[r,{0,rup},{0,1}],"Up"},
r<rdown,{Rescale[r,{rup,rdown},{0,1}],"Down"},
True,{Rescale[r,{rdown,1},{0,1}],"Sup"}
];
If[r==1,subr=1];

(*choose parameters depending on the state*)
{epilog,sup,partup}=Switch[state,
"Up",{labelsUpDown[subr,c1,c2,True],False,True},
"Down",{labelsUpDown[subr,c1,c2,False],False,False},
"Sup",{labelsSup[subr,sc1,sc2,sc3,sc4],True,True}
];
epilog={epilog,
Text[Style[Subscript["D","\[LeftArrow]"],Orange,fontSize],ImageScaled@{.25,.92}],
Text[Style[Subscript["D","\[Rule]"],Orange,fontSize],ImageScaled@{.78,.92}]
};

(*create graphics*)
Graphics3D[
(*scene*)
{subsceneParticle[subr,partup,sup],subsceneDetectors[subr,partup],subsceneMagnets[subr]},

(*options*)
Epilog->epilog,PlotRange->{{-1,20},5.5{-1,1},10{-1,1}},SphericalRegion->True,Lighting->"Neutral",Boxed->False,
ViewPoint->{-2.31`,-2.47`,-0.06`},ViewAngle->20Degree,ViewCenter->{0.3, 0.5, 0.5},ViewVertical->{1,0,0}
]
]



(* ::Section:: *)
(*Preview*)


(* ::Input:: *)
(*Manipulate[animation[r],{r,0,1,Appearance->"Open"}]*)


(* ::Section:: *)
(*Export*)


(* ::Input:: *)
(*{time,frames}=AbsoluteTiming[ParallelTable[Rasterize[animation[r],ImageSize->350],{r,0,1,.01}]];*)
(*Print@time;*)
(*Export["anim2.gif",frames,AnimationRepetitions->Infinity,"DisplayDurations"->.18]*)
