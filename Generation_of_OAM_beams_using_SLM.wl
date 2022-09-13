(* ::Package:: *)

(* ::Title:: *)
(*OAM beams created by SLM*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Generation_of_OAM_beams_using_SLM.gif"*)
(*https://commons.wikimedia.org/wiki/File:Generation_of_OAM_beams_using_SLM.gif*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Text:: *)
(*Description: Animation demonstrating the generation of orbital angular momentum (OAM) beams using spatial light modulator (SLM)*)


(* ::Chapter::Closed:: *)
(*Auxiliary routines and constants*)


(* ::Input::Initialization:: *)
{pt1,pt2,pt3,pt4}={{-1,-1,0},{0,0,0},{.5,-1.5,0},1.12{1,-1,0}};
{rad,speed,finalStageIdx}={0.2,0.1,16};
arrowFun[pts_,col_:Orange]:={Thickness[0.005],Arrowheads[0.03],col,Arrow[BezierCurve[pts]]}
fadeFun[gr_,t_]:=gr/.{col_:>Blend[{col,White},t]/;ColorQ[col],img_Image:>Blend[{RemoveAlphaChannel[img,White],ConstantImage[White,ImageDimensions[img]]},t]}


(* ::Chapter:: *)
(*3D elements*)


(* ::Section::Closed:: *)
(*SLM*)


(* ::Input::Initialization:: *)
frame3D[w_,h_,d_,scale_]:=Module[{pts,coords,reg},
pts={##,-d/2}&@@@({{-w,-h},{w,-h},{w,h},{-w,h}}/2);
pts=Join[pts,TranslationTransform[{0,0,d}][pts],ScalingTransform[{scale,scale,1}][pts],TranslationTransform[{0,0,d}]@ScalingTransform[{.8,.8,1}][pts]];coords={{1,2,6,5},{2,3,7,6},{3,4,8,7},{4,1,5,8},{1,2,3,4},{5,6,7,8}};
reg=RegionDifference[Polyhedron[pts,coords],Polyhedron[pts,Map[Plus[#,8]&,coords,{2}]]];
reg
];


(* ::Input::Initialization:: *)
getSLM[tex_,w_:1,h_:.9,d_:.05,scale_:.8]:=Module[{pts},
pts={##,0}&@@@((1+scale)/2{{-w,-h},{w,-h},{w,h},{-w,h}}/2);
Rotate[#,\[Pi]/2,{1,0,0}]&@{Gray,EdgeForm[],frame3D[w,h,d,scale],Texture[Rotate[tex,-\[Pi]/2]],Polygon[pts,VertexTextureCoordinates->RotateRight[{{0,0},{1,0},{1,1},{0,1}}]]}
];


(* ::Section::Closed:: *)
(*Beams*)


(* ::Input::Initialization:: *)
tubeBeamFun[pt_,opacity_:.5,pt2_:pt2]:={CapForm["Square"],Glow[RGBColor[1, 0, 0]],RGBColor[1, 0.5, 0],JoinForm["Miter"],Opacity[opacity],Tube[{pt2,pt},rad]}


(* ::Input::Initialization:: *)
getHelix[k_:1]:=getHelix[k]=Module[{plot,helix,rad=rad},
plot=ParametricPlot3D[Evaluate[Table[{r Cos[2\[Pi]/k t+j 2\[Pi]/k],r Sin[2\[Pi] /k t+j 2\[Pi]/k],t},{j,0,Abs[k]-1}]],{t,0,1},{r,0,1},Mesh->None,PlotStyle->Orange,PlotPoints->If[k==1||k==-1,25,Automatic]];
plot=First[Cases[InputForm[plot],_GraphicsComplex,Infinity,1]];
helix=Scale[Translate[plot,Table[{0,0,0.8+i},{i,7}]],{0.8rad,0.8rad,0.2},{0,0,0}];
helix
]


(* ::Input::Initialization:: *)
helicalWavefrontFun[k_,t_,initpt_,finpt_]:=Module[{wfs,speed=2},
wfs=Rotate[getHelix[k],-Sign[k]speed t,{0,0,1}];
Translate[#,pt2]&@Rotate[wfs,{{0,0,1},finpt-initpt}]
]


(* ::Input::Initialization:: *)
(*disk=ResourceFunction["Disk3D"][{0,0,0},0.8rad,{{1,0,0},{0,0,1}}];*)
disk=BSplineSurface[{{{-0.16,0.,0.},{-0.16,0.,-0.16},{0.,0.,-0.16}},{{-0.16,0.,0.16},{0.0178,0.,0.},{0.16,0.,-0.16}},{{0.,0.,0.16},{0.16,0.,0.16},{0.16,0.,0.}}},SplineKnots->{{0,0,0,1,1,2},{0,0,0,1,1,2}},SplineWeights->{{1,1/Sqrt[2],1},{1/Sqrt[2],1,1/Sqrt[2]},{1,1/Sqrt[2],1}}];


(* ::Input::Initialization:: *)
flatWavefrontFun[t_,initpt_,finpt_,offset_:0,opacity_:.5,lenvec_:1]:=Module[{wfs,len=1.5,step,offsets,wfnum=8},
step=(*len*)1.1/wfnum;
If[lenvec==0,Return[{}]];
offsets=(offset+Mod[speed t+#,len])&/@Range[0,lenvec len,step];
wfs=Translate[disk,{0,#,0}&/@offsets];
{Orange,EdgeForm[Opacity[Rescale[opacity ,{0,.5},{0,1}]0.9,Red]],Opacity[opacity],Translate[#,initpt]&@Rotate[wfs,{{0,1,0},finpt-initpt}]}
]


(* ::Input::Initialization:: *)
ptrot[tloc_]:=RotationTransform[Rescale[tloc,{0,1},{0,-VectorAngle[pt3,pt4]}],{pt3,pt4},pt2][pt4];
flatFrontFun[tglob_,op_:0.8]:=flatWavefrontFun[tglob,pt2,pt4,0.4,op];
beamsFun[k_][tglob_]:={tubeBeamFun[pt4],flatFrontFun[tglob],If[k=!=None,helicalWavefrontFun[k,tglob,pt2,pt4],Nothing]};
beamsFun2[k_][tglob_]:={tubeBeamFun[pt4,.2],tubeBeamFun[pt3],flatWavefrontFun[tglob,pt2,pt4,0.45,.2],helicalWavefrontFun[k,tglob,pt2,pt3]};


(* ::Input::Initialization:: *)
getBeams[stage_,tloc_,tglob_]:=Module[{list,speed=speed,incbeam,incwavefronts},

list={
{tubeBeamFun[pt2+Clip[2tloc-1,{0,1}](pt4-pt2)],flatWavefrontFun[tglob,pt2,pt4,0.4,0.5,Clip[2tloc-1,{0,1}]]},
beamsFun[None][tglob],
beamsFun[1][tglob],
beamsFun[2][tglob],
beamsFun[-2][tglob],
beamsFun[-1][tglob],
beamsFun[None][tglob],
{tubeBeamFun[pt4,.2],tubeBeamFun[ptrot[tloc]],flatWavefrontFun[tglob,pt2,ptrot[tloc],0.35,0.8],flatFrontFun[tglob,0.2]},
{tubeBeamFun[pt4,.2],tubeBeamFun[pt3],flatWavefrontFun[tglob,pt2,pt3,0.35,0.8],flatFrontFun[tglob,0.2]},
beamsFun2[1][tglob],
beamsFun2[1][tglob],
beamsFun2[2][tglob],
beamsFun2[-2][tglob],
beamsFun2[-1][tglob],
beamsFun2[1][tglob],
beamsFun2[1][tglob]
};
incbeam=tubeBeamFun[If[stage==1,pt1+Clip[2tloc,{0,1}](pt2-pt1),pt2],.5,pt1];
incwavefronts=flatWavefrontFun[tglob,pt1,pt2,0,0.5,If[stage==1,Clip[2tloc,{0,1}],1]];
Join[{incbeam,incwavefronts},list[[stage]]]
]


(* ::Chapter:: *)
(*2D elements*)


(* ::Section::Closed:: *)
(*Side slide*)


(* ::Input::Initialization:: *)
slideAsideFun[times_,funs_,def_:{}]:=Module[{aux,x},
aux=MapThread[{#1[Rescale[x,{#2,#3},{0,1}]],x<#3}&,{funs,times,Append[Rest[times],1]}];
With[{p=Piecewise[aux,def]/.x->#},p&]
];


(* ::Input::Initialization:: *)
slidePics[times_,pics_,ipos_]:=Module[{aux,x,funs,u,pos=Identity[ipos],def},

funs=MapThread[Function[{u},{Translate[#,#2+u(#3-#2)]}]&,{pics,pos,Append[Rest[pos],Last[pos]]}];
def=Translate[Last[pics],Last[pos]];
slideAsideFun[times,funs,def]
];


(* ::Input::Initialization:: *)
slidePicsAccum[itimes_,pauses_,ipics_,ipos_]:=Module[{aux,x,funs,u,pos,def,pics=FoldList[Append,ipics],times},

times=Riffle[itimes,itimes+pauses];
pics=Riffle[pics,pics];
pos=Riffle[ipos,ipos];

If[Last[pauses]==0,{times,pics,pos}=Most/@{times,pics,pos}];
slidePics[times,pics,pos]
];


(* ::Input::Initialization:: *)
slideAsideTwo[itimes_,pauses_,tfade1_,tfade2_,k1_,k2_,k3_,finpos_:-1]:=Module[{slideTwo,gr1,grMid,gr2,gr3},

gr1=If[k1===None,{},texFun[texSmoothFun[k1],{0,0}]];
grMid=Text[Style["\[Rule]",40,FontColor->Black],{-finpos/2,0}];
gr2=texFun[texSmoothFun[k2],{-finpos,0}];
gr3=texFun[texSmoothFun[k3],{-finpos,0}];

slideTwo=slidePicsAccum[itimes,pauses,{{Translate[#,{finpos,0}]&@gr2},{grMid,gr3}},{{.25,1.5},{.25,1.5}+{finpos,0}}];

Piecewise[{
{Translate[#,{.25,1.5}+{finpos,0}]&@{fadeFun[{gr1,If[k1===None,{},grMid]},Rescale[#,{0,tfade1},{0,1}]],gr2},#<tfade1},
{texFun[texSmoothFun[k2],{.25,1.5}],#<tfade2},
{slideTwo[Rescale[#,{tfade2,1},{0,1}]],True}
}]&
]


(* ::Section::Closed:: *)
(*Textures*)


(* ::Input::Initialization:: *)
texFun[tex_,pos_,pars___]:={Texture[tex],pars,EdgeForm[],Polygon[TranslationTransform[pos][0.6{{-1,-1},{1,-1},{1,1},{-1,1}}/2],VertexTextureCoordinates->{{0,0},{1,0},{1,1},{0,1}}]}


(* ::Input::Initialization:: *)
getHologram[charge_,grating_:0,disk_:True,colorFun_:GrayLevel]:=getHologram[charge,grating,disk,colorFun]=Module[{slope,lim=2.5,plotPoints=70,imgSize=200},
slope=Rescale[grating,{0,1},{0,12}];
If[charge==0&&grating==0&&Not@disk,
ConstantImage[Gray,{imgSize,imgSize}]
,
DensityPlot[Evaluate[Mod[slope y+Arg[Exp[-I charge ArcTan[-x,y]]],2\[Pi],-\[Pi]]],{y,-lim,lim},{x,-lim,lim},
Exclusions->(#1<=0&&#2==0&),PlotPoints->If[grating>0,2plotPoints,plotPoints],PlotRangePadding->None,
Frame->None,ColorFunction->colorFun,MaxRecursion->Automatic,RegionFunction->If[disk,(#1^2+#2^2<=lim^2&),True],ImageSize->imgSize]
]
];
orangeLevel=Blend[{Orange,Black},#]&;


(* ::Input::Initialization:: *)
slidingHolos=With[{opos={.25,1.5}},
slidePicsAccum[{0,0.5,0.95},{0.02,0.02,0},
{
{texFun[texGratingFun[1],opos]},
{Text[Style["+",40,FontColor->Black],opos+{0.5,0}],texFun[texSmoothFun[1],opos+{1,0}]},
{Text[Style["=",40,FontColor->Black],opos+{1.5,0}],texFun[texSLM[1],opos+{2,0}]}
}
,{{0,0},{-1,0},{-2,0}}]
];


(* ::Input::Initialization:: *)
sumHolo[k_]:=With[{opos={.25,1.5}+{-2,0}},
{
{texFun[texGratingFun[1],opos]},
{Text[Style["+",40,FontColor->Black],opos+{0.5,0}],texFun[texSmoothFun[k],opos+{1,0}]},
{Text[Style["=",40,FontColor->Black],opos+{1.5,0}],texFun[texSLM[k],opos+{2,0}]}
}
];


(* ::Input::Initialization:: *)
texSLM[k_]:=texSLM[k]=Rasterize[getHologram[k,1,False],Background->None]
texGratingFun[n_]:=texGratingFun[n]=Rasterize[getHologram[0,n,False],Background->None]
texSmoothFun[k_]:=texSmoothFun[k]=Image@getHologram[k,0,False];
texOrange[k_]:=texOrange[k]=Rasterize[getHologram[k,0,True,Blend[{Orange,Black},#]&],Background->None]


(* ::Input::Initialization:: *)
texOrangePlusGauss[k_]:=texOrangePlusGauss[k]=Rasterize[Graphics[{
Inset[texOrange[0],ImageScaled[{.6,.4}],ImageScaled[{1,1}/2],1.5],
Inset[texOrange[k],ImageScaled[{.4,.6}],ImageScaled[{1,1}/2],1.5]
}],Background->None]


(* ::Section::Closed:: *)
(*Labels*)


(* ::Input::Initialization:: *)
getTextures[stage_,t_]:=Module[{list},
list={texSmoothFun[0],texSmoothFun[0],texSmoothFun[1],texSmoothFun[2],texSmoothFun[-2],texSmoothFun[-1],texSmoothFun[0],texGratingFun[t],texGratingFun[1],texSLM[1],texSLM[1],texSLM[2],texSLM[-2],texSLM[-1],texSLM[1],texSLM[1]};
list[[stage]]
]


(* ::Input::Initialization:: *)
slide[k1_,k2_,k3_]:=slideAsideTwo[{0,.5},{0.02,0 0.02},.3,.85,k1,k2,k3];
textlab[text_]:=Text[Framed[Style[text,50,FontColor->Black,FontFamily->"Times"],FrameStyle->Black],{.8,1.5}];
textlab[text_,tt_]:=fadeFun[textlab[text],tt];
tor2[k_,addGauss_:False]:=texFun[If[addGauss,texOrangePlusGauss[k],texOrange[k]],{.85,-.9}];
tor3[k_]:=texFun[texOrange[k],{-0.5,-.9}];


(* ::Input::Initialization:: *)
getLabels[stage_,t_,tex_]:=Module[{list,
arr1=arrowFun[{{-1.917,0.638},{-1.817,0.825},{-1.53,0.845},{-1.383`,0.6583`}}],
arr3=arrowFun[{{-0.8044`,-0.61`},{-0.8489`,-0.3789`},{-0.6978`,-0.1389`},{-0.4933`,-0.1389`}}],
arr2=arrowFun[{{1.151`,-0.5878`},{1.302`,-0.4056`},{1.053`,-.2}}],
tf=texFun[tex,{.25,1.5},EdgeForm[{Thick,Black}]]},

list={
{arr1,tf},
{arr1,arr2,tor2[0],slide[None,0,1][t]},
{arr1,arr2,tor2[1,True],textlab["+1",t],slide[0,1,2][t]},
{arr1,arr2,tor2[2,True],textlab["+2",t],slide[1,2,-2][t]},
{arr1,arr2,tor2[-2,True],textlab["-2",t],slide[2,-2,-1][t]},
{arr1,arr2,tor2[-1,True],textlab["-1",t],slide[-2,-1,0][t]},
{arr1,arr2,tor2[0],tf},
{arr1,arr2,tor2[0],tf},
{arr1,arr2,arr3,tor2[0],tor3[0],slidingHolos[t]},
{arr1,arr2,arr3,tor2[0],tor3[1],sumHolo[1],textlab["+1"]},
{fadeFun[{arr1,arr2,arr3,tor2[0]},t],tor3[1],sumHolo[1],textlab["+1"]},
{sumHolo[2],textlab["+2"],tor3[2]},
{sumHolo[-2],textlab["-2"],tor3[-2]},
{sumHolo[-1],textlab["-1"],tor3[-1]},
{fadeFun[sumHolo[+1],t],tf,textlab["+1"],tor3[1]},
fadeFun[{tf,textlab["+1"],tor3[1]},t]
};
list=Join[{texFun[texOrange[0],{-2.1,.3}]},#]&/@list;

list[[stage]]
]


(* ::Chapter:: *)
(*Composition*)


(* ::Section::Closed:: *)
(*Scene*)


(* ::Input::Initialization:: *)
scene[stage_,t_,tglob_]:=Module[{incbeam,reflbeam,reflbeam2,wavefrontsIn,wavefronts1,wavefronts2,gr3D,tex,texOut,texOut2,gr,imgRes=50,img},

tex=getTextures[stage,t];
gr3D=Graphics3D[{getSLM[tex],getBeams[stage,t,tglob]},
Lighting->{{"Point",White,2{-1,-1,0}},{"Point",White,2{1,-1,0}},{"Point",White,2{0,0,1}}},Boxed->False,ViewVertical->{0,0,1},ViewVector->{{10,-17,8},{0,0,0}},PlotRange->{{-1.5,1.5},{-1.8,0.2},{-1,1}}
];
gr=Graphics[{Inset[gr3D,{.25,.5},ImageScaled[{1,1}/2],4],getLabels[stage,t,tex]},ImageSize->900,PlotRange->{{-2.5,1.5},{-1.5,1.9}}];

(*rasterization is done basically only because of the very last stage where the whole scene fades away, with Graphics is it more complicated than with Image*)
img=Rasterize[gr,ImageResolution->imgRes];
If[stage==finalStageIdx,Blend[{img,ConstantImage[White,ImageDimensions[img]]},t],img]
]


(* ::Input:: *)
(*(*Manipulate[scene[stage,t,tg],{stage,1,16,1,Appearance\[Rule]"Open",ControlsRendering\[Rule]"Generic"},{{t,0.6},0,1,Appearance\[Rule]"Open",ControlsRendering\[Rule]Automatic},{tg,0,1,Appearance\[Rule]"Open"}]*)*)


(* ::Section:: *)
(*Generation and export*)


(* ::Input::Initialization:: *)
animation[t_]:=Module[{stage,tloc,num=finalStageIdx},
{stage,tloc}=QuotientRemainder[t,1/num];
stage+=1;
tloc*=num ;
If[stage==num+1,stage-=1;tloc=1];
tloc=Clip[1.2tloc,{0,1}];

scene[stage,tloc,15t]
]


(* ::Input:: *)
(*(*Manipulate[animation[t],{t,0,1}]*)*)


(* ::Input:: *)
(*numsamples=200-1;*)
(*frames=Table[animation[t],{t,0,1,1/numsamples}];*)
(*{time,frames}=AbsoluteTiming[Rasterize[#,ImageSize->500]&/@frames];*)


(* ::Input:: *)
(*time*)


(* ::Input:: *)
(*filename="anim.gif";*)
(*SetDirectory[NotebookDirectory[]]*)
(*SystemOpen@Export[filename,frames,AnimationRepetitions->Infinity,"DisplayDurations"->.25]*)
