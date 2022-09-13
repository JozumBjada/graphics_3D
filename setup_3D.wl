(* ::Package:: *)

(* ::Title:: *)
(*3D experimental setup*)


(* ::Text:: *)
(*Source code for Fig. 2 in PNAS 117 (42) 26118-26122*)
(*https://www.pnas.org/doi/10.1073/pnas.2011405117*)
(*Version: "13.0.1 for Microsoft Windows (64-bit) (January 28, 2022)"*)


(* ::Chapter::Closed:: *)
(*Components*)


(* ::Section:: *)
(*Trombone arrow*)


(* ::Input::Initialization:: *)
arrow2D[da_,ds_,s_,h_,z_:0]:=Module[{pts1,pts2,pts,o1,o2,t1,t2,u1,u2,d1,d2,a1,a2,b1,b2},
o1={-h/2,0};
o2={h/2,0};

a1=o1+{0,ds};
b1=o1-{0,ds};
u1=o1+{0,da};
d1=o1-{0,da};
t1=o1-{s,0};

a2=o2+{0,ds};
b2=o2-{0,ds};
u2=o2+{0,da};
d2=o2-{0,da};
t2=o2+{s,0};

pts1={a1,u1,t1,d1,b1};
pts2={a2,u2,t2,d2,b2};
pts=Join[pts1,Reverse@pts2];
pts={##,z}&@@@pts;
Polygon[pts]
]


(* ::Input:: *)
(*(*Graphics3D[{Black,Opacity[.5],arrow2D[1,.4,1,2]}]*)*)


(* ::Section:: *)
(*Edge params etc.*)


(* ::Input::Initialization:: *)
edgeStyle={Thickness[.0001],GrayLevel[0.1]};
roundRad=.25;
fontFamily="Arial Baltic";(*"Consolas"*)(*"DejaVu Sans Light"*)(*"Times"*)
pumpCol=RGBColor[0.5,0.41,0.85];
spdcCol=Orange;


(* ::Input:: *)
(*(*{#,ColorData[#,"Image"]}&/@ColorData["Indexed"]*)*)
(*(*Multicolumn[Labeled[Style["Laser &",FontFamily\[Rule]#,Background\[Rule]LightBlue,FontSize\[Rule]30],#,Left]&/@$FontFamilies,4]*)*)


(* ::Section:: *)
(*Beveled cuboid*)


(* ::Input::Initialization:: *)
roundedCuboid[p1_,p2_,r_]:=ResourceFunction["RoundedCuboid"][p1,p2,RoundingRadius->r]


(* ::Input:: *)
(*(*Graphics3D[{Yellow,roundedCuboid[{0,0,0},{1,3,1},1/10],Blue,roundedCuboid[{2,1,1},{4,2,3},1/4]},Boxed\[Rule]False]*)*)


(* ::Section:: *)
(*SPP*)


(* ::Input::Initialization:: *)
segmentNum=15;
segmentAngle=(2\[Pi])/segmentNum;
segmentLength=1.;
segmentHalfBase=segmentLength Tan[segmentAngle/2];
segmentThickness=0.02;
colSpp=Setting[ColorSetter[RGBColor[0.5254901960784314, 0.8666666666666667, 0.8666666666666667]]];
colSppLett=Darker[colSpp,.5];


(* ::Input::Initialization:: *)
segment[n_]:=Prism[{{0,0,0},{segmentLength,-segmentHalfBase,0},{segmentLength,+segmentHalfBase,0},{0,0,n segmentThickness},{segmentLength,-segmentHalfBase,n segmentThickness},{segmentLength,+segmentHalfBase,n segmentThickness}}]
spp={Opacity[.5],colSpp,EdgeForm[edgeStyle],Table[Rotate[segment[n],-n segmentAngle,{0,0,1}],{n,segmentNum}]};
spp=Rotate[spp,\[Pi]/2,{0,1,0}];


(* ::Section:: *)
(*Detectors*)


(* ::Input::Initialization:: *)
detCol=GrayLevel[.15];


(* ::Input::Initialization:: *)
tt=Translate[Rotate[#,\[Pi]/2,{0,1,0}],{-0.7,0,0}]&@InputForm[RevolutionPlot3D[1.5 0.95^2,{t,0,0.95},BoxRatios->{1, 1, 1},PlotRange->All,Mesh->None,PlotStyle->detCol,Lighting->"Neutral"]][[1,1]];
detR={Translate[Rotate[#,\[Pi]/2,{0,1,0}],{-0.7,0,0}]&@InputForm[RevolutionPlot3D[(*Exp[5t]/150*)1.5 t^4,{t,0,1},BoxRatios->{1, 1, 1},PlotRange->1,Mesh->None,PlotStyle->detCol(*GrayLevel[0.32]*),Lighting->"Neutral"]][[1,1]]};
det={detR,tt};


(* ::Input:: *)
(*(*Graphics3D[det,Lighting\[Rule](*{"Ambient",White}*)"Neutral"]*)*)


(* ::Section:: *)
(*Coincidence logic*)


(* ::Input::Initialization:: *)
texCoinc=Graphics[Text[Style["&",FontFamily->fontFamily,FontSize->180]],PlotRange->{{-3,3},{-4,4}}];


(* ::Input::Initialization:: *)
coinc=With[{x=1.5,y=2,z=1},With[{pt={x,y,z-0.05}},{EdgeForm[None],{Lighter[detCol,.3],roundedCuboid[-pt,pt,roundRad]},{Texture[Rasterize[texCoinc,RasterSize->200,Background->None]],Polygon[{{-x,-y,z},{-x,y,z},{x,y,z},{x,-y,z}},VertexTextureCoordinates->{{0,0},{0,1},{1,1},{1,0}}]}}]];


(* ::Input::Initialization:: *)
wiresPlot=Plot[2ArcTan[x],{x,-15,10},AspectRatio->1,PlotRange->10,PlotPoints->10];


(* ::Input::Initialization:: *)
curve=Cases[InputForm[wiresPlot],x_Line,Infinity]/.Line->List;
curve=Flatten[curve,2];
curve3d=curve/.{x_Real,y_Real}:>{x,y,0};


(* ::Input:: *)
(*(*Graphics3D[coinc,Lighting\[Rule]"Neutral"]*)*)


(* ::Input:: *)
(*(*Graphics3D[Tube[curve3d,1]]*)*)


(* ::Section:: *)
(*SLM*)


(* ::Input::Initialization:: *)
slmHeight=1.5;
slmNum=7;
slmW=slmHeight/slmNum;
slm=Translate[#,-{slmHeight/2,.1,slmHeight/2}]&@Translate[{{Black,Cuboid[{0,0,0},{slmW,.2,slmHeight}]},{White,EdgeForm[edgeStyle],Translate[#,{slmW,0,0}]&@Cuboid[{0,0,0},{slmW,.2,slmHeight}]}},Range[0,slmHeight,2 slmW]/.x_?NumberQ:>{x,0,0}];
(*Graphics3D[slm]*)


(* ::Section:: *)
(*Laser*)


(* ::Input::Initialization:: *)
texLaser=Graphics[Text[Style["Laser",FontFamily->fontFamily,FontSize->100],{0,0},{0,0},{0,1}],PlotRange->{{-3,3},{-5,5}}];
lasCol=Setting[ColorSetter[RGBColor[0.396078431372549, 0.5686274509803921, 0.9725490196078431]]];


(* ::Input::Initialization:: *)
laser=With[{x=1.5,y=3,z=1},With[{pt={x,y,z-0.05}},{EdgeForm[None],{lasCol,roundedCuboid[-pt,pt,roundRad]},{Texture[Rasterize[texLaser,RasterSize->200,Background->None]],Polygon[{{-x,-y,z},{-x,y,z},{x,y,z},{x,-y,z}},VertexTextureCoordinates->{{0,0},{0,1},{1,1},{1,0}}]}}]];


(* ::Section:: *)
(*Bounding boxes*)


(* ::Input::Initialization:: *)
ClearAll[colorBox]


(* ::Input::Initialization:: *)
colBoxOpacity=.4(*.25*);


(* ::Input::Initialization:: *)
colorBox[col_,ptsa_List,ind_:5]:=Module[{ptsaux,pts,zoff=3,len=Length[ptsa]},
pts=Plus[{0,0,-1.5},#]&/@ptsa;
ptsaux=Plus[{0,0,zoff},#]&/@pts;
{Opacity[colBoxOpacity],EdgeForm[None],col,GraphicsComplex[Join[pts,ptsaux],Polygon[Table[{i,i+1,i+1+len,i+len},{i,2,len-1}]~Join~{{len,1,len+1,2len},Range[len]}]]}
]


(* ::Input::Initialization:: *)
colorBoxRest[col_,ptsa_List,ind_:5]:=Module[{ptsaux,pts,zoff=3,len=Length[ptsa]},
pts=Plus[{0,0,-1.5},#]&/@ptsa;
ptsaux=Plus[{0,0,zoff},#]&/@pts;
{Opacity[colBoxOpacity],EdgeForm[None],col,GraphicsComplex[Join[pts,ptsaux],Polygon[Table[{i,i+1,i+1+len,i+len},{i,2,len-3}]~Join~{(*{len,1,len+1,2len},*)Range[len]}]]}
]


(* ::Input:: *)
(*(*Graphics3D[colorBox[Red,{{0,0,0},{1,0,0},{1,1,0},{2,1,0},{2,2,0},{0,2,0}},5]]*)*)


(* ::Section:: *)
(*Colors*)


(* ::Input::Initialization:: *)
col1r[1]=RGBColor[0.17,0.5,0.83]
col1r[2]=RGBColor[0.17,0.5,0.83]
col1r[3]=RGBColor[0.17,0.5,0.83]


(* ::Input::Initialization:: *)
col2r[1]=RGBColor[0.5,0.85,0.19]
col2r[2]=RGBColor[0.23,0.65,0.5]
col2r[3]=Green


(* ::Input::Initialization:: *)
col3r[1]=Setting[ColorSetter[RGBColor[0.9764705882352941, 0.6862745098039216, 0.14901960784313725`]]]
col3r[2]=RGBColor[0.99,0.5,0.08]
col3r[3]=Orange


(* ::Input::Initialization:: *)
col4r[1]=Setting[ColorSetter[RGBColor[0.7372549019607844, 0.35294117647058826`, 0.996078431372549]]]
col4r[2]=RGBColor[0.5,0.18,0.5]
col4r[3]=Purple


(* ::Input::Initialization:: *)
col1=col1r
col2=col2r
col3=col3r
col4=col4r


(* ::Input::Initialization:: *)
col1=col3r
col2=col2r
col3=col1r
col4=col4r


(* ::Input::Initialization:: *)
colidx=3;


(* ::Input::Initialization:: *)
col1ra[1]=col1ra[2]=col1ra[3]=ColorData[colidx][1]
col2ra[1]=col2ra[2]=col2ra[3]=ColorData[colidx][2]
col3ra[1]=col3ra[2]=col3ra[3]=ColorData[colidx][3]
col4ra[1]=col4ra[2]=col4ra[3]=ColorData[colidx][4]


(* ::Input::Initialization:: *)
col1ra[1]=col1ra[2]=col1ra[3]=ColorData[colidx][2]
col2ra[1]=col2ra[2]=col2ra[3]=ColorData[colidx][3]
col3ra[1]=col3ra[2]=col3ra[3]=ColorData[colidx][4]
col4ra[1]=col4ra[2]=col4ra[3]=ColorData[colidx][5]


(* ::Input::Initialization:: *)
col1ra[2]=Darker[col1ra[2],.5]
col2ra[2]=Darker[col2ra[2],.5]
col3ra[2]=Darker[col3ra[2],.5]
col4ra[2]=Darker[col4ra[2],.5]


(* ::Input::Initialization:: *)
col1=col1ra
col2=col2ra
col3=col3ra
col4=col4ra


(* ::Section:: *)
(*Labels*)


(* ::Input::Initialization:: *)
ClearAll[label]


(* ::Input::Initialization:: *)
label[text_,col_,p1_,p2_,size_,fontcol_]:=Module[{upper=3.5,lower=1.5,tex,yoff=.1},
tex=Graphics[Inset[Text[Style[text,FontSize->size,FontColor->fontcol,FontFamily->fontFamily]],Left,Left],PlotRange->{{0,p2[[1]]-p1[[1]]},All}];
{{EdgeForm[None],Texture[Rasterize[tex,RasterSize->350,Background->None]],Polygon[{p1+{0,0,lower},p2+{0,0,lower},p2+{0,0,upper},p1+{0,0,upper}},VertexTextureCoordinates->{{0,0},{1,0},{1,1},{0,1}}]},
{EdgeForm[None],col,Opacity[.7],Polygon[{p1+{0,yoff,lower},p2+{0,yoff,lower},p2+{0,yoff,upper},p1+{0,yoff,upper}}]}}
]


(* ::Input::Initialization:: *)
labelA[text_,col_,p1_,p2_,size_,fontcol_]:=Module[{upper=-0.1,lower=-2.5,tex,zz=-1.5,zoff=-1.6},
tex=Graphics[Inset[Text[Style[text,FontSize->size,FontColor->fontcol,FontFamily->fontFamily]],Left,Left],PlotRange->{{0,p2[[1]]-p1[[1]]},All}];
{{EdgeForm[None],Texture[Rasterize[tex,RasterSize->950,Background->None]],Polygon[{p1+{0,lower,zz},p2+{0,lower,zz},p2+{0,upper,zz},p1+{0,upper,zz}},VertexTextureCoordinates->{{0,0},{1,0},{1,1},{0,1}}]},
{EdgeForm[None],col,Opacity[.7],Polygon[{p1+{0,lower,zoff},p2+{0,lower,zoff},p2+{0,upper,zoff},p1+{0,upper,zoff}}]}}
]


(* ::Input:: *)
(*(*Graphics3D[labelA["1st dim",LightGray,{0,0,0},{1,0,0},50,Blue]]*)*)


(* ::Input:: *)
(*(*Graphics3D[labelA[" Detection",RGBColor[0.5,0.23,0.66],{boxX1d,boxY1,0},{boxX2d,boxY1,0},28,RGBColor[0.5,0.18,0.5]]]*)*)


(* ::Section:: *)
(*Crystals*)


(* ::Input::Initialization:: *)
crX=0.85(*.8*);
roundRadCr=0.1`;
colCr=Setting[ColorSetter[RGBColor[1., 0.6705882352941176, 0.17647058823529413`]]];
colCrLett=Darker[colCr,.5];
crystal=With[{pt={crX,.5,.5}},{colCr,roundedCuboid[-pt,pt,roundRadCr]}];


(* ::Input:: *)
(*(*Graphics3D[crystal,Lighting\[Rule]"Neutral"]*)*)


(* ::Section:: *)
(*Opt. elements*)


(* ::Input::Initialization:: *)
mirror={Setting[ColorSetter[RGBColor[0.5686274509803921, 0.6862745098039216, 0.8352941176470589]]],EdgeForm[edgeStyle],With[{pt={.15,0,0}},Cylinder[{{0,0,0},2pt}(*{-pt,pt}*),.7]]};
dm=With[{pt={.2,.6,.6}},{LightGreen,EdgeForm[edgeStyle],Cuboid[-pt,pt]}];
lens={Setting[ColorSetter[RGBColor[0.47843137254901963`, 0.6549019607843137, 0.9215686274509803]]],Opacity[.5],Ellipsoid[{0,0,0},{.3,1,1}]};
trLen=3.5;
trW=.7;
bpf=With[{pt={.15,0,0}},{Cyan,EdgeForm[edgeStyle],Cylinder[{-pt,pt},.7]}];
trombone=With[{off={0,0,1},p1={0,0,0},p2={trLen,0,0},p3={trLen-trW,trW,0},p4={trW,trW,0}},{LightGray,EdgeForm[edgeStyle],Opacity[.7],Translate[#,-{trLen/2,trW/2,.5}]&@Hexahedron[{p1,p2,p3,p4,p1+off,p2+off,p3+off,p4+off}]}];
bs=With[{pt1={0,0,0},pt2a={1,0,0},pt2={0,1,0},pt3={1,1,0}},{LightBlue,EdgeForm[edgeStyle],Opacity[.5],Translate[#,-.5{1,1,1}]&@Prism[{pt1,pt2,pt3,pt1+{0,0,1},pt2+{0,0,1},pt3+{0,0,1}}],Translate[#,-.5{1,1,1}]&@Prism[{pt1,pt2a,pt3,pt1+{0,0,1},pt2a+{0,0,1},pt3+{0,0,1}}]}];


(* ::Input:: *)
(*(*Graphics3D[{bs,laser,spp,trombone(*Translate[Rotate[mirror,-\[Pi]/4,{0,0,1}],{0,-3,0}]*)(*,lens,crystal*)},Lighting\[Rule]"Neutral"]*)*)


(* ::Input:: *)
(*(*coinc=With[{pt={1.5,2,-1}},{Gray,Cuboid[-pt,pt]}];*)*)


(* ::Chapter:: *)
(*Composition*)


(* ::Section::Closed:: *)
(*Parameters*)


(* ::Input::Initialization:: *)
pos1cr=0;
pos3cr=25;
pos2cr=(pos3cr-pos1cr)/2;
lensoff=3;

pos11lens=(pos2cr-pos1cr-crX-lensoff)/4+(crX+lensoff)/2;
pos12lens=3 (pos2cr-pos1cr-crX-lensoff)/4+(crX+lensoff)/2;
pos21lens=pos11lens+pos2cr;
pos22lens=pos12lens+pos2cr;
pos31lens=pos11lens+pos3cr;
pos1mirr=pos1dm-pos2cr;
dmoffset=.3;
pos1dm=(pos2cr+pos12lens)/2-dmoffset;
pos2dm=(pos3cr+pos22lens)/2-dmoffset;
lensY=-5;
baseY=-10;
pos1tr=2 (pos1dm-pos1mirr)/3+pos1mirr;
pos2tr=pos1tr+(pos2dm-pos1dm);
tromOffset=3;
pos1spp=(pos1dm-pos1mirr)/3+pos1mirr;
pos2spp=pos1spp+(pos2dm-pos1dm);
laserX=pos1mirr-(pos2cr-pos1cr).3;
posB11mirr=pos1tr-trLen/2+trW;
posB12mirr=pos1tr+trLen/2-trW;
posB21mirr=posB11mirr+(pos2dm-pos1dm);
posB22mirr=posB12mirr+(pos2dm-pos1dm);
posB23mirr=pos2dm;
posBPF=pos3cr+(pos3cr-pos2cr).6;
spdcrad=.4;
pumprad=.25;
pumpradMid=pumprad/2;
posFinalBS=pos3cr+(pos3cr-pos2cr).8;
slm1X=pos3cr+(pos3cr-pos2cr)1.1;
detX=posBPF+(posFinalBS-posBPF).3;


(* ::Input::Initialization:: *)
boxX1a=laserX-3;
boxX2a=pos1cr(*pos1mirr+3*);
boxX3a=(pos11lens+pos12lens)/2;
boxY1=baseY-2;
boxY2=lensY;
boxY3=2;


(* ::Input::Initialization:: *)
xoffset=.2;


(* ::Input::Initialization:: *)
boxY2a=lensY-xoffset;


(* ::Input::Initialization:: *)
boxX1b=boxX2a+xoffset;
boxX2b=pos2cr;
boxX3b=(pos21lens+pos22lens)/2;
boxX4b=boxX3a+xoffset;


(* ::Input::Initialization:: *)
boxX1c=boxX1b+pos3cr-pos2cr;
boxX2c=boxX2b+pos3cr-pos2cr;
boxX3c=boxX3b+pos3cr-pos2cr;
boxX4c=boxX4b+pos3cr-pos2cr;


(* ::Input::Initialization:: *)
boxX1d=boxX2c+xoffset;
boxX2d=slm1X+2;
boxX3d=boxX3c+xoffset;


(* ::Input::Initialization:: *)
detY2=baseY+(lensY-baseY).6;


(* ::Section::Closed:: *)
(*Setup*)


(* ::Input:: *)
(*gr=Graphics3D[(*Scale[#,50]&@*){EdgeForm[edgeStyle(*Thickness[0.0005]*)],*)
(**)
(**)
(*Translate[laser,{laserX,lensY,0}],*)
(*Translate[Rotate[mirror,3\[Pi]/4,{0,0,1}],{{pos1mirr,0,0}}],Translate[crystal,{{pos1cr,0,0},{pos2cr,0,0},{pos3cr,0,0}}],Translate[lens,{{pos11lens,0,0},{pos12lens,0,0},{pos21lens,0,0},{pos22lens,0,0},{pos31lens,0,0}}],Translate[Rotate[dm,-\[Pi]/4,{0,0,1}],{{pos1dm,0,0},{pos2dm,0,0}}],Translate[Rotate[lens,\[Pi]/2,{0,0,1}],{{pos1mirr,lensY,0},{pos1dm,lensY,0},{pos2dm,lensY,0}}],*)
(*Translate[bs,{{pos1mirr,baseY,0},{pos1dm,baseY,0}}],*)
(*Translate[trombone,{{pos1tr,baseY+tromOffset,0},{pos2tr,baseY+tromOffset,0}}],*)
(*Translate[spp,{{pos1spp,baseY,0},{pos2spp,baseY,0}}],*)
(*Translate[Rotate[mirror,-3\[Pi]/4,{0,0,1}],{{laserX,baseY,0},{posB12mirr,baseY,0},{posB22mirr,baseY,0}}],*)
(*Translate[Rotate[mirror,-\[Pi]/4,{0,0,1}],{{posB11mirr,baseY,0},{posB21mirr,baseY,0},{posB23mirr,baseY,0}}],*)
(*Translate[Rotate[bs,\[Pi]/2,{0,0,1}],{posFinalBS,0,0}],*)
(**)
(*Translate[bpf,{posBPF,0,0}],*)
(*{pumpCol,{Opacity[0.76],Tube[{{{laserX,lensY,0},{laserX,baseY,0},{posB11mirr,baseY,0},{posB11mirr,baseY+tromOffset,0},{posB12mirr,baseY+tromOffset,0},{posB12mirr,baseY,0},{posB21mirr,baseY,0},{posB21mirr,baseY+tromOffset,0},{posB22mirr,baseY+tromOffset,0},{posB22mirr,baseY,0},{posB23mirr,baseY,0},{posB23mirr,baseY,0},{posB23mirr,lensY,0}},{{pos1mirr,baseY,0},{pos1mirr,lensY,0}},{{pos1dm,baseY,0},{pos1dm,lensY,0}},{{pos11lens,0,0},{pos12lens,0,0}},{{pos21lens,0,0},{pos22lens,0,0}}},pumprad]},*)
(*{Opacity[1],Cone[{{pos11lens,0,0},{pos1cr,0,0}},pumprad],Cone[{{pos12lens,0,0},{pos2cr,0,0}},pumprad],Cone[{{pos21lens,0,0},{pos2cr,0,0}},pumprad],Cone[{{pos22lens,0,0},{pos3cr,0,0}},pumprad],Cone[{{pos31lens,0,0},{pos3cr,0,0}},pumprad],Cone[{{pos1mirr,0,0},{pos1cr,0,0}},pumpradMid],Tube[{{{pos1mirr,lensY,0},{pos1mirr,0,0}},{{pos1dm,lensY,0},{pos1dm,0,0}},{{pos2dm,lensY,0},{pos2dm,0,0}}},{pumprad,pumpradMid}],*)
(*Tube[{{pos31lens,0,0},{posBPF-.1,0,0}},pumprad]}},*)
(*{Glow[RGBColor[1, 0, 0]],spdcCol,Opacity[.5],Tube[{{{pos11lens,0,0},{pos12lens,0,0}},{{pos21lens,0,0},{pos22lens,0,0}},{{pos31lens,0,0},{posFinalBS,0,0}}},spdcrad],Cone[{{pos11lens,0,0},{pos1cr,0,0}},spdcrad],Cone[{{pos12lens,0,0},{pos2cr,0,0}},spdcrad],Cone[{{pos21lens,0,0},{pos2cr,0,0}},spdcrad],Cone[{{pos22lens,0,0},{pos3cr,0,0}},spdcrad],Cone[{{pos31lens,0,0},{pos3cr,0,0}},spdcrad],*)
(*Tube[{{{posFinalBS,0,0},{posFinalBS,detY2,0},{detX,detY2,0}},{{posFinalBS,0,0},{slm1X,0,0},{slm1X,baseY,0},{detX,baseY,0}}},spdcrad]*)
(*},*)
(*Translate[Rotate[Translate[slm,{0,.3,0}],-\[Pi]/4,{0,0,1}],{slm1X,0,0}],*)
(*Translate[Rotate[Translate[slm,{0,-.3,0}],\[Pi]/4,{0,0,1}],{posFinalBS,detY2,0}],*)
(*Translate[Rotate[Translate[mirror,{.3,0,0}],-\[Pi]/4,{0,0,1}],{slm1X,baseY,0}],*)
(*Translate[det,{{detX,detY2,0},{detX,baseY,0}}],*)
(*Translate[coinc,{pos31lens,baseY+tromOffset/2,0}],*)
(**)
(*colorBox[col1[3](*Gray*),{{boxX1a,boxY1,0},{boxX2a,boxY1,0},{boxX2a,boxY2,0},{boxX3a,boxY2,0},{boxX3a,boxY3,0},{boxX1a,boxY3,0}},5],*)
(*colorBoxRest[col2[3],{{boxX1b,boxY1,0},{boxX2b,boxY1,0},{boxX2b,boxY2,0},{boxX3b,boxY2,0},{boxX3b,boxY3,0},{boxX4b,boxY3,0},{boxX4b,boxY2a,0},{boxX1b,boxY2a,0}},5],*)
(*colorBoxRest[col3[3],{{boxX1c,boxY1,0},{boxX2c,boxY1,0},{boxX2c,boxY2,0},{boxX3c,boxY2,0},{boxX3c,boxY3,0},{boxX4c,boxY3,0},{boxX4c,boxY2a,0},{boxX1c,boxY2a,0}},5],*)
(*colorBoxRest[col4[3],{{boxX1d,boxY1,0},{boxX2d,boxY1,0},{boxX2d,boxY3,0},{boxX3d,boxY3,0},{boxX3d,boxY2a,0},{boxX1d,boxY2a,0}},3],*)
(**)
(*labelA[" 1st dim",col1[1](*GrayLevel[0.7]*),{boxX1a,boxY1,0},{boxX2a,boxY1,0},42,col1[2](*GrayLevel[0.37]*)],*)
(*labelA[" 2nd dim",col2[1],{boxX1b,boxY1,0},{boxX2b,boxY1,0},35,col2[2]],*)
(*labelA[" 3rd dim",col3[1](*RGBColor[0.5,0.85,0.19]*),{boxX1c,boxY1,0},{boxX2c,boxY1,0},35,(*RGBColor[0.23,0.65,0.5]*)col3[2]],*)
(*labelA[" Detection",col4[1],{boxX1d,boxY1,0},{boxX2d,boxY1,0},28,col4[2]],*)
(**)
(*{GrayLevel[0.15],*)
(*Translate[Tube[.1curve3d,.1],{(detX+pos31lens)/2+.5,detY2-.25,0}],*)
(*Translate[Rotate[Tube[.1curve3d,.1],\[Pi],{1,0,0}],{(detX+pos31lens)/2+.5,baseY+.25,0}]*)
(*},*)
(**)
(*{*)
(*(*Text[Style["PBS",FontFamily\[Rule]fontFamily,FontSize\[Rule]27],{posFinalBS,0,0},{-.5,-3}],*)*)
(*Text[Style["A",FontWeight->Bold,FontFamily->fontFamily,FontSize->35,FontColor->colCrLett],{pos1cr,0,0},{-.5,-2.2}],*)
(*Text[Style["B",FontWeight->Bold,FontFamily->fontFamily,FontSize->35,FontColor->colCrLett],{pos2cr,0,0},{-.5,-2.2}],*)
(*Text[Style["C",FontWeight->Bold,FontFamily->fontFamily,FontSize->35,FontColor->colCrLett],{pos3cr,0,0},{-.5,-2.2}],*)
(**)
(*Text[Style["+4",FontFamily->fontFamily,FontSize->30,FontColor->colSppLett],{pos1spp,baseY,0},{-.5,-2.5}],*)
(*Text[Style["-8",FontFamily->fontFamily,FontSize->30,FontColor->colSppLett],{pos2spp,baseY,0},{-.5,-2.5}]*)
(*},*)
(**)
(*{*)
(*{Black,Opacity[.5],Translate[Rotate[arrow2D[1,.4,1,2],\[Pi]/2,{0,0,1}],{{pos1tr,baseY+tromOffset(*+.2*),0.6},{pos2tr,baseY+tromOffset(*+.2*),0.6}}]}*)
(*}*)
(*},Lighting->(*{"Point",GrayLevel[0.8],{1.3`,-2.4`,2.`}}*)(*{"Ambient",White}*)(*{"Directional",GrayLevel[0.87],{50,50,50}}*)"Neutral"(*{"Ambient",GrayLevel[0.51]}*),Boxed->False,ImageSize->1500(*,ViewPoint\[Rule]Top*)(*,ViewProjection\[Rule]"Perspective"*)(*ViewMatrix\[Rule]{mat[1,-5,-5,10],IdentityMatrix[4]}*)];*)
(*gr*)


(* ::Section::Closed:: *)
(*Image*)


(* ::Input:: *)
(*img=Rasterize[gr,ImageResolution->100,Background->None];*)
(*img=ImagePad[img,-BorderDimensions[img, 0.001]];*)
(*img*)


(* ::Section::Closed:: *)
(*Export*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)
(*(*Export["setup3dJ.pdf",gr,ImageResolution\[Rule]1000]*)*)
(*(*Export["setup3D_J.pdf",img]*)*)
(*(*Export["setup3D_L.svg",img]*)*)
