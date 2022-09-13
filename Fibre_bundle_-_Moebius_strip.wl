(* ::Package:: *)

(* ::Title:: *)
(*M\[ODoubleDot]bius strip as a fibre bundle*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "Fibre bundle - Moebius strip.png"*)
(*https://commons.wikimedia.org/wiki/File:Fibre_bundle_-_Moebius_strip.png*)
(*Version: "Mathematica 10.4.1.0 for Linux x86 (64-bit)"*)


(* ::Section:: *)
(*Code*)


(* geometry constants *)
r=3;(* radius *)
{circlePos,stripPos}={-5,0};
{phi1,phi2,phi3}={0,2,4};
fibrePar1=-0.1;
{tubeWidth,tubeWidthFat}={0.05,0.15};
fibrePos={-r-1,-r-1,-1};

(*appearance constants*)
{maxRec,imgResolution}={5,400};
fontFam="TeX Gyre Pagella"(*"Latin Modern Roman"*);
{smallFontSize,largeFontSize}={15,25};
{arrowColor,circleColor,stripColor,fibreColor}={Darker[Green],Darker[Green],Red,Blue};
plotRangeA={{-r-2,r+3.5},{-r-2,r+2},{-5,2}};
viewPoint={2.876201,-4.651416,1.7582050};

(* from curve make tube or arrow *)
tubePlot[plot_,tubeWidth_:tubeWidth]:=(plot)/.Line[pts_,rest___]:>Tube[pts,tubeWidth,rest]
tubeArrowPlot[plot_,tubeWidth_:tubeWidth]:=(plot)/.Line[pts_,rest___]:>Arrow@Tube[pts,tubeWidth,rest]

(* circle *)
circle[v_,z_:circlePos,r_:r]:={r Cos[v],r Sin[v],z}
ring=tubePlot@ParametricPlot3D[circle[v],{v,0,2\[Pi]},PlotStyle->circleColor];

(* moebius strip *)
x[u_,v_]:=(r+u Cos[v/2]) Cos[v]
y[u_,v_]:=(r+u Cos[v/2]) Sin[v]
z[u_,v_]:=stripPos+u Sin[v/2]
moebius[u_,v_]:={x[u,v],y[u,v],z[u,v]};
moebiusStrip=ParametricPlot3D[moebius[u,v],{u,-1,1},{v,0,2\[Pi]},PlotStyle->{stripColor,Opacity[0.8]},MaxRecursion->maxRec,Mesh->None];

(* fibre *)
fibre=Graphics3D[{fibreColor,Tube[{fibrePos,fibrePos+{0,0,2}},tubeWidth]}];

(* points on base space and corresponding fibres *)
fibresOnStrip=ParametricPlot3D[moebius[u,#],{u,-1,1},PlotStyle->fibreColor,Mesh->None]&/@{phi1,phi2,phi3};
pointsOnRing=Graphics3D[{arrowColor,Ball[circle[#],0.2]}]&/@{phi1,phi2,phi3};

(* labels for points on base space *)
(* textForPointsOnRingData = {{label string, angle, text offset}..} *)
textForPointsOnRingData={{"Subscript[m, 1] = \[Pi](Subscript[F, 1])",phi1,{-.2,.6}},{"Subscript[m, 2]",phi2,{0,0}},{"Subscript[m, 3]",phi3,{0,0}}}; 
textForPointsOnRing=Graphics3D[Style[Text[#1,circle[#2,circlePos-0.5],#3],FontSize->smallFontSize,arrowColor,FontFamily->fontFam]]&@@@textForPointsOnRingData;

(* arrows *)
arrow1=With[{\[CurlyPhi]=phi1},Graphics3D[{arrowColor,Arrow@Tube@BezierCurve[{moebius[1,\[CurlyPhi]],circle[\[CurlyPhi],z[1,\[CurlyPhi]],r+5],circle[\[CurlyPhi],circlePos+1,r+3],circle[\[CurlyPhi]]}]}]];
arrow2=With[{\[CurlyPhi]=phi2},Graphics3D[{arrowColor,Arrow@Tube@BezierCurve[{moebius[-1,\[CurlyPhi]],moebius[-1,\[CurlyPhi]]-1.5(moebius[1,\[CurlyPhi]]-moebius[-1,\[CurlyPhi]]),circle[\[CurlyPhi]]}]}]];
arrow3=With[{\[CurlyPhi]=phi3},Graphics3D[{arrowColor,Arrow@Tube@BezierCurve[{moebius[-1,\[CurlyPhi]],moebius[-1,\[CurlyPhi]]-1.5(moebius[1,\[CurlyPhi]]-moebius[-1,\[CurlyPhi]]),circle[\[CurlyPhi]]}]}]];

(* labels for fibres over points *)
(* textForFibresOnStripData = {{label string, angle, z-position, extra-radius, text offset}..} *)
textForFibresOnStripData={{"Subscript[F, 1] = \[Pi]^-1(Subscript[m, 1])",phi1,0.8,1.6,{-1.1,0}},{"Subscript[F, 2]",phi2,1.4,2,{0,0}},{"Subscript[F, 3]",phi3,1.9,1,{-1.5,0}}};
textForFibresOnStrip=Graphics3D[Style[Text[#1,circle[#2,stripPos+#3,r+#4],#5],FontSize->smallFontSize,fibreColor,FontFamily->fontFam]]&@@@textForFibresOnStripData;

(* labels for total space,base space and fibre *)
textStrip=Graphics3D[Style[Text["E",{r,0.5r,2.4}],stripColor,FontSize->largeFontSize,Bold,FontFamily->fontFam]];
textBase  =Graphics3D[Style[Text["M",{r,0,-2.7}],circleColor,FontSize->largeFontSize,Bold,FontFamily->fontFam]];
textFibre=Graphics3D[Style[Text["F",{-r-1,-r-1,-2}],fibreColor,FontSize->largeFontSize,Bold,FontFamily->fontFam]];

(* objects corresponding to point p_3 *)
stripFibre3=With[{\[CurlyPhi]=phi3},ParametricPlot3D[{(-r-1+v(x[u,\[CurlyPhi]]-(-r-1))),(-r-1+v(y[u,\[CurlyPhi]]-(-r-1))),u},{u,-1,1},{v,0,1},Mesh->None,PlotStyle->{fibreColor,Opacity[0.5]}]];
fibrePointPos={-r-1,-r-1,fibrePar1};
stripPoint3=Ball[moebius[fibrePar1,phi3],0.15];
fibrePoint3=Ball[fibrePointPos,0.15];
arrowPoint3=Arrow@Tube[{fibrePointPos,moebius[fibrePar1,phi3]}];
textFibreStripPoint3=Style[Text["Subscript[f, 3]",fibrePointPos,{1.8,0}],FontSize->smallFontSize,fibreColor,FontFamily->fontFam];
textStripPoint3=Style[Text["Subscript[p, 3]",moebius[fibrePar1,phi3],{-1.5,0}],FontSize->smallFontSize,fibreColor,FontFamily->fontFam];

(* assemble objects corresponding to point p_3 *)
fibrePointArrow=Graphics3D[{fibreColor,stripPoint3,fibrePoint3,arrowPoint3,textFibreStripPoint3,textStripPoint3}];

(* put everything together *)
Do[item[[0]]=Sequence,{item,{fibresOnStrip,pointsOnRing,textForPointsOnRing,textForFibresOnStrip}}];
img=Show[{moebiusStrip,ring,fibre,fibresOnStrip,pointsOnRing,arrow1,arrow2,arrow3,textStrip,textBase,textFibre,fibrePointArrow,stripFibre3,textForPointsOnRing,textForFibresOnStrip},
PlotRange->plotRangeA,Boxed->False,Axes->False,ViewPoint->viewPoint];


(* ::Code:: *)
(*(* export and show *)*)
(*Export[NotebookDirectory[]<>"fibre_bundle.png",img,ImageResolution->imgResolution]*)
(*Magnify[img]*)
