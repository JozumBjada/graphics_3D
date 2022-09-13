(* ::Package:: *)

(* ::Title:: *)
(*Fibre bundle*)


(* ::Subtitle:: *)
(*Wiki animation source code*)


(* ::Text:: *)
(*Source code for file: "Fibre bundle - animation.gif"*)
(*https://commons.wikimedia.org/wiki/File:Fibre_bundle_-_animation.gif*)
(*Version: "Mathematica 10.4.1.0 for Linux x86 (64-bit)"*)


(* ::Section:: *)
(*Code*)


(* geometry constants *)
r=3; (* radius *)
{circlePos,stripPos}={-5,0};
tubeWidth=0.05;

(* appearance constants *)
{maxRecur,imgResolution}={2,100};
{circleColor,stripColor,fibreColor}={Darker[Green],Red,Blue};
plotRange={{-r-1,r+1},{-r-1,r+1},{-5,2}};
viewPoint={2.876201,-4.651416,1.7582050};

(* from curve make tube or arrow *)
tubePlot[plot_,tubeWidth_:tubeWidth]:=(plot)/.Line[pts_,rest___]:>Tube[pts,tubeWidth,rest]
tubeArrowPlot[plot_,tubeWidth_:tubeWidth]:=(plot)/.Line[pts_,rest___]:>Arrow@Tube[pts,tubeWidth,rest]

(* circle *)
circle[v_,z_:circlePos,r_:r]:={r Cos[v],r Sin[v],z}
ring=tubePlot@ParametricPlot3D[circle[u,stripPos],{u,0,2\[Pi]},PlotStyle->circleColor];

(* moebius strip *)
moebius[u_,v_]:={(r+u Cos[v/2]) Cos[v],(r+u Cos[v/2]) Sin[v],stripPos+u Sin[v/2]};
moebiusStrip=ParametricPlot3D[moebius[u,v],{u,-1,1},{v,0,2\[Pi]},PlotStyle->{stripColor,Opacity[0.8]},MaxRecursion->maxRecur,Mesh->None];

(* brush of fibres *)
brushFun=moebius[u,#]&/@Range[0,2\[Pi],0.1];
brush=ParametricPlot3D[brushFun,{u,-1,1},PlotStyle->fibreColor];

(* fibre *)
fibre=tubePlot@ParametricPlot3D[moebius[u,0],{u,-1,1},PlotStyle->{fibreColor,Opacity[0.8]},MaxRecursion->maxRecur,Mesh->None];

(* individual stages of animation... *)
ringGeneration={tubePlot@ParametricPlot3D[circle[u,stripPos],{u,0,#},PlotStyle->circleColor]}&/@Range[0.01,2\[Pi],0.1];
fibreGeneration={ring,tubeArrowPlot@ParametricPlot3D[moebius[u,0],{u,0,#},PlotStyle->fibreColor],tubePlot@ParametricPlot3D[moebius[u,0],{u,0,-#},PlotStyle->fibreColor]}&/@Range[0.01,1,0.05];
delayAfterFibreGeneration=Table[Last@fibreGeneration,5];
stripGeneration={ring,ParametricPlot3D[moebius[u,v],{u,-1,1},{v,0,#},PlotStyle->{stripColor,Opacity[0.8]},MaxRecursion->maxRecur,Mesh->None],tubeArrowPlot@ParametricPlot3D[moebius[u,#],{u,-1,1},PlotStyle->{fibreColor,Opacity[0.8]},MaxRecursion->maxRecur,Mesh->None]}&/@Range[0.01,2\[Pi]+0.05,0.1];
delayAfterStripGeneration=Table[Last@stripGeneration,5];
brushGeneration={moebiusStrip,fibre,ring,ParametricPlot3D[brushFun,{u,0,#},PlotStyle->fibreColor],ParametricPlot3D[brushFun,{u,0,-#},PlotStyle->fibreColor]}&/@Range[0.01,1,0.05];
ringMovement={moebiusStrip,brush,ring/.Tube[x___]:>Translate[Tube[x],{0,0,-#}]}&/@Range[0,4.2,0.1];
fibreMovement={moebiusStrip,brush,Last@ringMovement,Graphics3D[{fibreColor,Translate[Rotate[Tube[Line[{{0,0,0},{2,0,0}}],tubeWidth],\[Pi]/2 #,{0,1,0},{0,0,0}],#({-r,-r,stripPos+1}-moebius[0,0])+moebius[0,0]]}]}&/@Range[0,1,0.05];
ringMovement=Join[#,{fibre}]&/@ringMovement;
delayAfterFibreMovement=Table[Last@fibreMovement,20];

(* put everything together and rasterize *)
animation=Join[ringGeneration,fibreGeneration,delayAfterFibreGeneration,stripGeneration,delayAfterStripGeneration,brushGeneration,ringMovement,fibreMovement,delayAfterFibreMovement];
animation=Show[#,Boxed->False,Axes->False,ViewPoint->viewPoint,PlotRange->plotRange]&/@animation;
animation=Rasterize[#,ImageResolution->imgResolution]&/@animation;


(* ::Code:: *)
(*(* export as gif animation and show *)*)
(*Export[NotebookDirectory[]<>"fibre_bundle_animation.gif",animation]*)
(*ListAnimate[animation]*)
