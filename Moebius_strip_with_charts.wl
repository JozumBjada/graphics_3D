(* ::Package:: *)

(* ::Title:: *)
(*M\[ODoubleDot]bius strip with charts*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "Moebius strip with charts.gif"*)
(*https://commons.wikimedia.org/wiki/File:Moebius_strip_with_charts.gif*)
(*Version: "Mathematica 10.4.1.0 for Linux x86 (64-bit)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
(* geometry constants *)
r=3; (* radius *)
{phi1A,phi2A,phi3A,phi4A}={0,5,5-1/10,2\[Pi]+1/10}; (* angle positions *)
{tubeWidth,tubeWidthFat}={0.05,0.15};
{circlePos,stripPos}={-5,0};

(* appearance constants *)
{rastFonSize,maxRec,imgResolution}={15,5,400};
fontFam="TeX Gyre Pagella"(*"Latin Modern Roman"*);
{smallFontSize,largeFontSize}={15,25};
{arrowColor,ringColor,stripColor,fibreColor}={Darker[Green],Darker[Green],Red,Blue};
plotRange={{-r-2,r+2},{-r-2,r+2},{-5,2}};
viewPoint={2.876201,-4.651416,1.7582050};

(* from curve make tube or arrow *)
tubePlot[plot_,tubeWidth_:tubeWidth]:=(plot)/.Line[pts_,rest___]:>Tube[pts,tubeWidth,rest]
tubeArrowPlot[plot_,tubeWidth_:tubeWidth]:=(plot)/.Line[pts_,rest___]:>Arrow@Tube[pts,tubeWidth,rest]

(* circle *)
circle[v_,z_:circlePos,r_:r]:={r Cos[v],r Sin[v],z}
ring=tubePlot@ParametricPlot3D[circle[v],{v,0,2\[Pi]},PlotStyle->ringColor];

(* moebius strip *)
moebius[u_,v_]:={(r+u Cos[v/2]) Cos[v],(r+u Cos[v/2]) Sin[v],stripPos+u Sin[v/2]};
moebiusStripTransparent=ParametricPlot3D[moebius[u,v],{u,-1,1},{v,0,2\[Pi]},PlotStyle->{stripColor,Opacity[0.5]},MaxRecursion->maxRec,Mesh->None];

(* fibre *)
fibrePos={-r-1,-r-1,-1};
fibre=Graphics3D[{fibreColor,Tube[{fibrePos,fibrePos+{0,0,2}},tubeWidth]}];

(* labels for total space, base space and fibre *)
textStrip=Graphics3D[Style[Text["E",{r,0.5r,2.4}],stripColor,FontSize->largeFontSize,Bold,FontFamily->fontFam]];
textRing =Graphics3D[Style[Text["M",{r,0,-2.7}],ringColor,FontSize->largeFontSize,Bold,FontFamily->fontFam]];
textFibre=Graphics3D[Style[Text["F",{-r-1,-r-1,-2}],fibreColor,FontSize->largeFontSize,Bold,FontFamily->fontFam]];

(* labels for points on a base space *)
textList = {{"\!\(\*SubscriptBox[\(m\), \(1\)]\) = m(0)", phi1A, {-.2, 0.5}},
    {"\!\(\*SubscriptBox[\(m\), \(2\)]\) = m(5)", phi2A, {1.0, 0.4}},
    {"\!\(\*SubscriptBox[\(m\), \(3\)]\) = m(5-1/10)", phi3A, {0.0, 0.3}},
    {"\!\(\*SubscriptBox[\(m\), \(4\)]\) = m(1/10)", phi4A, {-.5, 0.5}}};
textRingPoints=Graphics3D[Style[Text[#1,circle[#2,circlePos-0.4,r+0.5],#3],FontSize->smallFontSize,arrowColor,FontFamily->fontFam]]&@@@textList;

(* arrows bordering the charts *)
fibresStatic=tubeArrowPlot@ParametricPlot3D[moebius[u,#],{u,-1,1},PlotStyle->fibreColor]&/@{phi1A,phi2A,phi3A,phi4A};

(* charts on moebius strip *)
moebiusStripNeigh1=ParametricPlot3D[moebius[u,v],{u,-1,1},{v,phi1A,phi2A},PlotStyle->stripColor,MaxRecursion->maxRec];
moebiusStripNeigh2=ParametricPlot3D[moebius[u,v],{u,-1,1},{v,phi3A,phi4A},PlotStyle->stripColor,MaxRecursion->maxRec];

(* neighborhoods on base space *)
neigh1=tubePlot[ParametricPlot3D[circle[v],{v,phi1A,phi2A},PlotStyle->ringColor],tubeWidthFat];
neigh2=tubePlot[ParametricPlot3D[circle[v],{v,phi3A,phi4A},PlotStyle->ringColor],tubeWidthFat];

(* first image *)
img1=Show[{moebiusStripTransparent,ring,fibre,textStrip,textRing,textFibre,textRingPoints[[1]],textRingPoints[[2]],fibresStatic[[1]],fibresStatic[[2]],moebiusStripNeigh1,neigh1},PlotRange->plotRange,ViewPoint->viewPoint,Boxed->False,Axes->False];

(* second image *)
img2=Show[{moebiusStripTransparent,ring,fibre,textStrip,textRing,textFibre,textRingPoints[[3]],textRingPoints[[4]],fibresStatic[[3]],fibresStatic[[4]],moebiusStripNeigh2,neigh2},PlotRange->plotRange,ViewPoint->viewPoint,Boxed->False,Axes->False];

(* rasterize images *)
img1=Rasterize[img1,ImageResolution->imgResolution];
img2=Rasterize[img2,ImageResolution->imgResolution];

(* put everything together, sorry for the messy labels *)
trivializationStringList={"\!\(\*SubscriptBox[\(\[CurlyPhi]\), \(1\)]\):\!\(\*SubscriptBox[\(U\
\), \(1\)]\)\[Times]F\[Rule]\!\(\*SuperscriptBox[\(\[Pi]\), \
\((\(-1\))\)]\)(\!\(\*SubscriptBox[\(U\), \(1\)]\))", 
  "\!\(\*SubscriptBox[\(\[CurlyPhi]\), \
\(2\)]\):\!\(\*SubscriptBox[\(U\), \
\(2\)]\)\[Times]F\[Rule]\!\(\*SuperscriptBox[\(\[Pi]\), \((\(-1\))\)]\
\)(\!\(\*SubscriptBox[\(U\), \(2\)]\))"};
imgChart=Grid[{{img1,img2},Style[Text[#],FontSize->rastFonSize,FontFamily->fontFam]&/@trivializationStringList}];



(* ::Input:: *)
(*(* export and show *)*)
(*Export[NotebookDirectory[]<>"/fibre_bundle_chart.gif",imgChart,ImageResolution->imgResolution]*)
(*imgChart*)
(**)
