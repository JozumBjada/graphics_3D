(* ::Package:: *)

(* ::Title:: *)
(*Spiral phase plate*)


(* ::Subtitle:: *)
(*Wiki image source code*)


(* ::Text:: *)
(*Source code for file: "Spiral_phase_plate.svg"*)
(*https://commons.wikimedia.org/wiki/File:Spiral_phase_plate.svg*)
(*Version: "Wolfram language 12.0.0 for Microsoft Windows (64-bit) (April 6, 2019)"*)


(* ::Section:: *)
(*Code*)


(* ::Input::Initialization:: *)
spp=Module[{num=20,aux,aux2,i=0,step=0.01,offset=0.2,circ,faces,base,segments,verts,end},
circ=CirclePoints[num];
AppendTo[circ,First[circ]];
aux={##,0}&@@@circ;
base=Polygon[aux];
aux2={##,offset+step i++}&@@@circ;
verts={0,0,#}&/@aux2[[;;,-1]];
end={aux2[[1]],verts[[1]],verts[[-1]],aux2[[-1]]};

aux=Partition[aux,2,1];
aux2=Partition[aux2,2,1];
verts=Partition[verts,2,1];

faces=Join[#1,#2[[{-1,1}]]]&@@@Transpose[{aux,aux2}];
segments=Join[#1[[{-1,1}]],#2]&@@@Transpose[{verts,aux2}];

{Polyhedron/@faces,base,Polyhedron/@segments,Polyhedron[end]}
];


(* ::Input::Initialization:: *)
spp=Graphics3D[{(*EdgeForm[],*)Specularity[10],Opacity[.7,Setting@ColorSetter[RGBColor[0.7686274509803922, 0.8196078431372549, 0.9568627450980393]]],Lighting->{{"Ambient",LightGray},{"Directional",White,{-1,-1,15}},{"Directional",White,{1,1,15}}},spp},Boxed->False,Lighting->"Neutral",ViewPoint->{0.68,2.28,2.41},ViewVertical->{0.353,0.22,0.91},ImageSize->2000];


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]]*)


(* ::Input:: *)
(*Export["spp.svg",sppaux]*)
(*Export["spp.png",sppaux,ImageResolution->200]*)
