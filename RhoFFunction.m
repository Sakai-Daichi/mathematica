(* ::Package:: *)

(* c={c,m,n}, c=Mod[ModBinary[m,a,p]*ModBinary[n,b,p],p] *)
RhoF[c_,a_,b_,l_,p_]:=Module[
{},
If[c[[1]]<p/3,Return[{Mod[c[[1]]*b,p],Mod[c[[2]],l],Mod[c[[3]]+1,l]}]];
If[c[[1]]>=p*2/3,Return[{Mod[c[[1]]*a,p],Mod[c[[2]]+1,l],Mod[c[[3]],l]}]];
Return[{Mod[c[[1]]*c[[1]],p],Mod[c[[2]]*2,l],Mod[c[[3]]*2,l]}]
];



Clear[Rho2CSV]
Rho2CSV[a_,b_,l_,p_,m0_,n0_,tmax_,fn_]:=Module[{c0,c1,m1,n1,i,fs},
  c0=Mod[ModBinary[m0,a,p]*ModBinary[n0,b,p],p];
  If[!FileExistsQ[fn],
	fs = OpenWrite[fn];
	Close[fs];
  ];
  {c1,m1,n1}=RhoF[{c0,m0,n0},a,b,l,p];
  fs = OpenAppend[fn];
  For[i=1,i<=tmax,i++,
	WriteString[fs,ExportString[{{c1,m1,n1}},"CSV"]<>"\n"];
    {c1,m1,n1}=RhoF[{c1,m1,n1},a,b,l,p];
  ];
  Close[fs];
  Return[False];
];

Rho2CSV[3,12020,10061,20123,0,0,50,FileNameJoin[{$UserDocumentsDirectory,"rhoftest.csv"}]];