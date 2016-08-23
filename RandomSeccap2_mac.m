(* ::Package:: *)

Clear[RandomSeccap2];
RandomSeccap2[Len_,Num_,seed_]:=Module[
  {path,randomexe,command,RandomData,RandomArray,i},

  path="/Users/futa/Documents/mathematica/Exp";
  randomexe="rngSeccap_mac_ver1.exe";

  command=ToFileName[path,randomexe]<>
     ToString[StringForm[" `` ",Len]]<>seed;

  (*Print[command];*)
  RandomData=RunThrough[command,Null];
  (*Print[RandomData];*)
  If[type==0,Return[RandomData]];
  If[type==1,Return[FromDigits[RandomData,16]]];
];



