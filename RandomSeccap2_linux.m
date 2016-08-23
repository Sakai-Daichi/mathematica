(* ::Package:: *)

Clear[RandomSeccap2];
RandomSeccap2[Len_,seed_,type_]:=Module[
  {path,randomexe,command,RandomData,RandomArray,i},

  path="/home/futa/mathematica/provided";
  randomexe="rngSeccap_linux_ver1.exe";

  command=ToFileName[path,randomexe]<>
     ToString[StringForm[" `` ",Len]]<>seed;

  (*Print[command];*)
  RandomData=RunThrough[command,Null];
  (*Print[RandomData];*)
  If[type==0,Return[RandomData]];
  If[type==1,Return[FromDigits[RandomData,16]]];
]
