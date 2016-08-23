(* ::Package:: *)

Clear[RandomSeccap2];
RandomSeccap2[Len_,seed_,type_]:=Module[
  {path,randomexe,command,RandomData,RandomArray,i},

  path="C:\\Users\\daichi-s\\Dropbox\\SecCap\\SecCap\:30bd\:30fc\:30b9";
  randomexe="rngSeccap_win_ver1.exe";

  command=ToFileName[path,randomexe]<>
     ToString[StringForm[" `` ",Len]]<>seed;

  (*Print[command];*)
  RandomData=RunThrough[command,Null];
  (*Print[RandomData];*)
  If[type==0,Return[RandomData]];
  If[type==1,Return[FromDigits[RandomData,16]]];
]
