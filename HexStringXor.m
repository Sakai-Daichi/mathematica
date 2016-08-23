(* ::Package:: *)

Clear[HexStringXor];
HexStringXor[S1_,S2_,Len_]:=Module[{temp},
   (* S1\:3068S2\:306eXOR\:3092\:8a08\:7b97 *)
   (* Len\:306fS1\:3068S2\:306e\:30d0\:30a4\:30c8\:30b5\:30a4\:30ba *)
Return[IntegerString[BitXor[FromDigits[S1,16],FromDigits[S2,16]],16,2*Len]];
];

