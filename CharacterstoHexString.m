(* ::Package:: *)

Clear[CharacterstoHexString];
CharacterstoHexString[string_]:=Module[
  {len,array},
  len=StringLength[string];
  array=ToCharacterCode[string]; 
  Return[IntegerString[FromDigits[array,256],16,2*len]];
];

Clear[HexStringtoCharacters];
HexStringtoCharacters[hexstring_]:=Module[
  {c,clen,hexdata,outdata},
  
  clen=StringLength[hexstring];

  If[BitAnd[clen,1]==1,
    hexdata=StringTake[hexstring,{1}];
  outdata=FromCharacterCode[FromDigits[hexdata,16]];c=2,
  outdata="";c=1];
  For[,c<=clen,c+=2,
    hexdata=StringTake[hexstring,{c,c+1}];
    outdata=outdata<>FromCharacterCode[FromDigits[hexdata,16]]
  ];
  Return[outdata];
];
