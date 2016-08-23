(* ::Package:: *)

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


Clear[MGF];
MGF[D_,dLen_]:=Module[
  {Dchar,c1,c2,indata,outdata},
(*D\:306f16\:9032\:6587\:5b57\:5217*)
(*dLen\:306f\:6574\:6570*)
  Dchar=HexStringtoCharacters[D];
  c1=Floor[dLen/20];
  indata=Dchar<>FromCharacterCode[{0,0,0,0}];
  outdata=Hash[indata,"SHA"];
  For[c2=1,c2<=c1,c2++,
    indata=Dchar<>FromCharacterCode[{0,0,0,c2}];
    outdata=outdata*2^160+Hash[indata,"SHA"];
  ];
  outdata=BitShiftRight[outdata,(c1+1)*160-dLen*8]; (*\:4e0a\:4f4ddLen\:30d0\:30a4\:30c8\:3092\:53d6\:5f97*)
  Return[IntegerString[outdata,16,2*dLen]];
];

Clear[PrimeGen];
PrimeGen[pBitLen_]:=Module[
  {r},

  r=Random[Integer,2^(pBitLen-1)-1];
  r=BitOr[r,2^(pBitLen-1)];
  While[!PrimeQ[r],
    r=Random[Integer,2^(pBitLen-1)-1];
    r=BitOr[r,2^(pBitLen-1)];
  ];

Return[r];
];

