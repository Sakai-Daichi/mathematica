(* ::Package:: *)

Clear[CharacterstoHexString2];
CharacterstoHexString2[string_,flag_]:=Module[
  {len,array},
  If[flag==0, 
  len=StringLength[string];
  array=ToCharacterCode[string],
  len=Length[ToCharacterCode[string,"ShiftJIS"]];
  array=ToCharacterCode[string,"ShiftJIS"]
  ];
   Return[IntegerString[FromDigits[array,256],16,2*len]]
];


Clear[HexStringtoCharacters2];
HexStringtoCharacters2[hexstring_,flag_]:=Module[
  {c,clen,hexdata,hexarray,mbcount,outdata},
  
  clen=StringLength[hexstring];

  If[flag==0,
    c=1;outdata="";
    For[,c<=clen,c+=2,
      hexdata=StringTake[hexstring,{c,c+1}];
      outdata=outdata<>FromCharacterCode[FromDigits[hexdata,16]]
    ],
    c=1;outdata="";hexarray={};mbcount=0;
    For[,c<=clen,c+=2,
	  mbcount=Mod[mbcount,2]+1;
      hexdata=FromDigits[StringTake[hexstring,{c,c+1}],16];
	  If[mbcount==1 && hexdata<2^7,
		outdata=outdata<>FromCharacterCode[hexarray,"ShiftJIS"];
		outdata=outdata<>FromCharacterCode[hexdata];
		hexarray = {};
		mbcount=0;,
	    hexarray=Append[hexarray,hexdata];
  	];
    ];
	outdata=outdata<>FromCharacterCode[hexarray,"ShiftJIS"]
  ];
  Return[outdata];
];

