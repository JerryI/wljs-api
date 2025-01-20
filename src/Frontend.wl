BeginPackage["Notebook`ExternalAPI`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Events`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`Notebook`AppExtensions`",
    "JerryI`Misc`WLJS`Transport`",
    "JerryI`WLJSPM`",
    "KirillBelov`HTTPHandler`",
    "KirillBelov`HTTPHandler`Extensions`",
    "KirillBelov`Internal`"    
}]

Begin["`Internal`"]

apiCall[request_] := With[{type = request["Path"]},
    Echo["API Request >> "<>type];
    Echo[ToString[request, StandardForm] ];

    ExportString[apiCall[request, type], "JSON"]
]

apiCall[request_, "/api/kernels"~~___] := With[{},
    "Hey There"
];


With[{http = AppExtensions`HTTPHandler},
    http["MessageHandler", "ExternalAPI"] = AssocMatchQ[<|"Path" -> ("/api/"~~___)|>] -> apiCall;
];

End[]
EndPackage[]

