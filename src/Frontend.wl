BeginPackage["Notebook`ExternalAPI`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Async`",
    "JerryI`Misc`Events`",
    "JerryI`Notebook`Evaluator`",
    "JerryI`Misc`Events`Promise`",
    "JerryI`Notebook`AppExtensions`",
    "JerryI`Misc`WLJS`Transport`",
    "JerryI`WLJSPM`",
    "KirillBelov`HTTPHandler`",
    "KirillBelov`HTTPHandler`Extensions`",
    "KirillBelov`Internal`",
    "JerryI`Notebook`Kernel`",
    "Notebook`Editor`FrontendObject`"
}]

Begin["`Internal`"]

apiCall[request_] := With[{type = request["Path"]},
    Echo["API Request >> "<>type];
    Echo[ToString[request, InputForm] ];

    ExportString[apiCall[request, type], "JSON"]
]

apiCall[_, _] := "Undefined API pattern"


apiCall[request_, "/api/transactions/"] := {
    "/api/transactions/create",
    "/api/transactions/submit",
    "/api/transactions/get",
    "/api/transactions/delete",
    "/api/transactions/list"
}



submitTransaction[t_, kernel_] := With[{transaction = Transaction[]},
   transaction["State"] = "Evaluation";
   transaction["Result"] = {};   
   transaction["EvaluationContext"] = <||>;

   EventHandler[transaction, {"Result" -> Function[data,
       (* AFTER, BEFORE, TYPE, PROPS can be altered using provided meta-data from the transaction *)

       If[data["Data"] != "Null",
           If[KeyExistsQ[data, "Meta"],
               transaction["Result"] = Append[transaction["Result"], <|"Data"->data["Data"], data["Meta"], "Type"->"Output"(*"" data["Meta"]*)|> ]
               
           ,
               transaction["Result"] = Append[transaction["Result"], <|"Data"->data["Data"], "Display"->"codemirror", "Type"->"Output"(*"" data["Meta"]*)|> ]
               
           ]
       ];
   ],
       "Finished" -> Function[Null,
           transaction["State"] = "Idle";
           Echo["Finished!"];
       ],

       "Error" -> Function[error,
           transaction["State"] = "Error";
           Echo["Error in evalaution... check syntax"];
       ]
   }];

   (* submit *)
   kernel["Container"][transaction];   
]



apiCall[request_, "/api/kernels/"] := {
    "/api/kernels/list/",
    "/api/kernels/restart/",
    "/api/kernels/abort/",
    "/api/kernels/get/",
    "/api/kernels/create/",
    "/api/kernels/unlink/",
    "/api/kernels/init/",
    "/api/kernels/deinit/"
}

apiCall[request_, "/api/kernels/list/"] := With[{},
    <|
        "Hash"->#["Hash"], 
        "State"->#["State"], 
        "ReadyQ"->#["ReadyQ"], 
        "Name"->#["Name"],
        "ContainerReadyQ" -> TrueQ[#["ContainerReadyQ"] ]
    |> &/@ AppExtensions`KernelList
];

apiCall[request_, "/api/kernels/get/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            <|
                "Hash"->#["Hash"], 
                "State"->#["State"], 
                "ReadyQ"->#["ReadyQ"], 
                "Name"->#["Name"],
                "ContainerReadyQ" -> TrueQ[#["ContainerReadyQ"] ]
            |> & @ m
        ]
    ]
];

apiCall[request_, "/api/kernels/restart/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            Kernel`Restart[m];
            True
        ]
    ]
];

apiCall[request_, "/api/kernels/create/"] := With[{},
    Kernel`Restart[m];
];

apiCall[request_, "/api/kernels/unlink/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            Kernel`Unlink[m];
            True
        ]
    ]
];

apiCall[request_, "/api/kernels/abort/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            Kernel`Abort[m];
            True
        ]
    ]
];

apiCall[request_, "/api/kernels/init/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            initializeKernel[m];
            True
        ]
    ]
];

apiCall[request_, "/api/kernels/deinit/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            deinitializeKernel[m];
            True
        ]
    ]
];

initializeKernel[kernel_] := With[{
  wsPort = $Env["ws2"], 
},
  Print["Init Kernel!!!"];

  (* load kernel packages and provide remote path *)
  With[{
    p = Import[FileNameJoin[{"wljs_packages", #}], "String"], 
    path = ToString[URLBuild[<|"Scheme" -> "http", "Domain" -> (StringTemplate["``:``"][With[{h =  $Env["host"]}, If[h === "0.0.0.0", "127.0.0.1", h] ], $Env["http"] ]), "Path" -> StringRiffle[Drop[FileNameSplit[#], -2], "/"]|> ], InputForm],
    dir = FileNameJoin[{Directory[], "wljs_packages", #}]
  },
    Echo[StringJoin["Loading into Kernel... ", #] ];
    Echo[kernel];
    Echo[kernel["LTPSocket"] ];
    
    (*fixme apply post-processor for remote paths*)
    With[{processed = StringReplace[p, "$RemotePackageDirectory" -> ("Internal`RemoteFS["<>path<>"]")]},      
      Kernel`Async[kernel,  ToExpression[processed, InputForm] ](*`*);
    ];

  ] &/@ WLJS`PM`Includes["kernel"];

  kernel["Container"] = StandardEvaluator`Container[kernel](*`*);
  kernel["ContainerReadyQ"] = True;

  kernel["State"] = "Initialized";

  With[{hash = kernel["Hash"]},
    Kernel`Init[kernel,  EventFire[Internal`Kernel`Stdout[ hash ], "State", "Initialized" ]; ];
  ];
]

deinitializeKernel[kernel_] := With[{},
  Echo["Cleaning up..."];

  kernel["ContainerReadyQ"] = False;
  kernel["WebSocket"] = .;
]


With[{http = AppExtensions`HTTPHandler},
    http["MessageHandler", "ExternalAPI"] = AssocMatchQ[<|"Path" -> ("/api/"~~___)|>] -> apiCall;
];

End[]
EndPackage[]

