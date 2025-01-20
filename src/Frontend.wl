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


apiCall[request_, "/api/frontendobjects/"] := {
    "/api/frontendobjects/get/",
    "/api/frontendobjects/list/"
}


apiCall[request_, "/api/frontendobjects/get/"] := With[{uid = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]["UId"]},

]


apiCall[request_, "/api/transactions/"] := {
    "/api/transactions/create/",
    "/api/transactions/get/",
    "/api/transactions/delete/",
    "/api/transactions/list/"
}

transactions = {};

apiCall[request_, "/api/transactions/create/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{
        k = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Kernel"]) &]
    },
        If[MissingQ[k], $Failed,
            submitTransaction[body["Data"], k];
            True
        ]
    ]
]

apiCall[request_, "/api/transactions/delete/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{
        m = SelectFirst[transactions, (#["Hash"] === body["Hash"]) &]
    },
        If[MissingQ[m], $Failed,
            transactions = transactions /. {m -> Nothing};
            True
        ]
    ]
]

apiCall[request_, "/api/transactions/get/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{
        m = SelectFirst[transactions, (#["Hash"] === body["Hash"]) &]
    },
        If[MissingQ[m], $Failed,
            <|
                "Hash" -> #["Hash"],
                "State" -> If[StringQ[#["State"] ], #["State"], "Undefined" ],
                "Result" -> If[ListQ[#["Result"] ], #["Result"], {} ]
            |>&@m
        ]
    ]
]

apiCall[request_, "/api/transactions/list/"] := With[{},
    With[{
        
    },
            <|
                "Hash" -> #["Hash"],
                "State" -> If[StringQ[#["State"] ], #["State"], "Undefined" ],
                "Result" -> If[ListQ[#["Result"] ], #["Result"], {} ]
            |>& /@ transactions
    ]
]

submitTransaction[data_String, kernel_] := With[{transaction = Transaction[]},
   transactions = Append[transactions, transaction];
   transaction["Data"] = data;
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

apiCall[request_, "/api/kernels/create/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    "Not implemented"
];

apiCall[request_, "/api/kernels/unlink/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    "Not implemented"
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
            initKernel[<|"env" -> $Env|>][m];
            True
        ]
    ]
];

apiCall[request_, "/api/kernels/deinit/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{m = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Hash"]) &]},
        If[MissingQ[m], $Failed,
            deinitKernel[m];
            True
        ]
    ]
];

{deinitKernel, initKernel}           = ImportComponent["KernelUtils.wl"];


With[{http = AppExtensions`HTTPHandler},
    http["MessageHandler", "ExternalAPI"] = AssocMatchQ[<|"Path" -> ("/api/"~~___)|>] -> apiCall;
];

End[]
EndPackage[]

