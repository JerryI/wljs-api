BeginPackage["Notebook`ExternalAPI`", {
    "JerryI`Notebook`", 
    "JerryI`Misc`Async`",
    "JerryI`Misc`Events`",
    "JerryI`Notebook`Transactions`",
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

    ExportString[apiCall[request, type], "JSON"]
]

apiCall[_, _] := "Undefined API pattern"

apiCall[request_, "/api/"] := {
    "/api/kernels/",
    "/api/transactions/",
    "/api/frontendobjects/",
    "/api/extensions/",
    "/api/ready/"
}

apiCall[request_, "/api/ready/"] := <|"ReadyQ" -> True|>


apiCall[request_, "/api/frontendobjects/"] := {
    "/api/frontendobjects/get/"
}


objects = <||>;

apiCall[request_, "/api/frontendobjects/get/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    With[{
        k = SelectFirst[AppExtensions`KernelList, (#["Hash"] === body["Kernel"]) &],
        uid = body["UId"],
        promise = Promise[]
    },
        If[!KeyExistsQ[objects, uid],
            If[MissingQ[k], $Failed, 
                With[{
                    promiseId = promise // First
                },
                    Kernel`Async[k, With[{o = Notebook`Editor`FrontendObject`GetObject[uid]},
                            EventFire[Internal`Kernel`Stdout[promiseId], Resolve, ExportString[o, "ExpressionJSON", "Compact"->1] ];
                        ]
                    ];
                ];

                objects[uid] = <|"Resolved" -> False|>;

                Then[promise, Function[data,
                    objects[uid] = Join[objects[uid], <|"Resolved" -> True,
                                                        "Data" -> data|>
                    ];
                ] ];

                objects[uid]

            ]
        ,
            objects[uid]
        ]
    ]    
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
            submitTransaction[body["Data"], k]
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
                "State" -> If[StringQ[#["State"] ], #["State"], "Undefined" ]
            |>& /@ transactions
    ]
]

submitTransaction[input_String, kernel_] := With[{transaction = Transaction[]},
   transactions = Append[transactions, transaction];
   transaction["Data"] = input;
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
   transaction["Hash"]
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


apiCall[request_, "/api/extensions/"] := {
    "/api/extensions/list/",
    "/api/extensions/get/minjs/",
    "/api/extensions/get/styles/"
}

apiCall[request_, "/api/extensions/list/"] := With[{},
    Map[Function[key, 
        <|"name" -> key, "version" -> WLJS`PM`Packages[key, "version"]|>
    ], 
        Select[WLJS`PM`Packages // Keys, (WLJS`PM`Packages[#, "enabled"] && KeyExistsQ[WLJS`PM`Packages[#, "wljs-meta"], "minjs"]) &] 
    ]
]

pmIncludes[param_, whitelist_List] := 
Table[ 
    Table[ 
      Import[FileNameJoin[{"wljs_packages", WLJS`PM`Packages[i, "name"], StringSplit[j, "/"]} // Flatten], "Text"] // URLEncode
    , {j, {WLJS`PM`Packages[i, "wljs-meta", param]} // Flatten} ]
, {i, Select[WLJS`PM`Packages // Keys, (MemberQ[whitelist, #] && WLJS`PM`Packages[#, "enabled"] && KeyExistsQ[WLJS`PM`Packages[#, "wljs-meta"], param])&]}] // Flatten;

apiCall[request_, "/api/extensions/get/minjs/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    pmIncludes["minjs", Flatten[{body}] ]
]

apiCall[request_, "/api/extensions/get/styles/"] := With[{body = ImportString[ByteArrayToString[request["Body"] ], "RawJSON"]},
    pmIncludes["styles", Flatten[{body}] ]
]


With[{http = AppExtensions`HTTPHandler},
    http["MessageHandler", "ExternalAPI"] = AssocMatchQ[<|"Path" -> ("/api/"~~___)|>] -> apiCall;
];

End[]
EndPackage[]

