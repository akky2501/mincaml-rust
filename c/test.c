/*
src:
let rec f x = 
                    let rec g y = 
                        let rec h z = x*y+z in h 
                    in g
                in
                let x = f 7 in
                let y = x 5 in
                y 10 + (x 7) 1
[[parse phase... begin]]
ast:
LetRec(
    FunDef {
        name: (
            Id(
                "f"
            ),
            Var(
                4
            )
        ),
        args: [
            (
                Id(
                    "x"
                ),
                Var(
                    5
                )
            )
        ],
        body: LetRec(
            FunDef {
                name: (
                    Id(
                        "g"
                    ),
                    Var(
                        2
                    )
                ),
                args: [
                    (
                        Id(
                            "y"
                        ),
                        Var(
                            3
                        )
                    )
                ],
                body: LetRec(
                    FunDef {
                        name: (
                            Id(
                                "h"
                            ),
                            Var(
                                0
                            )
                        ),
                        args: [
                            (
                                Id(
                                    "z"
                                ),
                                Var(
                                    1
                                )
                            )
                        ],
                        body: Add(
                            Mul(
                                Var(
                                    Id(
                                        "x"
                                    )
                                ),
                                Var(
                                    Id(
                                        "y"
                                    )
                                )
                            ),
                            Var(
                                Id(
                                    "z"
                                )
                            )
                        )
                    },
                    Var(
                        Id(
                            "h"
                        )
                    )
                )
            },
            Var(
                Id(
                    "g"
                )
            )
        )
    },
    Let(
        (
            Id(
                "x"
            ),
            Var(
                12
            )
        ),
        App(
            Var(
                Id(
                    "f"
                )
            ),
            [
                Int(
                    7
                )
            ],
            Var(
                6
            )
        ),
        Let(
            (
                Id(
                    "y"
                ),
                Var(
                    11
                )
            ),
            App(
                Var(
                    Id(
                        "x"
                    )
                ),
                [
                    Int(
                        5
                    )
                ],
                Var(
                    7
                )
            ),
            Add(
                App(
                    Var(
                        Id(
                            "y"
                        )
                    ),
                    [
                        Int(
                            10
                        )
                    ],
                    Var(
                        8
                    )
                ),
                App(
                    App(
                        Var(
                            Id(
                                "x"
                            )
                        ),
                        [
                            Int(
                                7
                            )
                        ],
                        Var(
                            9
                        )
                    ),
                    [
                        Int(
                            1
                        )
                    ],
                    Var(
                        10
                    )
                )
            )
        )
    )
)

[[parse phase... end]]

[[alpha transform phase... begin]]
alpha transformed ast:
LetRec(
    FunDef {
        name: (
            Id(
                "@13"
            ),
            Var(
                4
            )
        ),
        args: [
            (
                Id(
                    "@14"
                ),
                Var(
                    5
                )
            )
        ],
        body: LetRec(
            FunDef {
                name: (
                    Id(
                        "@15"
                    ),
                    Var(
                        2
                    )
                ),
                args: [
                    (
                        Id(
                            "@16"
                        ),
                        Var(
                            3
                        )
                    )
                ],
                body: LetRec(
                    FunDef {
                        name: (
                            Id(
                                "@17"
                            ),
                            Var(
                                0
                            )
                        ),
                        args: [
                            (
                                Id(
                                    "@18"
                                ),
                                Var(
                                    1
                                )
                            )
                        ],
                        body: Add(
                            Mul(
                                Var(
                                    Id(
                                        "@14"
                                    )
                                ),
                                Var(
                                    Id(
                                        "@16"
                                    )
                                )
                            ),
                            Var(
                                Id(
                                    "@18"
                                )
                            )
                        )
                    },
                    Var(
                        Id(
                            "@17"
                        )
                    )
                )
            },
            Var(
                Id(
                    "@15"
                )
            )
        )
    },
    Let(
        (
            Id(
                "@19"
            ),
            Var(
                12
            )
        ),
        App(
            Var(
                Id(
                    "@13"
                )
            ),
            [
                Int(
                    7
                )
            ],
            Var(
                6
            )
        ),
        Let(
            (
                Id(
                    "@20"
                ),
                Var(
                    11
                )
            ),
            App(
                Var(
                    Id(
                        "@19"
                    )
                ),
                [
                    Int(
                        5
                    )
                ],
                Var(
                    7
                )
            ),
            Add(
                App(
                    Var(
                        Id(
                            "@20"
                        )
                    ),
                    [
                        Int(
                            10
                        )
                    ],
                    Var(
                        8
                    )
                ),
                App(
                    App(
                        Var(
                            Id(
                                "@19"
                            )
                        ),
                        [
                            Int(
                                7
                            )
                        ],
                        Var(
                            9
                        )
                    ),
                    [
                        Int(
                            1
                        )
                    ],
                    Var(
                        10
                    )
                )
            )
        )
    )
)
[[alpha transform phase... end]]

[[typing phase... begin]]
type: Int
env:
TypeEnv {
    seq: []
}
subst:
TypeSubst {
    equations: [
        (
            5,
            Var(
                3
            )
        ),
        (
            3,
            Int
        ),
        (
            1,
            Int
        ),
        (
            0,
            Fun(
                [
                    Int
                ],
                Int
            )
        ),
        (
            2,
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            )
        ),
        (
            4,
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            )
        ),
        (
            21,
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            )
        ),
        (
            12,
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            )
        ),
        (
            22,
            Fun(
                [
                    Int
                ],
                Int
            )
        ),
        (
            11,
            Fun(
                [
                    Int
                ],
                Int
            )
        ),
        (
            23,
            Int
        ),
        (
            24,
            Fun(
                [
                    Int
                ],
                Int
            )
        ),
        (
            25,
            Int
        )
    ]
}
typed ast:
LetRec(
    FunDef {
        name: (
            Id(
                "@13"
            ),
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            )
        ),
        args: [
            (
                Id(
                    "@14"
                ),
                Int
            )
        ],
        body: LetRec(
            FunDef {
                name: (
                    Id(
                        "@15"
                    ),
                    Fun(
                        [
                            Int
                        ],
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    )
                ),
                args: [
                    (
                        Id(
                            "@16"
                        ),
                        Int
                    )
                ],
                body: LetRec(
                    FunDef {
                        name: (
                            Id(
                                "@17"
                            ),
                            Fun(
                                [
                                    Int
                                ],
                                Int
                            )
                        ),
                        args: [
                            (
                                Id(
                                    "@18"
                                ),
                                Int
                            )
                        ],
                        body: Add(
                            Mul(
                                Var(
                                    Id(
                                        "@14"
                                    )
                                ),
                                Var(
                                    Id(
                                        "@16"
                                    )
                                )
                            ),
                            Var(
                                Id(
                                    "@18"
                                )
                            )
                        )
                    },
                    Var(
                        Id(
                            "@17"
                        )
                    )
                )
            },
            Var(
                Id(
                    "@15"
                )
            )
        )
    },
    Let(
        (
            Id(
                "@19"
            ),
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            )
        ),
        App(
            Var(
                Id(
                    "@13"
                )
            ),
            [
                Int(
                    7
                )
            ],
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            )
        ),
        Let(
            (
                Id(
                    "@20"
                ),
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            ),
            App(
                Var(
                    Id(
                        "@19"
                    )
                ),
                [
                    Int(
                        5
                    )
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            ),
            Add(
                App(
                    Var(
                        Id(
                            "@20"
                        )
                    ),
                    [
                        Int(
                            10
                        )
                    ],
                    Int
                ),
                App(
                    App(
                        Var(
                            Id(
                                "@19"
                            )
                        ),
                        [
                            Int(
                                7
                            )
                        ],
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    ),
                    [
                        Int(
                            1
                        )
                    ],
                    Int
                )
            )
        )
    )
)
substituted type: Int
[[typing phase... end]]

[[knormal transform phase... begin]]
k-normal form:
LetRec(
    FunDef {
        name: (
            Id(
                "@13"
            ),
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            )
        ),
        args: [
            (
                Id(
                    "@14"
                ),
                Int
            )
        ],
        body: LetRec(
            FunDef {
                name: (
                    Id(
                        "@15"
                    ),
                    Fun(
                        [
                            Int
                        ],
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    )
                ),
                args: [
                    (
                        Id(
                            "@16"
                        ),
                        Int
                    )
                ],
                body: LetRec(
                    FunDef {
                        name: (
                            Id(
                                "@17"
                            ),
                            Fun(
                                [
                                    Int
                                ],
                                Int
                            )
                        ),
                        args: [
                            (
                                Id(
                                    "@18"
                                ),
                                Int
                            )
                        ],
                        body: Let(
                            (
                                Id(
                                    "@28"
                                ),
                                Int
                            ),
                            Mul(
                                Id(
                                    "@14"
                                ),
                                Id(
                                    "@16"
                                )
                            ),
                            Add(
                                Id(
                                    "@28"
                                ),
                                Id(
                                    "@18"
                                )
                            )
                        )
                    },
                    Var(
                        Id(
                            "@17"
                        )
                    )
                )
            },
            Var(
                Id(
                    "@15"
                )
            )
        )
    },
    Let(
        (
            Id(
                "@19"
            ),
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            )
        ),
        Let(
            (
                Id(
                    "@31"
                ),
                Int
            ),
            Int(
                7
            ),
            App(
                Id(
                    "@13"
                ),
                [
                    Id(
                        "@31"
                    )
                ]
            )
        ),
        Let(
            (
                Id(
                    "@20"
                ),
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            ),
            Let(
                (
                    Id(
                        "@33"
                    ),
                    Int
                ),
                Int(
                    5
                ),
                App(
                    Id(
                        "@19"
                    ),
                    [
                        Id(
                            "@33"
                        )
                    ]
                )
            ),
            Let(
                (
                    Id(
                        "@40"
                    ),
                    Int
                ),
                Let(
                    (
                        Id(
                            "@35"
                        ),
                        Int
                    ),
                    Int(
                        10
                    ),
                    App(
                        Id(
                            "@20"
                        ),
                        [
                            Id(
                                "@35"
                            )
                        ]
                    )
                ),
                Let(
                    (
                        Id(
                            "@41"
                        ),
                        Int
                    ),
                    Let(
                        (
                            Id(
                                "@38"
                            ),
                            Fun(
                                [
                                    Int
                                ],
                                Int
                            )
                        ),
                        Let(
                            (
                                Id(
                                    "@37"
                                ),
                                Int
                            ),
                            Int(
                                7
                            ),
                            App(
                                Id(
                                    "@19"
                                ),
                                [
                                    Id(
                                        "@37"
                                    )
                                ]
                            )
                        ),
                        Let(
                            (
                                Id(
                                    "@39"
                                ),
                                Int
                            ),
                            Int(
                                1
                            ),
                            App(
                                Id(
                                    "@38"
                                ),
                                [
                                    Id(
                                        "@39"
                                    )
                                ]
                            )
                        )
                    ),
                    Add(
                        Id(
                            "@40"
                        ),
                        Id(
                            "@41"
                        )
                    )
                )
            )
        )
    )
)
k-normal form type:
Int
flatted k-normal form:
LetRec(
    FunDef {
        name: (
            Id(
                "@13"
            ),
            Fun(
                [
                    Int
                ],
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            )
        ),
        args: [
            (
                Id(
                    "@14"
                ),
                Int
            )
        ],
        body: LetRec(
            FunDef {
                name: (
                    Id(
                        "@15"
                    ),
                    Fun(
                        [
                            Int
                        ],
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    )
                ),
                args: [
                    (
                        Id(
                            "@16"
                        ),
                        Int
                    )
                ],
                body: LetRec(
                    FunDef {
                        name: (
                            Id(
                                "@17"
                            ),
                            Fun(
                                [
                                    Int
                                ],
                                Int
                            )
                        ),
                        args: [
                            (
                                Id(
                                    "@18"
                                ),
                                Int
                            )
                        ],
                        body: Let(
                            (
                                Id(
                                    "@28"
                                ),
                                Int
                            ),
                            Mul(
                                Id(
                                    "@14"
                                ),
                                Id(
                                    "@16"
                                )
                            ),
                            Add(
                                Id(
                                    "@28"
                                ),
                                Id(
                                    "@18"
                                )
                            )
                        )
                    },
                    Var(
                        Id(
                            "@17"
                        )
                    )
                )
            },
            Var(
                Id(
                    "@15"
                )
            )
        )
    },
    Let(
        (
            Id(
                "@31"
            ),
            Int
        ),
        Int(
            7
        ),
        Let(
            (
                Id(
                    "@19"
                ),
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            ),
            App(
                Id(
                    "@13"
                ),
                [
                    Id(
                        "@31"
                    )
                ]
            ),
            Let(
                (
                    Id(
                        "@33"
                    ),
                    Int
                ),
                Int(
                    5
                ),
                Let(
                    (
                        Id(
                            "@20"
                        ),
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    ),
                    App(
                        Id(
                            "@19"
                        ),
                        [
                            Id(
                                "@33"
                            )
                        ]
                    ),
                    Let(
                        (
                            Id(
                                "@35"
                            ),
                            Int
                        ),
                        Int(
                            10
                        ),
                        Let(
                            (
                                Id(
                                    "@40"
                                ),
                                Int
                            ),
                            App(
                                Id(
                                    "@20"
                                ),
                                [
                                    Id(
                                        "@35"
                                    )
                                ]
                            ),
                            Let(
                                (
                                    Id(
                                        "@37"
                                    ),
                                    Int
                                ),
                                Int(
                                    7
                                ),
                                Let(
                                    (
                                        Id(
                                            "@38"
                                        ),
                                        Fun(
                                            [
                                                Int
                                            ],
                                            Int
                                        )
                                    ),
                                    App(
                                        Id(
                                            "@19"
                                        ),
                                        [
                                            Id(
                                                "@37"
                                            )
                                        ]
                                    ),
                                    Let(
                                        (
                                            Id(
                                                "@39"
                                            ),
                                            Int
                                        ),
                                        Int(
                                            1
                                        ),
                                        Let(
                                            (
                                                Id(
                                                    "@41"
                                                ),
                                                Int
                                            ),
                                            App(
                                                Id(
                                                    "@38"
                                                ),
                                                [
                                                    Id(
                                                        "@39"
                                                    )
                                                ]
                                            ),
                                            Add(
                                                Id(
                                                    "@40"
                                                ),
                                                Id(
                                                    "@41"
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)
[[knormal transform phase... end]]

[[closure transform phase... begin]]
known set {Label("@13")}
program:
Program {
    decls: [
        Function {
            entry: (
                Label(
                    "@17"
                ),
                Fun(
                    [
                        Int
                    ],
                    Int
                )
            ),
            free_variables: {
                Id(
                    "@14"
                ): Int,
                Id(
                    "@16"
                ): Int
            },
            args: [
                (
                    Id(
                        "@18"
                    ),
                    Int
                )
            ],
            body: Let(
                (
                    Id(
                        "@28"
                    ),
                    Int
                ),
                Mul(
                    Id(
                        "@14"
                    ),
                    Id(
                        "@16"
                    )
                ),
                Add(
                    Id(
                        "@28"
                    ),
                    Id(
                        "@18"
                    )
                )
            )
        },
        Function {
            entry: (
                Label(
                    "@15"
                ),
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            ),
            free_variables: {
                Id(
                    "@14"
                ): Int
            },
            args: [
                (
                    Id(
                        "@16"
                    ),
                    Int
                )
            ],
            body: MakeClosure(
                Id(
                    "@17"
                ),
                Closure {
                    entry: Label(
                        "@17"
                    ),
                    free_variables: {
                        Id(
                            "@16"
                        ),
                        Id(
                            "@14"
                        )
                    }
                },
                Var(
                    Id(
                        "@17"
                    )
                )
            )
        },
        Function {
            entry: (
                Label(
                    "@13"
                ),
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    )
                )
            ),
            free_variables: {},
            args: [
                (
                    Id(
                        "@14"
                    ),
                    Int
                )
            ],
            body: MakeClosure(
                Id(
                    "@15"
                ),
                Closure {
                    entry: Label(
                        "@15"
                    ),
                    free_variables: {
                        Id(
                            "@14"
                        )
                    }
                },
                Var(
                    Id(
                        "@15"
                    )
                )
            )
        }
    ],
    code: Let(
        (
            Id(
                "@31"
            ),
            Int
        ),
        Int(
            7
        ),
        Let(
            (
                Id(
                    "@19"
                ),
                Fun(
                    [
                        Int
                    ],
                    Fun(
                        [
                            Int
                        ],
                        Int
                    )
                )
            ),
            AppDirect(
                Label(
                    "@13"
                ),
                [
                    Id(
                        "@31"
                    )
                ]
            ),
            Let(
                (
                    Id(
                        "@33"
                    ),
                    Int
                ),
                Int(
                    5
                ),
                Let(
                    (
                        Id(
                            "@20"
                        ),
                        Fun(
                            [
                                Int
                            ],
                            Int
                        )
                    ),
                    AppClosure(
                        Id(
                            "@19"
                        ),
                        [
                            Id(
                                "@33"
                            )
                        ]
                    ),
                    Let(
                        (
                            Id(
                                "@35"
                            ),
                            Int
                        ),
                        Int(
                            10
                        ),
                        Let(
                            (
                                Id(
                                    "@40"
                                ),
                                Int
                            ),
                            AppClosure(
                                Id(
                                    "@20"
                                ),
                                [
                                    Id(
                                        "@35"
                                    )
                                ]
                            ),
                            Let(
                                (
                                    Id(
                                        "@37"
                                    ),
                                    Int
                                ),
                                Int(
                                    7
                                ),
                                Let(
                                    (
                                        Id(
                                            "@38"
                                        ),
                                        Fun(
                                            [
                                                Int
                                            ],
                                            Int
                                        )
                                    ),
                                    AppClosure(
                                        Id(
                                            "@19"
                                        ),
                                        [
                                            Id(
                                                "@37"
                                            )
                                        ]
                                    ),
                                    Let(
                                        (
                                            Id(
                                                "@39"
                                            ),
                                            Int
                                        ),
                                        Int(
                                            1
                                        ),
                                        Let(
                                            (
                                                Id(
                                                    "@41"
                                                ),
                                                Int
                                            ),
                                            AppClosure(
                                                Id(
                                                    "@38"
                                                ),
                                                [
                                                    Id(
                                                        "@39"
                                                    )
                                                ]
                                            ),
                                            Add(
                                                Id(
                                                    "@40"
                                                ),
                                                Id(
                                                    "@41"
                                                )
                                            )
                                        )
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
}
[[closure transform phase... end]]

*/


#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

typedef void (*func_type)(void);
typedef long long i64;
typedef long long bool;
typedef unsigned char unit;

struct value {
    void* p;
};

struct closure {
    func_type func;
    i64 fv_num;
    struct value fv[0];
};

int stack_top = -1;
struct value stack_pool[0xffff];

void push(struct value x){
    stack_top++;
    stack_pool[stack_top] = x;
}

struct value pop(void){
    struct value r = stack_pool[stack_top];
    stack_top--;
    return r;
}

struct value make_unit(void){
    struct value r;
    r.p = malloc(sizeof(unit));
    *(unit*)(r.p) = 0xff;
    return r;
}

struct value make_int(i64 i){
    struct value r;
    r.p = malloc(sizeof(i64));
    *(i64*)(r.p) = i;
    return r;
}

struct value make_bool(bool b){
    struct value r;
    r.p = malloc(sizeof(bool));
    *(bool*)(r.p) = b;
    return r;
}

struct value make_closure(func_type func, i64 fv_num, ...){
    struct value r;
    va_list args;

    r.p = malloc(sizeof(struct closure)+sizeof(struct value)*fv_num);
    ((struct closure*)(r.p))->func = func;
    ((struct closure*)(r.p))->fv_num = fv_num;

    va_start(args,fv_num);
    for(int i=0;i<fv_num;i++)
        ((struct closure*)(r.p))->fv[i] = va_arg(args, struct value);
    va_end(args);

    return r;
}


void func17(void){
    struct value var18 = pop(); // arg
    struct value var14 = pop(); // fv
    struct value var16 = pop(); // fv
    push(make_int(*(i64*)(var14.p) * *(i64*)(var16.p)));
    struct value var28 = pop();
    push(make_int(*(i64*)(var28.p) + *(i64*)(var18.p)));
}
void func15(void){
    struct value var16 = pop(); // arg
    struct value var14 = pop(); // fv
    struct value var17 = make_closure( func17, 2, var14, var16);
    push(var17);
}
void func13(void){
    struct value var14 = pop(); // arg
    struct value var15 = make_closure( func15, 1, var14);
    push(var15);
}
void entry_point(void){
    push(make_int(7));
    struct value var31 = pop();
    push(var31);
    func13();
    struct value var19 = pop();
    push(make_int(5));
    struct value var33 = pop();
    for(int i=((struct closure *)(var19.p))->fv_num-1; i >= 0;i--) push(((struct closure *)(var19.p))->fv[i]);
    push(var33);
    ((struct closure *)(var19.p))->func();
    struct value var20 = pop();
    push(make_int(10));
    struct value var35 = pop();
    for(int i=((struct closure *)(var20.p))->fv_num-1; i >= 0;i--) push(((struct closure *)(var20.p))->fv[i]);
    push(var35);
    ((struct closure *)(var20.p))->func();
    struct value var40 = pop();
    push(make_int(7));
    struct value var37 = pop();
    for(int i=((struct closure *)(var19.p))->fv_num-1; i >= 0;i--) push(((struct closure *)(var19.p))->fv[i]);
    push(var37);
    ((struct closure *)(var19.p))->func();
    struct value var38 = pop();
    push(make_int(1));
    struct value var39 = pop();
    for(int i=((struct closure *)(var38.p))->fv_num-1; i >= 0;i--) push(((struct closure *)(var38.p))->fv[i]);
    push(var39);
    ((struct closure *)(var38.p))->func();
    struct value var41 = pop();
    push(make_int(*(i64*)(var40.p) + *(i64*)(var41.p)));
}

void print_result(void){
    struct value r = pop();

    printf("result: %lld\n", *(i64*)(r.p));

}

int main(void){
    entry_point();
    print_result();
    return 0;
}

