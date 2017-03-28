
extern crate mincaml_rust;

use std::str;
use std::collections::HashMap;

use mincaml_rust::parser::parse;
use mincaml_rust::typing::{infer,TypeSubst,TypeEnv};
use mincaml_rust::alpha::alpha_transform;
use mincaml_rust::knormal::{knormal_transform, flat_let};
use mincaml_rust::closure::closure_transform;

#[allow(unused_variables)]
fn main() {
    let src0 = b"
    let (a, b, c) = (
        let rec id a = a in
        let x = 10 in
        let y = false in
        let z = if y then 100 else -100 in
            (); 
            id x + 42,id y && true,id z 
    ) in if b then a else c
    ";

    let src = b"
        let rec id x = x in
        let rec const x = 
            let rec f y = id x in
            f
        in 
        let rec succ x = x + 1 in
        let rec infinite x = (succ (infinite (succ x))) in
        let rec sum min = 
            let rec helper max =
                let rec loop x = 
                    if x = max then x else x + loop (succ x)
                in loop min 
            in helper
        in
        let main = (sum 0) 10 in
        main
                ";
    let src2 = b"let rec f x = x in (f 1, f true)";
    let src3 = b"let y = (let rec f x = x+1 in f)
                in y";
    let src4 = b"
        let int = 0 in
        let rec S x y z = x z (y z) in
        let rec K x y   = x         in
        let rec I x     = x         in
        (S, K, I)
    ";

    let src5 = b"
        let rec id x = x in
        let a = 1+(let x = (let y = 5+3 in y+y) in x+7) in
        id a
        ";
    let src6 = b"
                let rec add x =
                    let rec addx y = x + y in addx in
                (add 10) 7 
    ";

    println!("src:\n{}",str::from_utf8(src).unwrap());

    println!("[[parse phase... begin]]");
    let (mut ast, mut vg) = parse(src).unwrap();
    println!("ast:\n{:?}\n",ast);
    println!("[[parse phase... end]]\n");

    println!("[[alpha transform phase... begin]]");
    alpha_transform(&mut ast, &mut HashMap::new(), &mut vg).unwrap();
    println!("alpha transformed ast:\n{:?}",ast);
    println!("[[alpha transform phase... end]]\n");
    
    println!("[[typing phase... begin]]");
    let mut subst = TypeSubst::new();
    let mut env = TypeEnv::new();
    let mut ty = infer(&mut ast, &mut env, &mut subst, &mut vg).unwrap();
    println!("type: {:?}",ty);
    println!("env:\n{:?}",env);
    println!("subst:\n{:?}",subst);
    println!("typed ast:\n{:?}",ast);
    ty.apply(&subst);
    println!("substituted type: {:?}",ty);
    /*let rty = ty.generalize(&env);
    if rty.bind.is_empty() {
        println!("pass type check!");
    }
    else {
        println!("failed type check.");
        println!("type scheme: {:?}",rty);
        return ();
    }*/
    println!("[[typing phase... end]]\n");

    println!("[[knormal transform phase... begin]]");
    let (mut k, ty) = knormal_transform(ast, &mut HashMap::new(), &mut vg);
    println!("k-normal form:\n{:?}",k);
    println!("k-normal form type:\n{:?}",ty);
    k = flat_let(k);
    println!("flatted k-normal form:\n{:?}",k);
    println!("[[knormal transform phase... end]]\n");
    
    println!("[[closure transform phase... begin]]");
    let p = closure_transform(k);
    println!("program:\n{:?}", p);
    println!("[[closure transform phase... end]]\n");
}


/*

Program { 
    decls: [
        Function { entry: (Label("@17"), Fun([Var(1)], Var(1))), free_variables: {}, 
            args: [(Id("@18"), Var(1))], 
            body: Var(Id("@18")) }, 

        Function { entry: (Label("@21"), Fun([Var(3)], Var(34))), free_variables: {Id("@17"): Fun([Var(1)], Var(1)), Id("@20"): Var(34)}, 
            args: [(Id("@22"), Var(3))], 
            body: AppClosure(Id("@17"), [Id("@20")]) },

        Function { entry: (Label("@19"), Fun([Var(34)], Fun([Var(36)], Var(34)))), free_variables: {Id("@17"): Fun([Var(1)], Var(1))}, 
            args: [(Id("@20"), Var(34))], 
            body: MakeClosure(Id("@21"), Closure { entry: Label("@21"), free_variables: {Id("@17"), Id("@20")} }, 
                  Var(Id("@21"))) }, 

        Function { entry: (Label("@23"), Fun([Int], Int)), free_variables: {}, 
            args: [(Id("@24"), Int)], 
            body: Let((Id("@48"), Int), Int(1), 
                  Add(Id("@24"), Id("@48"))) }, 

        Function { entry: (Label("@25"), Fun([Int], Int)), free_variables: {Id("@23"): Fun([Int], Int), Id("@25"): Fun([Int], Int)}, 
            args: [(Id("@26"), Int)], 
            body: Let((Id("@51"), Int), AppClosure(Id("@23"), [Id("@26")]), 
                  Let((Id("@53"), Int), AppClosure(Id("@25"), [Id("@51")]), 
                  AppClosure(Id("@23"), [Id("@53")]))) }, 

        Function { entry: (Label("@31"), Fun([Int], Int)), free_variables: {Id("@30"): Int, Id("@31"): Fun([Int], Int), Id("@23"): Fun([Int], Int)}, 
            args: [(Id("@32"), Int)], 
            body: IfEq(Id("@32"), Id("@30"), 
                  Var(Id("@32")), 
                  Let((Id("@59"), Int), AppClosure(Id("@23"), [Id("@32")]), 
                  Let((Id("@62"), Int), AppClosure(Id("@31"), [Id("@59")]), 
                  Add(Id("@32"), Id("@62"))))) }, 

        Function { entry: (Label("@29"), Fun([Int], Int)), free_variables: {Id("@23"): Fun([Int], Int), Id("@28"): Int}, 
            args: [(Id("@30"), Int)], 
            body: MakeClosure(Id("@31"), Closure { entry: Label("@31"), free_variables: {Id("@31"), Id("@23"), Id("@30")} }, 
                  AppClosure(Id("@31"), [Id("@28")])) }, 

        Function { entry: (Label("@27"), Fun([Int], Fun([Int], Int))), free_variables: {Id("@23"): Fun([Int], Int)}, 
            args: [(Id("@28"), Int)], 
            body: MakeClosure(Id("@29"), Closure { entry: Label("@29"), free_variables: {Id("@23"), Id("@28")} }, 
                  Var(Id("@29"))) }], 
    
    code: MakeClosure(Id("@17"), Closure { entry: Label("@17"), free_variables: {} }, 
          MakeClosure(Id("@19"), Closure { entry: Label("@19"), free_variables: {Id("@17")} }, 
          MakeClosure(Id("@23"), Closure { entry: Label("@23"), free_variables: {} }, 
          MakeClosure(Id("@25"), Closure { entry: Label("@25"), free_variables: {Id("@25"), Id("@23")} }, 
          MakeClosure(Id("@27"), Closure { entry: Label("@27"), free_variables: {Id("@23")} }, 
          Let((Id("@65"), Int), Int(10), 
          Let((Id("@66"), Int), Int(0),
          Let((Id("@68"), Fun([Int], Int)), AppClosure(Id("@27"), [Id("@66")]), 
          Let((Id("@33"), Int), AppClosure(Id("@68"), [Id("@65")]), 
          Var(Id("@33"))))))))))) }

*/

/*
Program { 
    decls: [
        Function { entry: (Label("@17"), Fun([Var(1)], Var(1))), 
            free_variables: {}, 
            args: [(Id("@18"), Var(1))], 
            body: Var(Id("@18")) }, 

        Function { entry: (Label("@21"), Fun([Var(3)], Var(34))), 
            free_variables: {Id("@17"): Fun([Var(1)], Var(1)), Id("@20"): Var(34)}, 
            args: [(Id("@22"), Var(3))], 
            body: AppClosure(Id("@17"), [Id("@20")]) }, 

        Function { entry: (Label("@19"), Fun([Var(34)], Fun([Var(36)], Var(34)))), 
            free_variables: {Id("@17"): Fun([Var(1)], Var(1))}, 
            args: [(Id("@20"), Var(34))], 
            body: MakeClosure(Id("@21"), Closure { entry: Label("@21"), free_variables: {Id("@17"), Id("@20")} }, 
                  Var(Id("@21"))) },

        Function { entry: (Label("@23"), Fun([Int], Int)), 
            free_variables: {}, 
            args: [(Id("@24"), Int)], 
            body: Let((Id("@48"), Int), Int(1), 
                  Add(Id("@24"), Id("@48"))) }, 

        Function { entry: (Label("@25"), Fun([Int], Int)), 
            free_variables: {Id("@23"): Fun([Int], Int)}, 
            args: [(Id("@26"), Int)], 
            body: Let((Id("@51"), Int), AppClosure(Id("@23"), [Id("@26")]), 
                  Let((Id("@53"), Int), AppClosure(Id("@25"), [Id("@51")]), 
                  AppClosure(Id("@23"), [Id("@53")]))) },

        Function { entry: (Label("@31"), Fun([Int], Int)), 
            free_variables: {Id("@30"): Int, Id("@23"): Fun([Int], Int)}, 
            args: [(Id("@32"), Int)], 
            body: IfEq(Id("@32"), Id("@30"), 
                       Var(Id("@32")), 
                       Let((Id("@59"), Int), AppClosure(Id("@23"), [Id("@32")]), 
                       Let((Id("@62"), Int), AppClosure(Id("@31"), [Id("@59")]), 
                       Add(Id("@32"), Id("@62"))))) }, 

        Function { entry: (Label("@29"), Fun([Int], Int)), 
            free_variables: {Id("@28"): Int, Id("@23"): Fun([Int], Int)}, 
            args: [(Id("@30"), Int)], 
            body: MakeClosure(Id("@31"), Closure { entry: Label("@31"), free_variables: {Id("@30"), Id("@23")} }, 
                  AppClosure(Id("@31"), [Id("@28")])) }, 

        Function { entry: (Label("@27"), Fun([Int], Fun([Int], Int))), 
            free_variables: {Id("@23"): Fun([Int], Int)}, 
            args: [(Id("@28"), Int)], 
            body: MakeClosure(Id("@29"), Closure { entry: Label("@29"), free_variables: {Id("@28"), Id("@23")} }, 
                  Var(Id("@29"))) }], 

    code: 
        MakeClosure(Id("@17"), Closure { entry: Label("@17"), free_variables: {} }, 
        MakeClosure(Id("@19"), Closure { entry: Label("@19"), free_variables: {Id("@17")} }, 
        MakeClosure(Id("@23"), Closure { entry: Label("@23"), free_variables: {} }, 
        MakeClosure(Id("@25"), Closure { entry: Label("@25"), free_variables: {Id("@23")} }, 
        MakeClosure(Id("@27"), Closure { entry: Label("@27"), free_variables: {Id("@23")} }, 
        Let((Id("@65"), Int), Int(10), 
        Let((Id("@66"), Int), Int(0), 
        Let((Id("@68"), Fun([Int], Int)), AppClosure(Id("@27"), [Id("@66")]), 
        Let((Id("@33"), Int), AppClosure(Id("@68"), [Id("@65")]), 
        Var(Id("@33"))))))))))) 
}
*/
