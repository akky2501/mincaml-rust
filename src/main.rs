
extern crate mincaml_rust;

use std::str;
use std::collections::HashMap;

use mincaml_rust::parser::parse;
use mincaml_rust::typing::{infer,TypeSubst,TypeEnv};
use mincaml_rust::alpha::alpha_transform;
use mincaml_rust::knormal::{knormal_transform, flat_let};

#[allow(unused_variables)]
fn main() {
    let src = b"
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
        1+(let x = (let y = 5+3 in y+y) in x+7)
        ";

    println!("src:\n{}",str::from_utf8(src).unwrap());

    println!("parse phase... begin");
    let (mut ast, mut vg) = parse(src).unwrap();
    println!("ast:\n{:?}\n",ast);
    println!("parse phase... end\n");

    println!("alpha transform phase... begin");
    alpha_transform(&mut ast, &mut HashMap::new(), &mut vg).unwrap();
    println!("alpha transformed ast:\n{:?}",ast);
    println!("alpha transform phase... end\n");
    
    println!("typing phase... begin");
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
    println!("typing phase... end\n");

    println!("knormal transform phase... begin");
    let (mut k, ty) = knormal_transform(ast, &mut HashMap::new(), &mut vg);
    println!("k-normal form:\n{:?}",k);
    println!("k-normal form type:\n{:?}",ty);
    k = flat_let(k);
    println!("flatted k-normal form:\n{:?}",k);
    println!("knormal transform phase... end\n");
    
    println!("closure transform phase... begin");
    println!("closure transform phase... end\n");
}

/*
LetRec(
    FunDef { 
        name: (Id("@4"), Fun([Var(3)], Fun([Var(8)], Var(3)))), 
        args: [(Id("@5"), Var(3))], 
        body: LetRec(
            FunDef { 
                name: (Id("@6"), Fun([Var(1)], Var(3))), 
                args: [(Id("@7"), Var(1))], 
                body: Var(Id("@5")) }, 
            Var(Id("@6"))) }, 
    Let(
        (Id("@12"), Int), Int(10), 
        App(Id("@4"), [Id("@12")])))
*/

/*
Let(
    (Id("a"), Int), 
    Int(1), 
    Let(
        (Id("b"), Int), 
        Let(
            (Id("x"), Int), 
            Let(
                (Id("y"), Int), 
                Let(
                    (Id("c"), Int), 
                    Int(5), 
                    Let(
                        (Id("d"), Int), 
                        Int(3), 
                        Add(Id("c"), Id("d")))), 
                Add(Id("y"), Id("y"))), 
            Let(
                (Id("e"), Int), 
                Int(7), 
                Add(Id("x"), Id("e")))), 
        Add(Id("a"), Id("b"))))
*/

/*
1+(let x = (let y = 5+3 in y+y) in x+7)
->
Let((Id("@10"), Int), Int(1), 
Let((Id("@4"), Int), Int(5), 
Let((Id("@5"), Int), Int(3), 
Let((Id("@2"), Int), Add(Id("@4"), Id("@5")), 
Let((Id("@3"), Int), Add(Id("@2"), Id("@2")), 
Let((Id("@9"), Int), Int(7), 
Let((Id("@11"), Int), Add(Id("@3"), Id("@9")), 
Add(Id("@10"), Id("@11")))))))))
*/


