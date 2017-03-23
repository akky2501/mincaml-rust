
extern crate mincaml_rust;

use std::str;
use std::collections::HashMap;
use std::collections::HashSet;

use mincaml_rust::parser::parse;
use mincaml_rust::typing::{infer,TypeSubst,TypeEnv};
use mincaml_rust::alpha_transform::alpha_transform;
use mincaml_rust::knormal::knormal_transform;

#[allow(unused_variables)]
fn main() {
    let src = b"
    let (a, b, c) = (
        let rec id a = a in
        let x = 10 in
        let y = false in
        let z = if y then 100 else -100 in
            x,z ; 
            id x + 42,id y && true,id z 
    ) in if b then a else c ; (a,c,b)
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
        let rec f x = x + 1 in
        f 42
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
    let (k, _) = knormal_transform(ast, &mut HashMap::new(), &mut vg);
    println!("k-normal form:\n{:?}",k);
    println!("knormal transform phase... end\n");
}


