
extern crate mincaml_rust;

use std::collections::HashMap;

use mincaml_rust::parser::parse;
use mincaml_rust::typing::{infer,TypeSubst,TypeEnv};
use mincaml_rust::alpha_transform::alpha_transform;

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
        let rec Infinite x = (succ (Infinite (succ x))) in
        let rec sum min = 
            let rec helper max =
                let rec loop x = 
                    if x = max then x else x + loop (succ x)
                in loop min 
            in helper
        in
        let main = (sum 0) 10 in
        const
                ";
    let src2 = b"let rec f x = x in (f 1, f true)";
    let src3 = b"let y = (let rec f x = x+1 in f)
                in y";
    let src = b"
        let rec S x y z = x z (y z) in
        let rec K x y   = x         in
        let rec I x     = x         in
        (S, K, I)
    ";


    let (mut ast, mut vg) = parse(src).unwrap();
    println!("ast: {:?}\n",ast);

    alpha_transform(&mut ast, &mut HashMap::new(), &mut vg).unwrap();
    println!("alpha transformed ast:\n{:?}",ast);
    
    let mut subst = TypeSubst::new();
    let mut tenv = TypeEnv::new();
    let mut ty = infer(&mut subst, &mut vg, &mut tenv, &mut ast).unwrap();
    println!("subst: {:?}\nenv: {:?}\ntype: {:?}",subst,tenv,ty);
    println!("typed ast: {:?}",ast);
    ty.apply(&subst);
    println!("result type: {:?}",ty);

}

