
extern crate mincaml_rust;

use std::str;
use std::collections::HashMap;

use mincaml_rust::parser::parse;
use mincaml_rust::typing::{infer,TypeSubst,TypeEnv};
use mincaml_rust::alpha::alpha_transform;
use mincaml_rust::knormal::{knormal_transform, flat_let};
use mincaml_rust::closure::closure_transform;
use mincaml_rust::codegen::code_generate;

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
                let rec id x = x in
                let rec add x =
                    let rec addx y = x + y in id (id addx) in
                let rec inc x = 1+ id x  in
                let g = if true then add 10 else inc in
                g 50
    ";

    let src7 = b"
        let g = let rec id x = x in let rec id2 x = x in if true then id else id2 in
        g
        ";

    let src8 = b"
        let rec id x = x in
        let rec add x =
            let rec f y = 
                let rec g z = 
                    let rec h w = x+y+z+w in id h
                in id g
            in id f
        in add
    ";

    let src9 =b"
        let rec pred x = x=x in
        let rec f p= let rec g x y z = if p y then x else z in g in
        let rec h x y z = if (z = 0)=y then x else x in
        (f pred, h)
    ";

    let src = b"
        let rec id x = x in
        let rec id1 x y = x in
        let rec id2 x y = y in
        1 + (if true then id id1 else id id2) 10 20
    ";

    let src = b"
        let rec id x = x in
        let rec add x y z w = x+y+z+w in
        let rec succ x = x + 1 in
        let x = id 42 in
        let a = id 0 in
        let y = (add (succ a) (succ (succ a)) (succ (succ (succ a))) (succ (succ (succ (succ 0))))) in
        let rec sum x = if x > 0 then x + sum (x - 1) else 0 in
        let rec fib n = if n = 0 || n = 1 then n else fib (n-1) + fib (n-2) in
        if sum y = fib 10 then 2000 else 0
    ";
    
    // 関数のbody内にその関数が変数として現れる場合、同処理するのか
    let src12 = b"
        let rec id x = x in
        let rec sum x = if x > 0 then x + (id sum) (x - 1) else 0 in
        sum 5
    ";

    let src13 = b"
        let rec mod x y = x - (x/y)*y in
        let rec is_prime x y = if x > y then (if mod x y = 0 then 0 else is_prime x (y+1)) else 1 in
        let rec greatest_prime n = if is_prime n 2 = 1 then n else greatest_prime (n-1) in
        greatest_prime 70000";

    let src = b"
        let rec greatest_prime n =
            let rec is_prime x y =
                let rec mod x y = x - (x/y)*y in
                if x > y then (if mod x y = 0 then 0 else is_prime x (y+1)) else 1 in
            if is_prime n 2 = 1 then n else greatest_prime (n-1) in
        greatest_prime 65535
    ";

    let src = b"let rec fact n = if n = 0 then 1 else n*fact (n-1) in
                fact 10";

    let src = b"let rec const x = let rec f y = x in f in
                let x = (1, (2, 3), (4, 5, 6)) in
                let (a, y, z) = x in
                let (b, c) = y in
                let (d, e, f) = z in
                (const (a + b + c + d + e + f)) 42";


    println!("/*");
    println!("src:\n{}",str::from_utf8(src).unwrap());

    println!("[[parse phase... begin]]");
    let (mut ast, mut vg) = parse(src).unwrap();
    println!("ast:\n{:#?}\n",ast);
    println!("[[parse phase... end]]\n");

    println!("[[alpha transform phase... begin]]");
    alpha_transform(&mut ast, &mut Vec::new(), &mut vg).unwrap();
    println!("alpha transformed ast:\n{:#?}",ast);
    println!("[[alpha transform phase... end]]\n");
    
    println!("[[typing phase... begin]]");
    let mut subst = TypeSubst::new();
    let mut env = TypeEnv::new();
    let mut ret_ty = infer(&mut ast, &mut env, &mut subst, &mut vg).unwrap();
    println!("type: {:#?}",ret_ty);
    println!("env:\n{:#?}",env);
    println!("subst:\n{:#?}",subst);
    println!("typed ast:\n{:#?}",ast);
    ret_ty.apply(&subst);
    println!("substituted type: {:#?}",ret_ty);
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
    println!("k-normal form:\n{:#?}",k);
    println!("k-normal form type:\n{:#?}",ty);
    k = flat_let(k);
    println!("flatted k-normal form:\n{:#?}",k);
    println!("[[knormal transform phase... end]]\n");
    
    println!("[[closure transform phase... begin]]");
    let p = closure_transform(k);
    println!("program:\n{:#?}", p);
    println!("[[closure transform phase... end]]\n");

    println!("*/\n");
    code_generate(p, ret_ty);
}

