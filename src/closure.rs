use std::collections::HashSet;
use std::collections::HashMap;

use id::*;
use knormal::Syntax as KN;
use knormal::FunDef as KNFunDef;
use typing::*;

#[derive(Debug)]
pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i64),
    Neg(Id),
    Add(Id, Id),
    Sub(Id, Id),
    Mul(Id, Id),
    Div(Id, Id),
    IfEq(Id, Id, Box<Syntax>, Box<Syntax>),
    IfLE(Id, Id, Box<Syntax>, Box<Syntax>),
    Let((Id, Type), Box<Syntax>, Box<Syntax>),
    Var(Id),
    MakeClosure(Id, Closure, Box<Syntax>), // let rec f x = e1(expression containing free variables) in e2
    AppClosure(Id, Vec<Id>),
    //AppDirect(Label, Vec<Id>),
    Tuple(Vec<Id>),
    LetTuple(Vec<(Id, Type)>, Id, Box<Syntax>),
}

#[derive(Debug)]
pub struct Closure {
    entry: Label,
    free_variables: HashSet<Id>,
}

#[derive(Debug)]
pub struct Function {
    entry: (Label, Type),
    free_variables: HashMap<Id, Type>,
    args: Vec<(Id, Type)>,
    body: Box<Syntax>,
}

impl Syntax {
    fn free_variables(&self) -> HashSet<Id> {
        use self::Syntax::*;

        fn fv(e: &Syntax, s: &mut HashSet<Id>){
            match *e {
                Unit | Bool(_) | Int(_) => (),
                Neg(ref i) => { s.insert(i.clone()); },
                Add(ref i, ref j) |
                Sub(ref i, ref j) |
                Mul(ref i, ref j) |
                Div(ref i, ref j) => {
                    s.insert(i.clone());
                    s.insert(j.clone());
                },
                IfEq(ref i, ref j, ref t, ref f) |
                IfLE(ref i, ref j, ref t, ref f) => {
                    s.insert(i.clone());
                    s.insert(j.clone());
                    fv(&**t, s);
                    fv(&**f, s);
                },
                Let((ref i, _), ref e1, ref e2) => {
                    fv(&**e1, s);
                    fv(&**e2, s);
                    s.remove(i);
                },
                Var(ref i) => { s.insert(i.clone()); },
                MakeClosure(ref i, Closure { entry: _, free_variables: ref v }, ref e1) => {
                    for x in v.iter() {
                        s.insert(x.clone());
                    }
                    fv(&**e1, s);
                    s.remove(i);
                },
                AppClosure(ref f, ref args) => {
                    s.insert(f.clone());
                    for i in args.iter() {
                        s.insert(i.clone());
                    }
                },
                //AppDirect(Label, Vec<Id>),
                Tuple(ref el) => {
                    for i in el.iter() {
                        s.insert(i.clone());
                    }
                },
                LetTuple(ref d, ref i, ref e1) => {
                    fv(&**e1, s);
                    s.insert(i.clone());
                    for &(ref x, _) in d.iter() {
                        s.remove(x);
                    }
                },
            }
        }

        let mut s = HashSet::new();
        fv(self, &mut s);
        s
    }
}

#[derive(Debug)]
pub struct Program {
    decls: Vec<Function>,
    code: Syntax,
}



pub fn closure_transform(exp: KN /*, env*/) -> Program {
    let mut env = HashMap::new();
    let mut decls = Vec::new();
    let code = transform(exp, &mut env, &mut decls);
    Program { decls: decls, code: code }
}


fn transform(exp: KN, env: &mut HashMap<Id,Type>, decls: &mut Vec<Function>) -> Syntax {
    use self::Syntax::*;
    match exp {
        KN::Unit => Unit,
        KN::Bool(b) => Bool(b),
        KN::Int(i)  => Int(i),
        KN::Neg(i)  => Neg(i),
        KN::Add(i, j) => Add(i, j),
        KN::Sub(i, j) => Sub(i, j),
        KN::Mul(i, j) => Mul(i, j),
        KN::Div(i, j) => Div(i, j),
        KN::IfEq(x, y, t, f) => IfEq(x, y, Box::new(transform(*t, env, decls)), Box::new(transform(*f, env, decls))),
        KN::IfLE(x, y, t, f) => IfLE(x, y, Box::new(transform(*t, env, decls)), Box::new(transform(*f, env, decls))),
        KN::Let((i, t), e1, e2) => {
            let e1 = transform(*e1, env, decls);
            env.insert(i.clone(), t.clone());
            let e2 = transform(*e2, env, decls);
            env.remove(&i);
            Let((i, t), Box::new(e1), Box::new(e2))
        },
        KN::Var(i) => Var(i),
        KN::LetRec(KNFunDef{ name: (Id(name), ty), args, body }, e) => {
            env.insert(Id(name.clone()), ty.clone());
            for x in args.iter() {
                env.insert(x.0.clone(), x.1.clone());
            }

            println!("body before: {:?}",body);
            let body = transform(*body, env, decls); 
            println!("body after: {:?}",body);
            let mut a:HashSet<_> = args.iter().map(|&(ref x, _)| x.clone()).collect();
            a.insert(Id(name.clone()));
            let bfv = body.free_variables();
            let fv: HashSet<_> = bfv.difference(&a).cloned().collect();
            /*println!("{{env: {:?}",env);
            println!("body_fv: {:?}",bfv);
            println!("a: {:?}",a);
            println!("fv = bfv-a: {:?}}}",fv);*/
            let mut fvt = HashMap::new();
            for x in fv.iter() {
                fvt.insert(x.clone(), env.get(x).unwrap().clone());
            }
 
            for &(ref x, _) in args.iter() {
                env.remove(x);
            }
           
            let f = Function{ entry: (Label(name.clone()), ty), free_variables: fvt, args: args, body: Box::new(body) };
            decls.push(f);


            let c = Closure {entry: Label(name.clone()), free_variables: fv};
            let e = transform(*e, env, decls);
            
            env.remove(&Id(name.clone()));
            
            MakeClosure(Id(name), c, Box::new(e))
        },
        KN::App(f, args) => {
            AppClosure(f, args)
            // AppDirect
        },
        KN::Tuple(t) => Tuple(t),
        KN::LetTuple(t, i, e) => {
            for x in t.iter() {
                env.insert(x.0.clone(), x.1.clone());
            }
            let e = transform(*e, env, decls);
            for &(ref x, _) in t.iter() {
                env.remove(x);
            }
            LetTuple(t, i, Box::new(e))
        },
    }
}


