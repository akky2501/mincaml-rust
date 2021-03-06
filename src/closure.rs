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
    AppDirect(Label, Vec<Id>),
    Tuple(Vec<Id>),
    LetTuple(Vec<(Id, Type)>, Id, Box<Syntax>),
}

#[derive(Debug)]
pub struct Closure {
    pub entry: Label,
    pub free_variables: HashSet<Id>,
}

#[derive(Debug)]
pub struct Function {
    pub entry: (Label, Type),
    pub free_variables: HashMap<Id, Type>,
    pub args: Vec<(Id, Type)>,
    pub body: Box<Syntax>,
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
                AppDirect(_, ref args) => {
                    for i in args.iter() {
                        s.insert(i.clone());
                    }
                },
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
    pub decls: Vec<Function>,
    pub code: Syntax,
}



pub fn closure_transform(exp: KN /*, env*/) -> Program {
    let mut env = HashMap::new();
    let mut decls = Vec::new();
    let mut known = HashSet::new();
    let code = transform(exp, &mut env, &mut decls, &mut known);
    println!("known set {:?}", known);
    Program { decls: decls, code: code }
}

fn transform(exp: KN, env: &mut HashMap<Id,Type>, decls: &mut Vec<Function>, known: &mut HashSet<Label>) -> Syntax {
    use self::Syntax::*;
    macro_rules! trans {
        ($e:expr) => { transform($e, env, decls, known) };
    }

    match exp {
        KN::Unit => Unit,
        KN::Bool(b) => Bool(b),
        KN::Int(i)  => Int(i),
        KN::Neg(i)  => Neg(i),
        KN::Add(i, j) => Add(i, j),
        KN::Sub(i, j) => Sub(i, j),
        KN::Mul(i, j) => Mul(i, j),
        KN::Div(i, j) => Div(i, j),
        KN::IfEq(x, y, t, f) => IfEq(x, y, Box::new(trans!(*t)), Box::new(trans!(*f))),
        KN::IfLE(x, y, t, f) => IfLE(x, y, Box::new(trans!(*t)), Box::new(trans!(*f))),
        KN::Let((i, t), e1, e2) => {
            let e1 = trans!(*e1);
            env.insert(i.clone(), t.clone());
            let e2 = trans!(*e2);
            env.remove(&i);
            Let((i, t), Box::new(e1), Box::new(e2))
        },
        KN::Var(i) => Var(i),
        KN::LetRec(KNFunDef{ name: (name, ty), args, body }, e) => {
            env.insert(name.clone(), ty.clone());
            
            for x in args.iter() {
                env.insert(x.0.clone(), x.1.clone());
            }

            known.insert(Label(name.0.clone())); // body内ではknownのはず、TODO: body内にnameが変数として現れる場合にどう処理するか
            // min-camlでは変換をし直している？
            let body = trans!(*body);

            let mut a:HashSet<_> = args.iter().map(|&(ref x, _)| x.clone()).collect();
            a.insert(Id(name.0.clone()));
            let fv:HashSet<_> = body.free_variables().difference(&a).cloned().collect();
            let mut fvt = HashMap::new();
            for x in fv.iter() {
                fvt.insert(x.clone(), env.get(x).unwrap().clone());
            }

            for &(ref x, _) in args.iter() {
                env.remove(x);
            }

            if ! fvt.is_empty() { // 自由変数を含んでいたらknownから外す
                known.remove(&Label(name.0.clone()));
            }

            let f = Function{ entry: (Label(name.0.clone()), ty), free_variables: fvt, args: args, body: Box::new(body) };
            decls.push(f);


            let c = Closure {entry: Label(name.0.clone()), free_variables: fv};
            let e = trans!(*e);

            env.remove(&name);
            
            if e.free_variables().contains(&name) {
                MakeClosure(name, c, Box::new(e))
            }
            else {
                e
            }
        },
        KN::App(Id(f), args) => {
            let l = Label(f);
            if known.contains(&l) {
                AppDirect(l, args)
            }
            else {
                AppClosure(Id(l.0), args)
            }
        },
        KN::Tuple(t) => Tuple(t),
        KN::LetTuple(t, i, e) => {
            for x in t.iter() {
                env.insert(x.0.clone(), x.1.clone());
            }
            let e = trans!(*e);
            for &(ref x, _) in t.iter() {
                env.remove(x);
            }
            LetTuple(t, i, Box::new(e))
        },
    }
}



