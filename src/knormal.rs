use std::collections::HashMap;

use typing::Type;
use ast::Syntax as Ast;
use ast::FunDef as AstFunDef;
use id::{Id, VarGenerater};

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
    LetRec(FunDef, Box<Syntax>),
    App(Id, Vec<Id>),
    Tuple(Vec<Id>),
    LetTuple(Vec<(Id, Type)>, Id, Box<Syntax>),
}

#[derive(Debug)]
pub struct FunDef{
    pub name: (Id, Type),
    pub args: Vec<(Id, Type)>,
    pub body: Box<Syntax>,
}

macro_rules! insert_let {
    ($id:ident : $ty:ident = $k:ident in $action:block) => {{
        match $k {
            Var($id) => { $action },
            _ => Let(($id.clone(), $ty), Box::new($k), Box::new($action)),
        }
    }};
}

fn helper_apply(mut v: Vec<Id>, mut rev_t: Vec<Ast>, env: &mut HashMap<Id, Type>, vg: &mut VarGenerater) -> Syntax {
    use self::Syntax::*;
    if let Some(e) = rev_t.pop() {
        let (k, ty) = knormal_transform(e, env, vg);
        let new = vg.gen_id();

        insert_let!(new: ty = k in {
            v.push(new);
            if ! rev_t.is_empty() {
                helper_apply(v, rev_t, env, vg)
            }
            else {
                // v = [func, arg1, arg2 ...]
                let f = v.remove(0);
                App(f, v)
            }
        })
    }
    else { panic!() }
}

fn helper_tuple(ty_vec: &mut Vec<Type>, mut v: Vec<Id>, mut rev_t: Vec<Ast>, env: &mut HashMap<Id, Type>, vg: &mut VarGenerater) -> Syntax {
    use self::Syntax::*;
    if let Some(e) = rev_t.pop() {
        let (k, ty) = knormal_transform(e, env, vg);
        let new = vg.gen_id();
        
        ty_vec.push(ty.clone());
        
        insert_let!(new: ty = k in {
            v.push(new);
            if ! rev_t.is_empty() {
                helper_tuple(ty_vec, v, rev_t, env, vg)
            }
            else {
                Tuple(v)
            }
        })
    }
    else { panic!() }
}

pub fn knormal_transform(ast: Ast, env: &mut HashMap<Id, Type>, vg: &mut VarGenerater) -> (Syntax, Type) {
    use self::Syntax::*;

    macro_rules! knormal_transform {
        ($ast:expr) => {
            knormal_transform($ast, env, vg)
        };
    } 

    match ast {
        Ast::Unit => (Unit, Type::Unit),
        Ast::Bool(b) => (Bool(b), Type::Bool),
        Ast::Int(i)  => (Int(i), Type::Int),
        Ast::Not(e) => {
            knormal_transform!(Ast::If(e, Box::new(Ast::Bool(false)), Box::new(Ast::Bool(true))))
        },
        Ast::Neg(e) => {
            let (k,ty) = knormal_transform!(*e);
            let new = vg.gen_id();
            (insert_let!(new:ty = k in {Neg(new)}), Type::Bool)
        },
        Ast::Add(e1, e2) =>{
            let (k1,ty1) = knormal_transform!(*e1);
            let (k2,ty2) = knormal_transform!(*e2);
            let new1 = vg.gen_id();
            let new2 = vg.gen_id();
            (insert_let!(new1:ty1 = k1 in {
                insert_let!(new2:ty2 = k2 in {
                    Add(new1, new2)
                })
            }), Type::Int)
        },
        Ast::Sub(e1, e2) => {
            let (k1,ty1) = knormal_transform!(*e1);
            let (k2,ty2) = knormal_transform!(*e2);
            let new1 = vg.gen_id();
            let new2 = vg.gen_id();
            (insert_let!(new1:ty1 = k1 in {
                insert_let!(new2:ty2 = k2 in {
                    Sub(new1, new2)
                })
            }), Type::Int)
        },
        Ast::Mul(e1, e2) => {
            let (k1,ty1) = knormal_transform!(*e1);
            let (k2,ty2) = knormal_transform!(*e2);
            let new1 = vg.gen_id();
            let new2 = vg.gen_id();
            (insert_let!(new1:ty1 = k1 in {
                insert_let!(new2:ty2 = k2 in {
                    Mul(new1, new2)
                })
            }), Type::Int)
        },
        Ast::Div(e1, e2) => {
            let (k1,ty1) = knormal_transform!(*e1);
            let (k2,ty2) = knormal_transform!(*e2);
            let new1 = vg.gen_id();
            let new2 = vg.gen_id();
            (insert_let!(new1:ty1 = k1 in {
                insert_let!(new2:ty2 = k2 in {
                    Div(new1, new2)
                })
            }), Type::Int)
        },
        Ast::Eq(e1, e2) => {
            knormal_transform!(Ast::If(Box::new(Ast::Eq(e1,e2)),
                Box::new(Ast::Bool(true)), Box::new(Ast::Bool(false))))
        },
        Ast::LE(e1, e2) => {
            knormal_transform!(Ast::If(Box::new(Ast::LE(e1,e2)),
                Box::new(Ast::Bool(true)), Box::new(Ast::Bool(false))))
        },
        Ast::If(c, t, f) => {
            let c = *c;
            match c {
                Ast::Not(c) => knormal_transform!(Ast::If(c, f, t)),
                Ast::Eq(l, r) => {
                    let (k1,ty1) = knormal_transform!(*l);
                    let (k2,ty2) = knormal_transform!(*r);
                    let new1 = vg.gen_id();
                    let new2 = vg.gen_id();
                    let (kt, tyt) = knormal_transform!(*t);
                    let (kf, _) = knormal_transform!(*f);
                    (insert_let!(new1:ty1 = k1 in {
                        insert_let!(new2:ty2 = k2 in {
                            IfEq(new1, new2, Box::new(kt), Box::new(kf))
                        })
                    }), tyt)
                },
                Ast::LE(l, r) => {
                    let (k1,ty1) = knormal_transform!(*l);
                    let (k2,ty2) = knormal_transform!(*r);
                    let new1 = vg.gen_id();
                    let new2 = vg.gen_id();
                    let (kt, tyt) = knormal_transform!(*t);
                    let (kf, _) = knormal_transform!(*f);
                    (insert_let!(new1:ty1 = k1 in {
                        insert_let!(new2:ty2 = k2 in {
                            IfLE(new1, new2, Box::new(kt), Box::new(kf))
                        })
                    }), tyt)
                },
                _ => knormal_transform!(Ast::If(Box::new(Ast::Eq(Box::new(c),Box::new(Ast::Bool(false)))), f, t)),
            }
        },
        Ast::Let((id, ty), e1, e2) => {
            let (k1, _) = knormal_transform!(*e1);
            let key = id.clone();
            env.insert(id.clone(), ty.clone());
            let (k2, ty2) = knormal_transform!(*e2);
            env.remove(&key);
            (Let((id, ty), Box::new(k1), Box::new(k2)), ty2)
        },
        Ast::Var(id) => {
            if let Some(t) = env.get(&id) {
                (Var(id), t.clone()) 
            }
            else { panic!() }
        },
        Ast::LetRec(AstFunDef{ name: (name, fun_type), args, body}, e) => {
            let t = args.into_vec();
            for i in t.iter() {
                env.insert(i.0.clone(), i.1.clone());
            }
            env.insert(name.clone(), fun_type.clone());

            let (k1, _) = knormal_transform!(*body);
 
            for i in t.iter() {
                env.remove(&i.0);
            }
                        
            let (k2, ty2) = knormal_transform!(*e);

            env.remove(&name);

            (LetRec(FunDef{name: (name, fun_type), args: t, body: Box::new(k1)}, Box::new(k2)), ty2)
        },
        Ast::App(f, args, ty) => {
            let mut a = args.into_vec();
            a.reverse();
            a.push(*f);
            let r = helper_apply(Vec::new(), a, env, vg);
            (r, ty)
        },
        Ast::Tuple(t) => {
            let mut t = t.into_vec();
            t.reverse();
            let mut ty = Vec::new();
            let r = helper_tuple(&mut ty, Vec::new(), t, env, vg);
            (r, Type::Tuple(ty))
        },
        Ast::LetTuple(t, e1, e2) => {
            let (k1, ty1) = knormal_transform!(*e1);
            let new = vg.gen_id();

            let t = t.into_vec();
            for i in t.iter() {
                env.insert(i.0.clone(), i.1.clone());
            }

            let (k2, ty2) = knormal_transform!(*e2);
            
            for i in t.iter() {
                env.remove(&i.0);
            }

            (insert_let!(new: ty1 = k1 in {
                LetTuple(t, new, Box::new(k2))
            }), ty2)
        },
    }
}

pub fn flat_let(k: Syntax) -> Syntax {
    use self::Syntax::*;
    match k {
        k@Unit      |
        k@Bool(_)   |
        k@Int(_)    |
        k@Neg(_)    |
        k@Add(_, _) |
        k@Sub(_, _) |
        k@Mul(_, _) |
        k@Div(_, _) |
        k@Var(_)    |
        k@App(_, _) |
        k@Tuple(_)  => k,
        IfEq(a, b, t, f) => {
            let t = flat_let(*t);
            let f = flat_let(*f);
            IfEq(a, b, Box::new(t), Box::new(f))
        },
        IfLE(a, b, t, f) => {
            let t = flat_let(*t);
            let f = flat_let(*f);
            IfLE(a, b, Box::new(t), Box::new(f))
        },
        Let(var, v, e) => {
            let v = *v;
            match v {
                Let(var2, v2, e2) => {
                    let v2 = flat_let(*v2);
                    let e = flat_let(*e);
                    let e2 = flat_let(*e2);
                    flat_let(Let(var2, Box::new(v2), Box::new(Let(var, Box::new(e2), Box::new(e)))))
                },
                v@_ => {
                    let v = flat_let(v);
                    let e = flat_let(*e);
                    Let(var, Box::new(v), Box::new(e))
                },
            }
        },
        LetRec(mut f, e) => {
            f.body = Box::new(flat_let(*f.body));
            let e = flat_let(*e);
            LetRec(f, Box::new(e))
        },
        LetTuple(a, b, e) => {
            let e = flat_let(*e);
            LetTuple(a, b, Box::new(e))
        },
    }
}


