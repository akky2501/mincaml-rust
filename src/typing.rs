use std::collections::HashSet;
use rand;
use id::{Id,VarGenerater};
use ast::*;

#[derive(Debug, Clone)]
pub enum Type {
    Unit,
    Bool,
    Int,
    Fun(Box<[Type]>, Box<Type>),
    Tuple(Box<[Type]>),
    Var(usize),
}

impl Type {
    fn free_variables(&self) -> HashSet<usize> {
        match *self {
            Type::Unit | Type::Bool | Type::Int => HashSet::new(),
            Type::Fun(ref l,ref r) => { 
                let mut s = HashSet::new();
                for ref x in l.iter() {
                    s = s.union(&(x.free_variables())).cloned().collect()
                }
                s.union(&(r.free_variables())).cloned().collect()
            },
            Type::Tuple(ref t) => {
                let mut s = HashSet::new();
                for ref x in t.iter() {
                    s = s.union(&(x.free_variables())).cloned().collect()
                }
                s
            },
            Type::Var(x) => {
                let mut s = HashSet::new();
                s.insert(x);
                s
            },
        }
    }

    fn rewrite_variables(&mut self, from: usize, to: usize) {
        match *self {
            Type::Unit | Type::Bool | Type::Int => (),
            Type::Fun(ref mut l, ref mut r) => {
                    for ref mut i in l.iter_mut() {
                        i.rewrite_variables(from,to);
                    }
                    r.rewrite_variables(from,to);
            },
            Type::Tuple(ref mut x) => {
                for i in x.iter_mut(){
                    i.rewrite_variables(from,to);
                }
            },
            Type::Var(ref mut x) => {
                if *x == from {
                    *x = to;
                }
            },
        }
    }

    fn subst(&mut self, var: usize, to: &Type){
        match *self {
            Type::Unit | Type::Bool | Type::Int => (),
            Type::Fun(ref mut l, ref mut r) => {
                    for ref mut i in l.iter_mut() {
                        i.subst(var,to);
                    }
                    r.subst(var,to);
            },
            Type::Tuple(ref mut x) => {
                for i in x.iter_mut(){
                    i.subst(var,to);
                }
            },
            Type::Var(x) => {
                if x == var {
                    *self = to.clone();
                }
            },
        }

    }

    pub fn apply(&mut self, subst: &TypeSubst) {
        for &(ref var, ref to) in subst.equations.iter() {
            self.subst(*var, to);
        }
    }

    /*fn generalize(&self, env: &TypeEnv) -> TypeScheme {
        let tfv = self.free_variables();
        let efv = env.free_variables();
        let bind: HashSet<_> = tfv.difference(&efv).cloned().collect();
        TypeScheme{ bind: bind, body: self.clone() }
    }*/

}


#[derive(Debug,Clone)]
pub struct TypeScheme {
    pub bind: HashSet<usize>,
    pub body: Type,
}

impl TypeScheme {
    fn instantiate(&self, vg: &mut VarGenerater) -> Type {
        let mut t = self.body.clone();
        for from in self.bind.iter() {
            let to = vg.gen_count();
            t.rewrite_variables(*from, to);
        }
        t
    }

    fn free_variables(&self) -> HashSet<usize> {
        let fv = self.body.free_variables();
        fv.difference(&self.bind).cloned().collect()
    }
    
    fn apply(&mut self, subst: &TypeSubst) {
        for &(ref var, ref to) in subst.equations.iter() {
            if ! self.bind.contains(var) {
                self.body.subst(*var, to);
            }
        }
    }

}

#[derive(Debug, Clone)]
pub struct TypeEnv{
    seq: Vec<(Id,TypeScheme)>,
}

impl TypeEnv {
    pub fn new() -> Self {
        TypeEnv{ seq: Vec::new() }
    }

    fn free_variables(&self) -> HashSet<usize> {
        let mut s = HashSet::new();
        for &(_,ref i) in &(self.seq) {
            s = s.union(&(i.free_variables())).cloned().collect();
        }
        s
    }

    fn push(&mut self, id: Id, ty: TypeScheme){
        self.seq.push((id, ty));
    }

    fn push_t(&mut self, id: Id, ty: Type) {
        self.seq.push((id, TypeScheme{bind: HashSet::new(), body: ty}));
    }

    fn pop(&mut self) {
        self.seq.pop();
    }

    fn contains(&self, id: &Id) -> Option<&TypeScheme> {
        if let Some(x) = self.seq.iter().rposition(|&(ref x, _)| *x == *id) {
            Some(&self.seq[x].1)
        }
        else { None }
    }

    fn apply(&mut self, subst: &TypeSubst) {
        for x in self.seq.iter_mut() {
            x.1.apply(subst);
        }
    }

}


#[derive(Debug)]
pub struct TypeSubst {
    equations: Vec<(usize,Type)>
}

impl TypeSubst {
    pub fn new() -> Self {
        TypeSubst{ equations: vec![] }
    }

    fn push(&mut self, var: usize, ty: Type) {
        self.equations.push((var,ty));
    }

    fn append(&mut self, mut s: TypeSubst) {
        self.equations.append(&mut s.equations);
    }

}


fn unify(mut c: Vec<(Type, Type)>) -> Result<TypeSubst, ()> {
    if let Some(last) = c.pop() {
        match last {
            (Type::Unit, Type::Unit) |
            (Type::Int,  Type::Int)  |
            (Type::Bool, Type::Bool) => {
                unify(c)
            },
            (Type::Var(x), Type::Var(y)) if x == y => {
                unify(c)
            },
            (Type::Var(x), t@_) |
            (t@_, Type::Var(x)) => {
                if ! t.free_variables().contains(&x) { // occur check
                    let mut subst = TypeSubst::new();
                    subst.push(x,t);
                    for i in c.iter_mut() {
                        i.0.apply(&subst); i.1.apply(&subst);
                    }
                    let s = unify(c)?;
                    subst.append(s); // 型代入の合成の順序は大事!
                    Ok(subst)
                }
                else { Err(()) }
            },
            (Type::Fun(args1, ret1), Type::Fun(args2, ret2)) => {
                c.push((*ret1, *ret2));
                c.push((Type::Tuple(args1), Type::Tuple(args2)));
                unify(c)
            },
            (Type::Tuple(elems1), Type::Tuple(elems2)) => {
                if elems1.len() == elems2.len() {
                    let mut t: Vec<_> = elems1.to_vec().into_iter()
                                          .zip(elems2.to_vec().into_iter()).collect();
                    c.append(&mut t);
                    unify(c)
                }
                else { Err(()) }
            },
            (_,_) => Err(()),
        }
    }
    else {
        Ok(TypeSubst::new())
    }
}

// Algorithm J
pub fn infer(gsubst: &mut TypeSubst, vg: &mut VarGenerater, env: &mut TypeEnv, expr: &mut Syntax) -> Result<Type, ()> {
    macro_rules! unify{
        ($e:expr) => {{
            let mut v = $e;
            for x in v.iter_mut() {
                x.0.apply(gsubst);
                x.1.apply(gsubst);
            }
            println!("before");
            let s = unify(v)?;
            println!("after");
            gsubst.append(s);
        }};

        ($e1:expr , $e2:expr) => {{
            unify!(vec![($e1, $e2)]);
        }};
    }

    macro_rules! infer{
        ($expr:expr) => {{
            infer(gsubst, vg, env, $expr)?
        }};

        ($id:ident : $ex:expr , $expr:expr) => {{
            let e = $ex;
            let i = $id.clone();
            env.push(i, e);
            let ty = infer!($expr);
            env.pop();
            ty
        }};

        /*( $id:ident : $ex:expr $( , $ids:ident : $exs:expr )+ , $expr:expr) => {{
            env.push($id.clone(), $ex);
            let ty = infer!($( $ids : $exs , )+ , $expr);
            env.pop();
            ty
        }};*/
    }

    match *expr {
        Syntax::Unit    => Ok(Type::Unit),
        Syntax::Bool(_) => Ok(Type::Bool),
        Syntax::Int(_)  => Ok(Type::Int),
        Syntax::Not(ref mut t) => {
            println!("Not");
            unify!(Type::Bool, infer!(t));
            Ok(Type::Bool)
        },
        Syntax::Neg(ref mut t) => {
            println!("Neg");
            unify!(Type::Int, infer!(t));
            Ok(Type::Int)
        },
        Syntax::Add(ref mut t1, ref mut t2) |
        Syntax::Sub(ref mut t1, ref mut t2) |
        Syntax::Mul(ref mut t1, ref mut t2) |
        Syntax::Div(ref mut t1, ref mut t2) => {
            println!("BinOp");
            let r1 = infer!(t1);
            let r2 = infer!(t2);
            unify!(r1.clone(), r2);
            unify!(r1, Type::Int);
            Ok(Type::Int)
        },
        Syntax::Eq(ref mut t1, ref mut t2) => {
            println!("Eq");
            unify!(infer!(t1), infer!(t2));
            Ok(Type::Bool)
        },
        Syntax::LE(ref mut t1, ref mut t2) => {
            println!("LE");
            let r = infer!(t1);
            unify!(Type::Int, r.clone());
            unify!(r, infer!(t2));
            Ok(Type::Bool)
        },
        Syntax::If(ref mut c, ref mut t, ref mut f) => {
            println!("If");
            let ty = infer!(c);
            println!("If uni 1");
            unify!(Type::Bool, ty);
            let ty = infer!(t);
            println!("If uni 2");
            unify!(ty.clone(), infer!(f));
            Ok(ty)
        },
        Syntax::Let((ref mut id, ref mut ty), ref mut t1, ref mut t2) => {
            println!("\nLet ({:?} {:?}) {:?} {:?}\n{:?}\n{:?}",id,ty,t1,t2,env,gsubst);
            let a = infer!(t1);
            unify!(ty.clone(), a.clone());
            *ty = a.clone(); ty.apply(gsubst);
            let mut sa = a.clone(); sa.apply(gsubst);
            let mut se = env.clone(); se.apply(gsubst);
            let bind: HashSet<_> = sa.free_variables().difference(&(se.free_variables())).cloned().collect();
            let gen = TypeScheme{ bind: bind, body: sa };
            let b = infer!(id : gen, t2);
            Ok(b)
        },
        Syntax::Var(ref id) => {
            let n = rand::random::<u16>();
            println!("\n{} Var {:?} \n{:?}\n{:?}",n,id,env,gsubst);
            if let Some(tys) = env.contains(id) {
                let r = tys.instantiate(vg);
                println!("{} Var instance {:?}",n,r);
                Ok(r)
            }
            else { Err(()) }
        },
        Syntax::LetRec(ref mut fun, ref mut t) => {
            let n = rand::random::<u16>();
            println!("{}\nLetRec {:?} {:?}\n{:?}\n{:?}",n,fun,t,env,gsubst);
            env.push_t(fun.name.0.clone(), fun.name.1.clone());
            for i in fun.args.iter() {
                env.push_t(i.0.clone(), i.1.clone());
            }

            let r = infer!(&mut *(fun.body));
            
            for _ in 0..fun.args.len() {
                env.pop();
            }
            env.pop(); // fun.name

            let a = Type::Fun(
                fun.args.iter()
                        .map(|&(_, ref t)| t.clone())
                        .collect::<Vec<Type>>().into_boxed_slice(),
                Box::new(r));
            println!("{} LetRec uni before {:?} | {:?}\n{:?}",n,fun.name.1,a,gsubst);
            unify!(fun.name.1.clone(), a.clone());
            println!("{} LetRec uni after",n);
            fun.name.1.apply(gsubst);
            for i in fun.args.iter_mut() {
                i.1.apply(gsubst);
            }

            let mut sa = a.clone(); sa.apply(gsubst);
            let mut se = env.clone(); se.apply(gsubst);
            let bind: HashSet<_> = sa.free_variables().difference(&(se.free_variables())).cloned().collect();
            println!("{}\nsubst {:?} \na {:?} \nenv {:?}",n,gsubst,a,env);
            println!(" sa {:?} \nse {:?}",sa,se);
            println!("bind {:?} \nfv(sa) {:?} \nfv(se) {:?}",bind,sa.free_variables(),se.free_variables());
            let gen = TypeScheme{ bind: bind, body: sa };
            println!("{}next infer\nsubst {:?} \ngen {:?} \nenv {:?}",n,gsubst,gen,env);
            let id = fun.name.0.clone();
            let b = infer!(id : gen, t);
            Ok(b)
        },
        Syntax::App(ref mut f, ref mut args) => {
            let n = rand::random::<u16>();
            println!("\n{} App {:?} {:?}\n{:?}\n{:?}",n,f,args,env,gsubst);
            let tf = infer!(f);
            let mut tya: Vec<Type> = Vec::new();
            for i in args.iter_mut() {
                tya.push(infer!(i));
            }
            let newvar = vg.gen_type();
            println!("\n{} tf {:?} , tya {:?} new {:?}",n,tf,tya,newvar);
            unify!(Type::Fun(tya.into_boxed_slice(), Box::new(newvar.clone())), tf);
            println!("\n{} gsubst {:?}",n,gsubst);
            Ok(newvar)
        },
        Syntax::Tuple(ref mut t) => {
            let mut tup: Vec<Type> = Vec::new();
            for i in t.iter_mut() {
                tup.push(infer!(i));
            }
            Ok(Type::Tuple(tup.into_boxed_slice()))
        },
        /*Syntax::LetTuple(ref mut decl, ref mut t1, ref mut t2) => {
            let a = infer!(t1);
            unify!(Type::Tuple(decl.iter().map(|&(_, ref x)| x.clone()).collect::<Vec<_>>().into_boxed_slice()), a.clone());
            let mut e = TypeEnv::new();
            for a in decl.iter_mut() {
                a.1.apply(gsubst);
                let mut sa = a.1.clone(); sa.apply(gsubst);
                let mut se = env.clone(); se.apply(gsubst);
                let bind: HashSet<_> = sa.free_variables().difference(&(se.free_variables())).cloned().collect();
                e.push(a.0.clone(), TypeScheme{ bind: bind, body: sa });
            }

            let n = e.seq.len();
            env.seq.append(&mut e.seq);
            let b = infer!(t2);
            for _ in 0..n {
                env.pop();
            }
            Ok(b)
        },*/
        _=>panic!(),
    }
}


/*
pub fn test(){
    println!("\n\ntyping::test");
    let e = TypeEnv{seq: vec![(Id("i".to_string()),TypeScheme{bind:HashSet::new(),body:Type::Var(42)}),(Id("j".to_string()),TypeScheme{bind:HashSet::new(),body:Type::Var(41)})]};
    let t = Type::Fun(Box::new([Type::Var(42),Type::Var(43),Type::Var(44),Type::Var(44)]),Box::new(Type::Tuple(Box::new([Type::Var(44),Type::Var(43)]))));
    println!("env:{:?} type:{:?}",e,t);
    
    let s = t.generalize(&e);
    println!("scheme:{:?}",s);

    let mut vg = VarGenerater::new();

    let mut nt = s.instantiate(&mut vg); println!("instance:{:?}",nt);
    let mut nt2 = s.instantiate(&mut vg); println!("instance2:{:?}",nt2);

    /*
    let mut sub = TypeSubst::new();
    sub.push(0,nt2.clone());
    sub.push(1,Type::Bool);
    println!("substisution:{:?}",sub);
    
    let nt = sub.apply(nt); println!("applyed nt:{:?}",nt);
    let nt2 = sub.apply(nt2); println!("applyed nt2:{:?}",nt2);
    */

    let subst = unify(vec![(nt.clone(), nt2.clone())]).unwrap();
    println!("unify subst:{:?}",subst);
    nt.apply(&subst); println!("unified nt:{:?}",nt);
    nt2.apply(&subst); println!("unified nt2:{:?}",nt2);
    println!("");
    {
    let mut nt = Type::Tuple(Box::new([Type::Var(0),Type::Var(1),Type::Var(0)]));
    let mut nt2 = Type::Tuple(Box::new([Type::Int,Type::Unit,Type::Var(42)]));
    println!("nt  :{:?}",nt);
    println!("nt2 :{:?}",nt2);
    let subst = unify(vec![(nt.clone(), nt2.clone())]).unwrap();
    println!("unify subst:{:?}",subst);
    nt.apply(&subst); println!("unified nt:{:?}",nt);
    nt2.apply(&subst); println!("unified nt2:{:?}",nt2);
    println!("");
    }
    
    {
    let mut nt = Type::Tuple(Box::new([Type::Var(0),Type::Var(1),Type::Var(0)]));
    let mut nt2 = Type::Tuple(Box::new([Type::Var(42),Type::Unit,Type::Int]));
    println!("nt  :{:?}",nt);
    println!("nt2 :{:?}",nt2);
    let subst = unify(vec![(nt.clone(), nt2.clone())]).unwrap();
    println!("unify subst:{:?}",subst);
    nt.apply(&subst); println!("unified nt:{:?}",nt);
    nt2.apply(&subst); println!("unified nt2:{:?}",nt2);
    println!("");
    }


    /*{
    let mut nt = Type::Tuple(Box::new([Type::Var(0),Type::Var(1),Type::Var(0)]));
    let mut nt2 = Type::Tuple(Box::new([Type::Int,Type::Unit,Type::Var(42)]));
    println!("nt  :{:?}",nt);
    println!("nt2 :{:?}",nt2);
    unify_mut(&mut nt, &mut nt2);
    println!("unified nt:{:?}",nt);
    println!("unified nt2:{:?}",nt2);
    }*/
}
*/


/*


   let y = (let f = \x.x+1 in f)
   in y
   :: Int -> Int

   wrong Algorithm J
   0@J(. , let y = (let f = \x.x+1 in f) in y) = B = v2->Int
        A = 1@J(. , let f = \x.x+1 in f) = v1->Int
        B = 5@J(.,y:.A , y) = 5@(.,y:v1.v1->Int , y) = v2->Int

    1@J(. , let f = \x.x+1 in f) = B = v1->Int
        A = 2@J(. , \x.x+1) = v0->Int
        B = 4@J(.,f:.A , f) = 4@J(.,f:v0.v0->Int , f) = v1->Int

    2@J(. , \x.x+1) = v0->A = v0->Int
        A = 3@J(.,x:v0 , x+1) = Int

    3@J(.,x:v0 , x+1) = Int
        E <- {v0 |-> Int}

    4@J(.,f:v0.v0->Int , f) = (v0->Int)[v0 |-> v1] = v1->Int

    5@J(.,y:v1.v1->Int , y) = (v1->Int)[v1 |-> v2] = v2->Int

    right Algorithm J
    0@J(. , let y = (let f = \x.x+1 in f) in y) = B = v0->Int
        A = 1@J(. , let f = \x.x+1 in f) = v0->Int
        B = 5@J(.,y:gen(A) , y) = 5@(.,y:v0->Int , y) = v0->Int
        gen(A) = fv(EA)\fv(.) . A = fv(Int->Int)\_ . v0->Int
               = _\_ . v0->Int = v0->Int

    1@J(. , let f = \x.x+1 in f) = B = v0->Int
        A = 2@J(. , \x.x+1) = v0->Int
        B = 4@J(.,f:.A , f) = 4@J(.,f:gen(A) , f) = 4@J(.,f:v0->Int , f) = v0->Int
        gen(A) = fv(EA)\fv(.) . v0->Int = fv(Int->Int)\fv(.) . v0->Int
               = _\_ . v0->Int = _.v0->Int = v0->Int

    2@J(. , \x.x+1) = v0->A = v0->Int
        A = 3@J(.,x:v0 , x+1) = Int

    3@J(.,x:v0 , x+1) = Int
        E <- {v0 |-> Int}

    4@J(.,f:v0->Int , f) = (v0->Int)[] = v0->Int

    5@J(.,y:v0->Int , y) = (v0->Int)[] = v0->Int


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
                let y = (let rec f x = if x > 0 then x * f x - 1 else 1 in f)
                in y
                ";



    0J(. , let i = \a.a in let c = \x.let f = \y.i x in f in c 0) = 
        1J(. , \a.a) = v0->v0
        2J(.,i:v0.v0->v0 , let c = \x.let f = \y.i x in f in c 0) = 

    2J(.,i:v0.v0->v0 , let c = \x.let f = \y.i x in f in c 0)
        3J(.,i:v0.v0->v0 , \x.let f = \y.i x in f) = 
        J(.,i:v0.v0->v0,c: , c 0) = 

    3J(.,i:v0.v0->v0 , \x.let f = \y.i x in f) = v1->
        J(.,i:v0.v0->v0,x:v1 , let f = \y.i x in f) = 

    J(.,i:v0.v0->v0,x:v1 , let f = \y.i x in f) = 
        J(.,i:v0.v0->v0,x:v1 , \y.i x) = 
        J(.,i:v0.v0->v0,x:v1,f: ,  f) = 

    J(.,i:v0.v0->v0,x:v1 , \y.i x) = 
        J(.,i:v0.v0->v0,x:v1,y:v2 , i x) = 
        i = v3->v3
        u(v1->v4,v3->v3)
    

*/

