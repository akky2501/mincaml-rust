use ast::*;
use id::*;

pub fn alpha_transform(expr: &mut Syntax, env: &mut Vec<(Id,Id)>, vg: &mut VarGenerater) -> Result<(), ()> {
    match *expr {
        Syntax::Unit | Syntax::Bool(_) | Syntax::Int(_) => Ok(()),
        Syntax::Not(ref mut t) |
        Syntax::Neg(ref mut t) => alpha_transform(t, env, vg),
        Syntax::Add(ref mut t1, ref mut t2) |
        Syntax::Sub(ref mut t1, ref mut t2) |
        Syntax::Mul(ref mut t1, ref mut t2) |
        Syntax::Div(ref mut t1, ref mut t2) |
        Syntax::Eq(ref mut t1, ref mut t2)  |
        Syntax::LE(ref mut t1, ref mut t2)  => {
            alpha_transform(t1, env, vg)?;
            alpha_transform(t2, env, vg)
        }
        Syntax::If(ref mut c, ref mut t, ref mut f) => {
            alpha_transform(c, env, vg)?;
            alpha_transform(t, env, vg)?;
            alpha_transform(f, env, vg)
        },
        Syntax::Let((ref mut id, _), ref mut t1, ref mut t2) => {
            alpha_transform(t1, env, vg)?;
            let new = vg.gen_id();
            env.push((id.clone(), new.clone()));
            alpha_transform(t2, env, vg)?;
            let _ = env.pop();
            *id = new;
            Ok(())
        },
        Syntax::Var(ref mut id) => {
            if let Some(&(_, ref new)) = env.iter().rev().find(|&&(ref old, _)| *old == *id) {
                *id = new.clone();
                Ok(())
            }
            else { Err(()) }
        },
        Syntax::LetRec(FunDef{ name: (ref mut name, _), ref mut args, ref mut body}, ref mut t) => {

            /*{ // 仮引数の名前重複チェック,MLはしないっぽい
                for &(ref s,_) in args.iter() {
                    if *name == *s {
                        return Err(())
                    }
                }

                for i in 0..args.len() {
                    for j in (i+1)..args.len() {
                        if args[i].0 == args[j].0 {
                            return Err(())
                        }
                    }
                }
            }*/

            let new = vg.gen_id();
            env.push((name.clone(), new.clone()));

            let new_args: Vec<_> = args.to_vec().iter().map(|_| vg.gen_id()).collect();

            for i in 0..args.len() {
                env.push((args[i].0.clone(), new_args[i].clone()));
            }

            alpha_transform(body, env, vg)?;

            for i in 0..new_args.len() {
                let _ = env.pop();
                args[i].0 = new_args[i].clone();
            }

            alpha_transform(t, env, vg)?;
            let _ = env.pop();
            *name = new;

            Ok(())
        },
        Syntax::App(ref mut f, ref mut args, _) => {
            alpha_transform(f, env, vg)?;
            for i in args.iter_mut() {
                alpha_transform(i, env, vg)?;
            }
            Ok(())
        },
        Syntax::Tuple(ref mut t) => {
            for i in t.iter_mut() {
                alpha_transform(i, env, vg)?;
            }
            Ok(())
        },
        Syntax::LetTuple(ref mut decl, ref mut t1, ref mut t2) => {
            {
                for i in 0..decl.len() {
                    for j in (i+1)..decl.len() {
                        if decl[i].0 == decl[j].0 {
                            return Err(())
                        }
                    }
                }
            }
            
            alpha_transform(t1, env, vg)?;

            let new: Vec<_> = decl.to_vec().iter().map(|_| vg.gen_id()).collect();
            for i in 0..decl.len() {
                env.push((decl[i].0.clone(), new[i].clone()));
            }

            alpha_transform(t2, env, vg)?;

            for i in 0..new.len() {
                env.pop();
                decl[i].0 = new[i].clone();
            }

            Ok(())
        },
    }
}


