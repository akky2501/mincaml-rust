
use id::Id;
use typing::Type;

#[derive(Debug)]
pub enum Syntax {
    Unit,
    Bool(bool),
    Int(i64),
    Not(Box<Syntax>),
    Neg(Box<Syntax>),
    Add(Box<Syntax>, Box<Syntax>),
    Sub(Box<Syntax>, Box<Syntax>),
    Mul(Box<Syntax>, Box<Syntax>),
    Div(Box<Syntax>, Box<Syntax>),
    Eq(Box<Syntax>, Box<Syntax>),
    LE(Box<Syntax>, Box<Syntax>),
    If(Box<Syntax>, Box<Syntax>, Box<Syntax>),
    Let((Id,Type), Box<Syntax>, Box<Syntax>),
    Var(Id),
    LetRec(FunDef, Box<Syntax>),
    App(Box<Syntax>, Box<[Syntax]>, Type),
    Tuple(Box<[Syntax]>),
    LetTuple(Box<[(Id, Type)]>, Box<Syntax>, Box<Syntax>),
}

#[derive(Debug)]
pub struct FunDef {
    pub name: (Id, Type),
    pub args: Box<[(Id, Type)]>,
    pub body: Box<Syntax>,
}


