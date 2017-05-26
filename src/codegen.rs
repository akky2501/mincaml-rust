
use id::*;
use typing::*;
use closure::*;


pub fn code_generate(p: Program, ty: Type) {
    println!("{}",
r"
#include<stdio.h>
#include<stdlib.h>
#include<stdarg.h>

typedef void (*func_type)(void);
typedef long long i64;
typedef long long bool;
typedef unsigned char unit;

struct value {
    void* p;
};

struct closure {
    func_type func;
    i64 fv_num;
    struct value fv[0];
};

int stack_top = -1;
struct value stack_pool[0xffff];

void push(struct value x){
    stack_top++;
    stack_pool[stack_top] = x;
}

struct value pop(void){
    struct value r = stack_pool[stack_top];
    stack_top--;
    return r;
}

struct value make_unit(void){
    struct value r;
    r.p = malloc(sizeof(unit));
    *(unit*)(r.p) = 0xff;
    return r;
}

struct value make_int(i64 i){
    struct value r;
    r.p = malloc(sizeof(i64));
    *(i64*)(r.p) = i;
    return r;
}

struct value make_bool(bool b){
    struct value r;
    r.p = malloc(sizeof(bool));
    *(bool*)(r.p) = b;
    return r;
}

struct value make_closure(func_type func, i64 fv_num, ...){
    struct value r;
    va_list args;

    r.p = malloc(sizeof(struct closure)+sizeof(struct value)*fv_num);
    ((struct closure*)(r.p))->func = func;
    ((struct closure*)(r.p))->fv_num = fv_num;

    va_start(args,fv_num);
    for(int i=0;i<fv_num;i++)
        ((struct closure*)(r.p))->fv[i] = va_arg(args, struct value);
    va_end(args);

    return r;
}

"
    );

    let Program { decls: d, code: c } = p;

    for x in d.into_iter() {
        gen_function(x);
    }

    gen_entry(c);

    println!("{}",
r"
void print_result(void){
    struct value r = pop();
"
    );

    gen_type(&ty);
    gen_print_value(&ty);

    println!("{}",
r"
}

int main(void){
    entry_point();
    print_result();
    return 0;
}
"
    );
}

fn gen_function(f: Function) {
    let name = to_func_string(&f.entry.0);
    println!("void {}(void){{", name);
    // take args
    for arg in f.args.iter() {
        println!("    struct value {} = pop(); // arg", to_variable_string(&arg.0));
    }
    // take fv TODO: 順番の固定
    let mut fvs = f.free_variables.keys().into_iter().collect::<Vec<_>>();
    fvs.sort();
    for fv in fvs.into_iter() {
        println!("    struct value {} = pop(); // fv", to_variable_string(&fv));
    }
    gen_code(*f.body);
    println!("}}");
}

fn gen_entry(c: Syntax) {
    println!("void entry_point(void){{");

    gen_code(c);

    println!("}}");
}

// TODO: nest level
fn gen_code(c: Syntax) {
    use self::Syntax::*;
    match c {
        Unit => { println!("    push(make_unit());"); },
        Bool(b) => {
            if b { println!("    push(make_bool(1));"); }
            else { println!("    push(make_bool(0));"); }
        },
        Int(i) => {
            println!("    push(make_int({}));", i);
        },
        Neg(i) => {
            println!("    push(make_int(-*(i64*)({}.p)));", to_variable_string(&i));
        },
        Add(l, r) => {
            println!("    push(make_int(*(i64*)({}.p) + *(i64*)({}.p)));", to_variable_string(&l), to_variable_string(&r));
        },
        Sub(l, r) => {
            println!("    push(make_int(*(i64*)({}.p) - *(i64*)({}.p)));", to_variable_string(&l), to_variable_string(&r));
        },
        Mul(l, r) => {
            println!("    push(make_int(*(i64*)({}.p) * *(i64*)({}.p)));", to_variable_string(&l), to_variable_string(&r));
        },
        Div(l, r) => {
            println!("    push(make_int(*(i64*)({}.p) / *(i64*)({}.p)));", to_variable_string(&l), to_variable_string(&r));
        },
        IfEq(l, r, th, el) => {
            // i64とboolに対して比較しないといけないが、演算が区別されていないためi64とboolのサイズを同じにして、ごまかしている
            // 環境を作って判別すれば解決できる。
            println!("    if(*(long long*)({}.p) == *(long long*)({}.p)){{", to_variable_string(&l), to_variable_string(&r));
            gen_code(*th);
            println!("    }}else{{");
            gen_code(*el);
            println!("    }}");
        },
        IfLE(l, r, th, el) => {
            println!("    if(*(i64*)({}.p) <= *(i64*)({}.p)){{", to_variable_string(&l), to_variable_string(&r));
            gen_code(*th);
            println!("    }}else{{");
            gen_code(*el);
            println!("    }}");
        },
        Let((id, _), v, cont) => {
            gen_code(*v);
            println!("    struct value {} = pop();", to_variable_string(&id));
            gen_code(*cont);
        },
        Var(i) => {
            println!("    push({});", to_variable_string(&i));
        },
        MakeClosure(id, closure, cont) => {
            let Closure{ entry: e, free_variables: fv_set } = closure;
            let mut fv: Vec<_> = fv_set.iter().collect();
            fv.sort_by(|a, b| a.partial_cmp(b).unwrap());
            print!("    struct value {} = make_closure( {}, ", to_variable_string(&id), to_func_string(&e));
            print!("{}", fv.len());
            for v in fv {
                print!(", {}", to_variable_string(&v));
            }
            println!(");");
            gen_code(*cont);
        },
        AppClosure(id, args) => {
            println!("    for(int i=((struct closure *)({0}.p))->fv_num-1; i >= 0;i--) push(((struct closure *)({0}.p))->fv[i]);", to_variable_string(&id)); // push captured free variables
            for arg in args.into_iter().rev() {
                println!("    push({});", to_variable_string(&arg));
            }
            println!("    ((struct closure *)({}.p))->func();", to_variable_string(&id));
        },
        AppDirect(label, args) => {
            for arg in args.into_iter().rev() {
                println!("    push({});", to_variable_string(&arg));
            }
            println!("    {}();", to_func_string(&label));
        },
        //Tuple(Vec<Id>) => { panic!(); },
        //LetTuple(Vec<(Id, Type)>, Id, Box<Syntax>) => { panic!(); },
        _ => panic!(),
    }

}

fn gen_type(ty: &Type) {
}

fn gen_print_value(ty: &Type) {
    println!("    printf(\"result: %lld\\n\", *(i64*)(r.p));");
}

fn to_func_string(label: &Label) -> String {
    let mut s = label.0.clone();
    s.remove(0);
    s.insert_str(0, "func");
    s
}

fn to_variable_string(id: &Id) -> String {
    let mut s = id.0.clone();
    s.remove(0);
    s.insert_str(0, "var");
    s
}

/*

   global stack;

   struct value {
    void* p;
   }

   struct closure {
    void (*func)(void);
    struct value[0];
   }
   
   void function(void) {
    value arg1 = pop();
    value arg2 = pop();
    value free1 = pop();
    value free2 = pop();

    ...

    push(ret);
   }


    let rec succ x = x + 1 in
    let rec id x = x in
    succ (id 2)

    =>

    decls:
        succ x = 
            let t = 1 in
            x + t
        
        id x = x

    code:
        let t0 = 2 in
        let t1 = id t0 in
        succ t1

    =>

    void succ(void){
        value x = pop();
        value t = make_int(1);
        value r = make_int((i64)*(x.p) + (i64)*(t.p));
        push(r);
    }

    void id(void){
        value x = pop();
        push(x);
    }

    int main(void){
        value t0 = make_int(2);
        push(t0);
        id();
        value t1 = pop();
        push(t1);
        value ret = pop();

        print(ret);

        return 0;
    }


    let rec const x = 
        let rec f y = x in
    (const 10) 5

    =>

    decl:
        const x = make_closure(f, {x});
        f {x} y = x;
    code:
        let t1 = 10 in
        let t2 = const t1 in
        let t3 = 5 in
        t2 t3

    =>

    void const(void){
        value x = pop();
        value r = make_closure(f, x);
        push(r);
    }

    void f(void){
        value x = pop();
        value y = pop();
        push(x);
    }

    int main(void){
        value t1 = make_int(10);
        push(t1);
        const();
        value t2 = pop();
        value t3 = make_int(5);
        push(t3);
        push(t2.p->free);
        (t2.p.f)();
        value r = pop();

        print(r);

        return 0;
    }

*/


