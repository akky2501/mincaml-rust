/*
src:

        let rec greatest_prime n =
            let rec is_prime x y =
                let rec mod x y = x - (x/y)*y in
                if x > y then (if mod x y = 0 then 0 else is_prime x (y+1)) else 1 in
            if is_prime n 2 = 1 then n else greatest_prime (n-1) in
        greatest_prime 65535
    
[[parse phase... begin]]
ast:
LetRec(FunDef { name: (Id("greatest_prime"), Var(10)), args: [(Id("n"), Var(11))], body: LetRec(FunDef { name: (Id("is_prime"), Var(5)), args: [(Id("x"), Var(6)), (Id("y"), Var(7))], body: LetRec(FunDef { name: (Id("mod"), Var(0)), args: [(Id("x"), Var(1)), (Id("y"), Var(2))], body: Sub(Var(Id("x")), Mul(Div(Var(Id("x")), Var(Id("y"))), Var(Id("y")))) }, If(Not(LE(Var(Id("x")), Var(Id("y")))), If(Eq(App(Var(Id("mod")), [Var(Id("x")), Var(Id("y"))], Var(3)), Int(0)), Int(0), App(Var(Id("is_prime")), [Var(Id("x")), Add(Var(Id("y")), Int(1))], Var(4))), Int(1))) }, If(Eq(App(Var(Id("is_prime")), [Var(Id("n")), Int(2)], Var(8)), Int(1)), Var(Id("n")), App(Var(Id("greatest_prime")), [Sub(Var(Id("n")), Int(1))], Var(9)))) }, App(Var(Id("greatest_prime")), [Int(65535)], Var(12)))

[[parse phase... end]]

[[alpha transform phase... begin]]
alpha transformed ast:
LetRec(FunDef { name: (Id("@13"), Var(10)), args: [(Id("@14"), Var(11))], body: LetRec(FunDef { name: (Id("@15"), Var(5)), args: [(Id("@16"), Var(6)), (Id("@17"), Var(7))], body: LetRec(FunDef { name: (Id("@18"), Var(0)), args: [(Id("@19"), Var(1)), (Id("@20"), Var(2))], body: Sub(Var(Id("@19")), Mul(Div(Var(Id("@19")), Var(Id("@20"))), Var(Id("@20")))) }, If(Not(LE(Var(Id("@16")), Var(Id("@17")))), If(Eq(App(Var(Id("@18")), [Var(Id("@16")), Var(Id("@17"))], Var(3)), Int(0)), Int(0), App(Var(Id("@15")), [Var(Id("@16")), Add(Var(Id("@17")), Int(1))], Var(4))), Int(1))) }, If(Eq(App(Var(Id("@15")), [Var(Id("@14")), Int(2)], Var(8)), Int(1)), Var(Id("@14")), App(Var(Id("@13")), [Sub(Var(Id("@14")), Int(1))], Var(9)))) }, App(Var(Id("@13")), [Int(65535)], Var(12)))
[[alpha transform phase... end]]

[[typing phase... begin]]
type: Int
env:
TypeEnv { seq: [] }
subst:
TypeSubst { equations: [(1, Var(2)), (2, Int), (0, Fun([Int, Int], Int)), (6, Int), (7, Int), (21, Int), (5, Fun([Int, Int], Var(22))), (22, Int), (11, Int), (23, Int), (10, Fun([Int], Var(24))), (24, Int), (25, Int)] }
typed ast:
LetRec(FunDef { name: (Id("@13"), Fun([Int], Int)), args: [(Id("@14"), Int)], body: LetRec(FunDef { name: (Id("@15"), Fun([Int, Int], Int)), args: [(Id("@16"), Int), (Id("@17"), Int)], body: LetRec(FunDef { name: (Id("@18"), Fun([Int, Int], Int)), args: [(Id("@19"), Int), (Id("@20"), Int)], body: Sub(Var(Id("@19")), Mul(Div(Var(Id("@19")), Var(Id("@20"))), Var(Id("@20")))) }, If(Not(LE(Var(Id("@16")), Var(Id("@17")))), If(Eq(App(Var(Id("@18")), [Var(Id("@16")), Var(Id("@17"))], Int), Int(0)), Int(0), App(Var(Id("@15")), [Var(Id("@16")), Add(Var(Id("@17")), Int(1))], Var(22))), Int(1))) }, If(Eq(App(Var(Id("@15")), [Var(Id("@14")), Int(2)], Int), Int(1)), Var(Id("@14")), App(Var(Id("@13")), [Sub(Var(Id("@14")), Int(1))], Var(24)))) }, App(Var(Id("@13")), [Int(65535)], Int))
substituted type: Int
[[typing phase... end]]

[[knormal transform phase... begin]]
k-normal form:
LetRec(FunDef { name: (Id("@13"), Fun([Int], Int)), args: [(Id("@14"), Int)], body: LetRec(FunDef { name: (Id("@15"), Fun([Int, Int], Int)), args: [(Id("@16"), Int), (Id("@17"), Int)], body: LetRec(FunDef { name: (Id("@18"), Fun([Int, Int], Int)), args: [(Id("@19"), Int), (Id("@20"), Int)], body: Let((Id("@31"), Int), Let((Id("@28"), Int), Div(Id("@19"), Id("@20")), Mul(Id("@28"), Id("@20"))), Sub(Id("@19"), Id("@31"))) }, IfLE(Id("@16"), Id("@17"), Int(1), Let((Id("@37"), Int), App(Id("@18"), [Id("@16"), Id("@17")]), Let((Id("@38"), Int), Int(0), IfEq(Id("@37"), Id("@38"), Int(0), Let((Id("@43"), Int), Let((Id("@42"), Int), Int(1), Add(Id("@17"), Id("@42"))), App(Id("@15"), [Id("@16"), Id("@43")]))))))) }, Let((Id("@47"), Int), Let((Id("@46"), Int), Int(2), App(Id("@15"), [Id("@14"), Id("@46")])), Let((Id("@48"), Int), Int(1), IfEq(Id("@47"), Id("@48"), Var(Id("@14")), Let((Id("@52"), Int), Let((Id("@51"), Int), Int(1), Sub(Id("@14"), Id("@51"))), App(Id("@13"), [Id("@52")])))))) }, Let((Id("@54"), Int), Int(65535), App(Id("@13"), [Id("@54")])))
k-normal form type:
Int
flatted k-normal form:
LetRec(FunDef { name: (Id("@13"), Fun([Int], Int)), args: [(Id("@14"), Int)], body: LetRec(FunDef { name: (Id("@15"), Fun([Int, Int], Int)), args: [(Id("@16"), Int), (Id("@17"), Int)], body: LetRec(FunDef { name: (Id("@18"), Fun([Int, Int], Int)), args: [(Id("@19"), Int), (Id("@20"), Int)], body: Let((Id("@28"), Int), Div(Id("@19"), Id("@20")), Let((Id("@31"), Int), Mul(Id("@28"), Id("@20")), Sub(Id("@19"), Id("@31")))) }, IfLE(Id("@16"), Id("@17"), Int(1), Let((Id("@37"), Int), App(Id("@18"), [Id("@16"), Id("@17")]), Let((Id("@38"), Int), Int(0), IfEq(Id("@37"), Id("@38"), Int(0), Let((Id("@42"), Int), Int(1), Let((Id("@43"), Int), Add(Id("@17"), Id("@42")), App(Id("@15"), [Id("@16"), Id("@43")])))))))) }, Let((Id("@46"), Int), Int(2), Let((Id("@47"), Int), App(Id("@15"), [Id("@14"), Id("@46")]), Let((Id("@48"), Int), Int(1), IfEq(Id("@47"), Id("@48"), Var(Id("@14")), Let((Id("@51"), Int), Int(1), Let((Id("@52"), Int), Sub(Id("@14"), Id("@51")), App(Id("@13"), [Id("@52")])))))))) }, Let((Id("@54"), Int), Int(65535), App(Id("@13"), [Id("@54")])))
[[knormal transform phase... end]]

[[closure transform phase... begin]]
known set {Label("@15"), Label("@13"), Label("@18")}
program:
Program { decls: [Function { entry: (Label("@18"), Fun([Int, Int], Int)), free_variables: {}, args: [(Id("@19"), Int), (Id("@20"), Int)], body: Let((Id("@28"), Int), Div(Id("@19"), Id("@20")), Let((Id("@31"), Int), Mul(Id("@28"), Id("@20")), Sub(Id("@19"), Id("@31")))) }, Function { entry: (Label("@15"), Fun([Int, Int], Int)), free_variables: {}, args: [(Id("@16"), Int), (Id("@17"), Int)], body: IfLE(Id("@16"), Id("@17"), Int(1), Let((Id("@37"), Int), AppDirect(Label("@18"), [Id("@16"), Id("@17")]), Let((Id("@38"), Int), Int(0), IfEq(Id("@37"), Id("@38"), Int(0), Let((Id("@42"), Int), Int(1), Let((Id("@43"), Int), Add(Id("@17"), Id("@42")), AppDirect(Label("@15"), [Id("@16"), Id("@43")]))))))) }, Function { entry: (Label("@13"), Fun([Int], Int)), free_variables: {}, args: [(Id("@14"), Int)], body: Let((Id("@46"), Int), Int(2), Let((Id("@47"), Int), AppDirect(Label("@15"), [Id("@14"), Id("@46")]), Let((Id("@48"), Int), Int(1), IfEq(Id("@47"), Id("@48"), Var(Id("@14")), Let((Id("@51"), Int), Int(1), Let((Id("@52"), Int), Sub(Id("@14"), Id("@51")), AppDirect(Label("@13"), [Id("@52")]))))))) }], code: Let((Id("@54"), Int), Int(65535), AppDirect(Label("@13"), [Id("@54")])) }
[[closure transform phase... end]]

*/


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


void func18(void){
    struct value var19 = pop(); // arg
    struct value var20 = pop(); // arg
    push(make_int(*(i64*)(var19.p) / *(i64*)(var20.p)));
    struct value var28 = pop();
    push(make_int(*(i64*)(var28.p) * *(i64*)(var20.p)));
    struct value var31 = pop();
    push(make_int(*(i64*)(var19.p) - *(i64*)(var31.p)));
}
void func15(void){
    struct value var16 = pop(); // arg
    struct value var17 = pop(); // arg
    if(*(i64*)(var16.p) <= *(i64*)(var17.p)){
    push(make_int(1));
    }else{
    push(var17);
    push(var16);
    func18();
    struct value var37 = pop();
    push(make_int(0));
    struct value var38 = pop();
    if(*(long long*)(var37.p) == *(long long*)(var38.p)){
    push(make_int(0));
    }else{
    push(make_int(1));
    struct value var42 = pop();
    push(make_int(*(i64*)(var17.p) + *(i64*)(var42.p)));
    struct value var43 = pop();
    push(var43);
    push(var16);
    func15();
    }
    }
}
void func13(void){
    struct value var14 = pop(); // arg
    push(make_int(2));
    struct value var46 = pop();
    push(var46);
    push(var14);
    func15();
    struct value var47 = pop();
    push(make_int(1));
    struct value var48 = pop();
    if(*(long long*)(var47.p) == *(long long*)(var48.p)){
    push(var14);
    }else{
    push(make_int(1));
    struct value var51 = pop();
    push(make_int(*(i64*)(var14.p) - *(i64*)(var51.p)));
    struct value var52 = pop();
    push(var52);
    func13();
    }
}
void entry_point(void){
    push(make_int(65535));
    struct value var54 = pop();
    push(var54);
    func13();
}

void print_result(void){
    struct value r = pop();

    printf("result: %lld\n", *(i64*)(r.p));

}

int main(void){
    entry_point();
    print_result();
    return 0;
}

