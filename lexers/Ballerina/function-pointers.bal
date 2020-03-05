function test1() returns (int){
    function (int, int) returns (int) addFunction = func1;
    return addFunction(1, 2);
}

function func1 (int a, int b) returns (int) {
    int c = a + b;
    return c;
}

function test2 () returns (string) {
    function (int , int ) returns (string) sumFunction = function (int a, int b) returns (string) {
                                       int value =  a + b;
                                       return "sum is " + value.toString();
                                   };
    return sumFunction(1,2);
}

function test3() returns (int){
    int x = test3Callee(1, func1);
    return x;
}

function test3Callee(int a, function (int x, int y) returns (int) func ) returns (int){
    int x = a + func(1, 2);
    return x;
}

function test4() returns (string){
    function (string a, string b) returns (string) foo = test4Callee();
    string v = foo("hello ", "world.");
    return v;
}

function test4Callee() returns (function (string a, string b) returns (string)){
   return function (string x, string y) returns (string){
             string z = x + y;
             return z;
          };
}

function test5() returns (string){
    function (string, float) returns (string) bar = test5Callee();
    return "test5 " + bar("string", 1.0);
}

function test5Callee() returns (function (string, float) returns (string)){
       return test5Ref;
}

function test5Ref(string a, float b) returns (string){
    string c = a + b.toString();
    return c;
}

function test6() returns (string){
    function (string, function (string, float) returns (string)) returns (string) foo = test6Callee();
    return foo("test6 ", test5Ref);
}

function test6Callee() returns (function (string, function (string, float) returns (string)) returns (string)){
       return test6Ref;
}

function test6Ref(string a, function (string, float) returns (string) b) returns (string){
    string c = a + b(a , 1.0);
    return c;
}

function testFuncWithArrayParams () returns (int){
    string[] s = ["me", "myself"];
    function (string[])  returns (int) x = funcWithArrayParams;
    return x(s);
}

function funcWithArrayParams (string[] a) returns (int) {
    return 0;
}

public function getCount(function (int , int ) returns (int) sumFunction, string first, string last) returns string {
    return first + ": " + sumFunction(4, 2).toString() + " " + last;
}

function testFunctionPointerAsFuncParam() returns [int, string] {
    function (int , int ) returns (int) sumFunction = function (int a, int b) returns (int) {
                               int value =  a + b;
                               return value;
                           };

    string s = getCount(sumFunction, "Total", "USD");
    return [sumFunction(5, 8), s];
}

function testAnyToFuncPointerConversion_1() returns (int|error) {
    any anyFunc = function (int a, int b) returns (int) {
                int value =  a + b;
                return value;
            };

    function (int , int ) returns (int) sumFunction = <function (int , int ) returns (int)> anyFunc;
    return sumFunction(3, 2);
}

type Person object {
    int age;

    function __init (int age) {
        self.age = age;
    }

    function getAge() returns (int) {
        return self.age;
    }
};

type Student object {
    int age = 40;
    private int marks;

    function getAge() returns (int) {
        return self.age;
    }
};

function testFuncPointerConversion() returns (int) {
    function (Person) returns (int) personFunc = function (Person p) returns (int) {
                return p.getAge();
            };

    function (Student) returns (int) studentFunc = personFunc;
    Student s = new Student();
    return studentFunc(s);
}

function testAnyToFuncPointerConversion_2() returns (int|error) {
    any anyFunc = function (Student s) returns (int) {
                        return s.getAge();
                    };

    function (Person) returns (int) personFunc = <function (Person) returns (int)> anyFunc;
    Person p = new Person(23);
    return personFunc(p);
}

function testInTypeGuard() returns int {
    (function (string[]) returns int) | error func = funcWithArrayParams;
    if (func is (function (string[]) returns int)) {
        return func([""]);
    }
    return 1;
}

function bar() returns string {
     return "12";
}

function testSubTypingWithAny() returns any {
    function () returns any ff = bar;
    any s = ff();
    return s;
}

var f1 = function() returns string {
    return "f1";
};

public function testGlobalFunctionPointerVar() returns string {
   return f1();
}

function() returns string f2 = function() returns string {
    return "f2";
};

public function testGlobalFunctionPointerTyped() returns string {
   return f2();
}

public function testVoidFunctionAsUnionReturnFunction() returns string {
    string s = "value";
    xyz(function () {
        s += " - updated through lambda";
    });

    return s;
}

function xyz(function() returns any|error func) {
    var result = func();
}
