import ballerina/lang.'object;

listener ABC ep = new;

service on ep {


    resource function foo(string b) {
    }

    resource function bar(string b) {
    }
}

public type ABC object {

    *'object:Listener;

    public function __start() returns error? {
        error e = error("startError");
        return e;
    }

    public function __gracefulStop() returns error? {
        return ();
    }

    public function __immediateStop() returns error? {
        return ();
    }

    public function __attach(service s, string? name = ()) returns error? {
        return ();
    }

    public function __detach(service s) returns error? {
    }
};

function test1 (){

}
