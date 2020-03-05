import ballerinax/java;

public function interopWithErrorReturn() returns string {
    error e = acceptStringOrErrorReturn("example error with given reason");
    return e.reason();
}

public function acceptStringOrErrorReturn(string s) returns error = @java:Method {
    class:"org/ballerinalang/nativeimpl/jvm/tests/StaticMethods"
} external;
