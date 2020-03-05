import ballerina/runtime;
import ballerinax/java;

int i = 0;

function timerTest() returns int {
    Callback c = new;
    startTimer(100, 3, c);
    runtime:sleep(500);
    return i;
}

public type Callback object {

    public function exec() {
        i = i + 1;
    }
};

// Interop functions
public function startTimer(int interval, int count, Callback c) = @java:Method {
    class:"org/ballerinalang/nativeimpl/jvm/tests/Timer"
} external;
