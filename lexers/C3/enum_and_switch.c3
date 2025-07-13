import std::io;

enum Height : uint {
    LOW,
    MEDIUM,
    HIGH,
}

fn void demo_enum(Height h) {
    switch (h) {
        case LOW:
        case MEDIUM:
            io::printn("Not high");
            // Implicit break.
        case HIGH:
            io::printn("High");
    }

    // This also works
    switch (h) {
        case LOW:
        case MEDIUM:
            io::printn("Not high");
            // Implicit break.
        case Height.HIGH:
            io::printn("High");
    }

    // Completely empty cases are not allowed.
    switch (h) {
        case LOW:
            break; // Explicit break required, since switches can't be empty.
        case MEDIUM:
            io::printn("Medium");
        case HIGH:
            break;
    }

    // special checking of switching on enum types
    switch (h) {
        case LOW:
        case MEDIUM:
        case HIGH:
            break;
        default:    // warning: default label in switch which covers all enumeration value
            break;
    }

    // Using "nextcase" will fallthrough to the next case statement,
    // and each case statement starts its own scope.
    switch (h) {
        case LOW:
            int a = 1;
            io::printn("A");
            nextcase;
        case MEDIUM:
            int a = 2;
            io::printn("B");
            nextcase;
        case HIGH:
            // a is not defined here
            io::printn("C");
    }
}
fn uint special(Height h) {
   uint reply;

    switch (h) {
        case MEDIUM:
        case HIGH:
            reply = 3;
            break;
        default:    
            reply = 4;
            break;
    }
    return reply;
}


fn void main() {
   demo_enum(Height.LOW);
   assert(special(Height.LOW) == 4);
   assert(special(Height.MEDIUM) == 3);
   assert(special(Height.HIGH) == 3);
}
