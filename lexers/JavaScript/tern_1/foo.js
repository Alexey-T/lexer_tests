var bar = require("./bar")

function yoba(a, b) {
    return a + b;
}

function foo(x) {
    return x;
}

yoba(foo(yoba(1, 2)), 3)
bar.long_name_yoba()
