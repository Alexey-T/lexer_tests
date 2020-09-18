 function f1() {
    yield/foo/g; // division expression
    await/foo/g; // division expression
}

function* f2() {
    yield/foo/g; // yield expression
    await/foo/g; // division expression
}

async function f3() {
    yield/foo/g; // division expression
    await/foo/g; // await expression
}

async function* f4() {
    yield/foo/g; // yield expression
    await/foo/g; // await expression
}