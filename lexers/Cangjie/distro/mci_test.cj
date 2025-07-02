package mci;
import std.collection.*
import std.math.*
import std.format.*
import mci.macros.*

@Test
func testReseed(){
    reseed(42)
    let r1 = sample(0.0, 1.0)
    reseed(42)
    let r2 = sample(0.0, 1.0)
    @ExpectFloat64(r1, r2, 1e-18)
}

@Test
func testSample() {
    let a: Float64 = 1.0
    let b: Float64 = 2.0
    let x: Float64 = sample(a, b)
    @Assert(a <= x && x < b)
}

@Test
func testSampleTuple() {
    let ab: (Float64, Float64) = (1.0, 2.0)
    let x: Float64 = sample(ab)
    @Assert(ab[0] <= x && x < ab[1])
}

@Test
func testSampleN() {
    let n: Int = 10
    let a: Float64 = 1.0
    let b: Float64 = 2.0
    let x: Array<Float64> = sample(n, a, b)
    @Assert(x.size == n)
    for (xi in x) {
        @Assert(a <= xi && xi < b)
    }
}

@Test
func testSampleNTuple() {
    let n: Int = 10
    let ab: (Float64, Float64) = (1.0, 2.0)
    let x: Array<Float64> = sample(n, ab)
    @Assert(x.size == n)
    for (xi in x) {
        @Assert(ab[0] <= xi && xi < ab[1])
    }
}

@Test
func testSampleArray() {
    let ab: Array<(Float64, Float64)> = [(1.0, 2.0),
        (3.0, 4.0),
        (5.0, 6.0)]
    let x: Array<Float64> = sample(ab)
    @Assert(x.size == ab.size)
    for (i in 0..ab.size) {
        @Assert(ab[i][0] <= x[i] && x[i] < ab[i][1])
    }
}

@Test
func testVolume() {
    let dimensions: Array<(Float64, Float64)> = [
        (1.0, 2.0),
        (3.0, 4.0),
        (5.0, 6.0)
    ]
    let v: Float64 = volume(dimensions)
    @Assert(v == 1.0 * 1.0 * 1.0)
}

@Test
func testVolumeN() {
    let n: Int = 3
    let a: Float64 = 1.0
    let b: Float64 = 2.0
    let v: Float64 = volume(n, a, b)
    @Assert(v == 1.0 * 1.0 * 1.0)
}

@Test
func testMonteCarloIntegral() {
    let sampleSize: Int = 1000
    let dimensions: Array<(Float64, Float64)> = [
        (0.0, 1.0),
        (0.0, 1.0),
        (0.0, 1.0)
    ]
    let f: (Array<Float64>) -> Float64 = {
        x => (x |> map {xi => xi * xi} |> reduce {a, b => a + b}).getOrThrow()
    }
    let integral: Float64 = monteCarloIntegral(sampleSize, dimensions, f)
    @Expect(integral, 1.0)
    @ExpectFloat64(integral, 1.0, 1e-3)
    @ExpectFloat64(integral, 1.0, 1e-2)
}

@Test
func testMonteCarloIntegralN() {
    let sampleSize: Int = 1000
    let n: Int = 3
    let dimensions: (Float64, Float64) = (0.0, 1.0)
    let f: (Array<Float64>) -> Float64 = {
        x => (x |> map {xi => xi * xi} |> reduce {a, b => a + b}).getOrThrow()
    }
    let integral: Float64 = monteCarloIntegral(sampleSize, n, dimensions, f)
    @Expect(integral, 1.0)
    @ExpectFloat64(integral, 1.0, 1e-3)
    @ExpectFloat64(integral, 1.0, 1e-2)
}