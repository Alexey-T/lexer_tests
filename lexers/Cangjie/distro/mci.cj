package mci

import std.random.*
import std.unittest.*
import std.collection.*
import std.math.*
import std.time.DateTime
import std.os.*

extend Random {
    public func sample(a: Float64, b: Float64): Float64 {
        return nextFloat64() * (b - a) + a
    }
}

let m: Random = Random()

public func reseed(s: UInt64) {
    m.seed = s
}

public func reseed() {
    m.seed = UInt64(DateTime.now().nanosecond)
}

public func sample(a: Float64, b: Float64): Float64 {
    return m.sample(a, b)
}

public func sample(ab: (Float64, Float64)): Float64 {
    return m.sample(ab[0], ab[1])
}

public func sample(n: Int, a: Float64, b: Float64): Array<Float64> {
    return Array<Float64>(n, {_ => sample(a, b)})
}

public func sample(n: Int, ab: (Float64, Float64)): Array<Float64> {
    return Array<Float64>(n, {_ => sample(ab)})
}

public func sample(ab: Array<(Float64, Float64)>): Array<Float64> {
    return Array<Float64>(ab.size, {i => sample(ab[i])})
}

public func volume(dimensions: Array<(Float64, Float64)>): Float64 {
    return (dimensions |> map<(Float64, Float64), Float64> {
        ab => abs(ab[1] - ab[0])
    } |> reduce<Float64> {
        a, b => a * b
    }).getOrThrow() {
        Exception("volume: empty dimensions")
    }
}

public func volume(n: Int, ab: (Float64, Float64)): Float64 {
    return volume(n, ab[0], ab[1])
}

public func volume(n: Int, a: Float64, b: Float64): Float64 {
    return pow(abs(a - b), n)
}

public func monteCarloIntegral(
    sampleSize: Int,
    dimensions: Array<(Float64, Float64)>,
    f: (Array<Float64>) -> Float64
): Float64 {
    let v = volume(dimensions)
    var accum = 0.0
    for (_ in 0..sampleSize) {
        let x = sample(dimensions)
        accum += f(x)
    }
    return accum / Float64(sampleSize) * v
}

public func monteCarloIntegral(
    sampleSize: Int,
    n: Int,
    dimensions: (Float64, Float64),
    f: (Array<Float64>) -> Float64
): Float64 {
    return monteCarloIntegral(sampleSize, Array<(Float64, Float64)>(n, {_ => dimensions}), f)
}

public func monteCarloIntegralPara(
    sampleSize: Int,
    dimensions: Array<(Float64, Float64)>,
    f: (Array<Float64>) -> Float64
): Float64 {
    let v = volume(dimensions)

    let pc = processorCount()
    let newSampleSize = Int64(sampleSize / pc)
    let results = Array<Future<Float64>>(pc) {
        _ => spawn {
            // println("spawn... ${i}")
            return (0..newSampleSize |> map<Int64, Float64> {
                _ => dimensions |> sample |> f
            } |> reduce<Float64> {
                a, b => a + b
            }).getOrThrow()
        }
    }
    println("newSampleSize: ${newSampleSize * pc}")
    
    var accumResults = (results |> map<Future<Float64>, Float64> {
        r => r.get()
    } |> reduce<Float64> {
        a, b => a + b
    }).getOrThrow()

    return accumResults / Float64(newSampleSize * pc) * v
}

public func monteCarloIntegralPara(
    sampleSize: Int,
    n: Int,
    dimensions: (Float64, Float64),
    f: (Array<Float64>) -> Float64
): Float64 {
    return monteCarloIntegralPara(sampleSize, Array<(Float64, Float64)>(n, {_ => dimensions}), f)
}
