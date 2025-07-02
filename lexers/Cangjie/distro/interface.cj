// 基于接口和扩展，实现简易信号系统仿真
// LJJ 2024.12.10
import std.math.*

interface Slot {
    func compute(t: Float64): Float64
    operator func >>(that: Slot): Slot {
        that << this
        that
    }
    operator func <<(that: Slot): Slot { this }
}

extend Float64 <: Slot {
    public func compute(t: Float64): Float64 {
        return this
    }
}

class Wave <: Slot {
    public Wave(let freq: Float64, let phi: Float64) {}
    public func compute(t: Float64): Float64 {
        return sin(2.0 * Float64.PI * freq * t + phi)
    }
}

class Mul <: Slot {
    public Mul(let a: Slot, let b: Slot) {}
    public func compute(t: Float64): Float64 {
        a.compute(t) * b.compute(t)
    }
}

class Integrator <: Slot {
    var input: ?Slot = None
    var sum = 0.0
    public Integrator(let dt: Float64) {}
    public func compute(t: Float64): Float64 {
        sum += dt * input.getOrThrow().compute(t)
        return sum
    }
    public operator func <<(that: Slot): Slot {
        input = Some(that)
        this
    }
}

main() {
    const DT = 0.001
    let left = 1.0 >> Integrator(DT)
    let right = Wave(0.5 / Float64.PI, 0.0)
    let flow = Mul(left, right) >> Integrator(DT)
    for (t in 0..1000) {
        println(flow.compute(Float64(t) * DT))
    }
}