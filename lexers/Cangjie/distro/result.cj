package simple_math_interpreter.ext

public enum Result<T, E> {
    | Ok(T)
    | Err(E)
}

public interface ResultExt {}

extend<T, E> Result<T, E> <: ResultExt {
    public func map<U>(f: (T) -> U): Result<U, E> {
        match (this) {
            case Ok(v) => Ok(f(v))
            case Err(e) => Err(e)
        }
    }

    public func mapErr<F>(f: (E) -> F): Result<T, F> {
        match (this) {
            case Ok(t) => Ok(t)
            case Err(e) => Err(f(e))
        }
    }

    public func andThen<U>(f: (T) -> Result<U, E>): Result<U, E> {
        match (this) {
            case Ok(v) => f(v)
            case Err(e) => Err(e)
        }
    }
}
