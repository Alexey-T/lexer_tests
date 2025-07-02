package simple_math_interpreter.ext

public interface OptionExt {}

extend<T> Option<T> <: OptionExt {
    public func map<U>(f: (T) -> U): Option<U> {
        match (this) {
            case Some(v) => Some(f(v))
            case None => None
        }
    }

    public func okOr<E>(e: E): Result<T, E> {
        match (this) {
            case Some(v) => Ok(v)
            case None => Err(e)
        }
    }
}
