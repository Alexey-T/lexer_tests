// cjlint-ignore -start !G.NAM.02
package cj_debounce_throttle.repeater

public import std.time.Duration

/**
 * 任务类型
 */
public type RepeaterTask = () -> Unit

/**
 * 模式,时间为毫秒
 */
public enum RepeaterMode {
    | Leading(Int)
    | Trailing(Int)
    | Both(Int)

    public prop config: RepeaterConfig {
        get() {
            match (this) {
                case Leading(interval) => RepeaterConfig(
                    interval: Duration.millisecond * interval,
                    leading: true,
                    trailing: false
                )
                case Trailing(interval) => RepeaterConfig(
                    interval: Duration.millisecond * interval,
                    leading: false,
                    trailing: true
                )
                case Both(interval) => RepeaterConfig(
                    interval: Duration.millisecond * interval,
                    leading: true,
                    trailing: true
                )
            }
        }
    }
}

/**
 * 属性配置
 */
public struct RepeaterConfig {
    /**
     * interval 等待间隔时间(毫秒)
     * leading在开头调用
     * trailing在最后调用
     */
    public RepeaterConfig(
        var interval!: Duration,
        var leading!: Bool = false,
        var trailing!: Bool = true
    ) {}
}
// cjlint-ignore -end
