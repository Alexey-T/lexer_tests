// cjlint-ignore -start !G.NAM.02 !G.EXP.03
package cj_debounce_throttle.repeater

public import std.sync.Timer

public class Repeater {
    private var lastCallTime: MonoTime = getNowTime()
    private var lastRunTaskTime: MonoTime = getNowTime()
    private var firstRunTask: Bool = false
    // 定时器，只有 trailing 才会使用
    private var timer: ?Timer = None

    // 配置和任务
    private let config: RepeaterConfig
    private let task: RepeaterTask
    private let throttle: Bool

    public init(throttle!: Bool, mode!: RepeaterMode, task!: RepeaterTask) {
        this(throttle: throttle, config: mode.config, task: task)
    }

    public init(throttle!: Bool, config!: RepeaterConfig, task!: RepeaterTask) {
        this.config = config
        this.task = task
        this.throttle = throttle
        this.verifyConfig()
    }

    // release条件下为空实现
    @When[!debug]
    func verifyConfig() {}

    // debug下实现具体逻辑
    @When[debug]
    func verifyConfig() {
        if (!config.leading && !config.trailing) {
            throw IllegalArgumentException('leading和trailing需要至少有一项为true')
        }
    }

    // 是否可以运行任务
    private prop canRunTask: Bool {
        get() {
            if (throttle) {
                let delta = getNowTime() - lastRunTaskTime
                (delta > config.interval) || !this.firstRunTask
            } else {
                let delta = getNowTime() - lastCallTime
                (delta > config.interval) || !this.firstRunTask
            }
        }
    }

    private func dealLeadingCall() {
        // timer不存在，说明没开启过计时。如果设置了头部执行，则立即执行
        if (config.leading && canRunTask) {
            runTask()
        }
    }

    private func dealTrailingCall() {
        if (config.trailing && timer.isNone()) {
            // 这里置为true，是考虑到只有trailing没有leading的情况。假设任务已执行一次
            firstRunTask = true
            lastRunTaskTime = getNowTime()
            // 启动timer
            timer = Timer.after(config.interval) {
                let now = getNowTime()

                if (throttle) {
                    runTask()
                    if (now - lastCallTime > config.interval) {
                        cancel()
                        return None
                    } else {
                        return config.interval
                    }
                } else {
                    if (now - lastCallTime > config.interval) {
                        runTask()
                        // Timer置None，重新计时
                        cancel()
                        // 返回None，让timer生效
                        return None
                    } else {
                        // 定时器距离上次调用call方法的时间间隔
                        let delta = now - lastCallTime
                        // 返回从上次点击后到下一个interval的时间
                        return config.interval - delta
                    }
                }
            }
        }
    }

    public func call(): Unit {
        this.dealLeadingCall()
        this.dealTrailingCall()
        lastCallTime = getNowTime()
    }

    public func cancel(): Unit {
        timer?.cancel()
        timer = None
    }

    private func runTask() {
        task()
        firstRunTask = true
        lastRunTaskTime = getNowTime()
    }
}
// cjlint-ignore -end
