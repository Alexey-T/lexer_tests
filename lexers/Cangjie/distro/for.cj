// 使用 for-in 循环计算输出 2024 年各月干支
// LJJ 2024.12.10

main() {
    let metaArray = ['甲', '乙', '丙', '丁', '戊',
        '己', '庚', '辛', '壬', '癸']
    let noumenonArray = ['寅', '卯', '辰', '巳', '午', '未',
        '申', '酉', '戌', '亥', '子', '丑']
    let year = 2024
    // 年份对应的天干索引
    let metaOfYear = ((year % 10) + 10 - 4) % 10
    // 此年首月对应的天干索引
    var index = (2 * metaOfYear + 3) % 10 - 1
    println("农历 2024 年各月干支：")
    for (noumenon in noumenonArray) {
        print("${metaArray[index]}${noumenon} ")
        index = (index + 1) % 10
    }
}
