
fun sum(a: Int, b: Int): Int {
    return a + b
}

fun sum2(a: Int, b: Int) = a + b

fun sum3(a: Int, b: Int): Unit {
    println("sum of $a and $b is ${a + b}")
}

fun functions() {
    println(sum(3, 5))
    println(sum2(3, 5))
    sum3(3, 5)
}

val PI = 3.14

fun variables() {
    val a: Int = 1
    val b = 2
    val c: Int
    c = 3

    println("a = $a, b = $b, c = $c")

    var x = 5
    x += 1
    println("x = $x")

    println("PI = $PI")
}

// magic comment
/* better comment */

fun stringTemplates() {
    var a = 1
    val s1 = "a is $a"
    a = 2

    val s2 = "${s1.replace("is", "was")}, but now is $a"
    println(s2)
}

fun maxOf(a: Int, b: Int): Int {
    if (a > b)
        return a
    else
        return b
}

fun maxOf2(a: Int, b: Int) = if (a > b) a else b

fun parseInt(str: String): Int? {
    return str.toIntOrNull()
}

fun printProduct(arg1: String, arg2: String) {
    val x = parseInt(arg1)
    val y = parseInt(arg2)

    if (x != null && y != null)
        println(x * y)
    else
        println("either '$arg1' or '$arg2' is not a number")
}

fun getStringLength(obj: Any): Int? {
    if (obj is String)
        return obj.length

    return null
}

fun getStringLength2(obj: Any): Int? {
    if (obj !is String)
        return null

    return obj.length
}

fun loops() {
    val items = listOf("apple", "banana", "kiwi")

    for (item in items)
        println(item)

    for (index in items.indices)
        println("item at $index is ${items[index]}")

    var idx = 0
    while (idx < items.size) {
        println("item at $idx is ${items[idx]}")
        idx++
    }

    if (10 in 1..12)
        println("fits in range")

     if (18 !in 1..12)
        println("does not fit in range")

    if (7 !in items.indices)
        println("index not in range")

    for (x in 1..5)
        print(x)
    println()

     for (x in 1..18 step 2)
        print("$x ")
    println()
}

fun describe(obj: Any): String =
        when (obj) {
            1 -> "One"
            "Hello" -> "Greeting"
            is Long -> "Long"
            !is String -> "Not a String"
            else -> "Unknown"
        }

fun collections() {
    val items = listOf("apple", "banana", "kiwi", "avocado")

    when {
        "kiwi" in items -> println("kiwi's there")
        "orange" in items -> println("juicy")
        "apple" in items -> println("apple's fine too")
    }

    items.filter { it.startsWith("a") }
            .sortedBy { it }
            .map { it.toUpperCase() }
            .forEach {println(it)}

}

interface RectangleProperties {
    val isSquare: Boolean

    fun test(a: Int): Boolean
}

class Rectangle(var height: Double, var length: Double) : RectangleProperties {
    override val isSquare: Boolean
        get() = length == height

    override fun test(a: Int): Boolean {
        return true
    }
}

fun basics() {
    functions()
    variables()
    stringTemplates()
    printProduct("2", "3")
    printProduct("2", "3x")

    fun printLength(obj: Any) {
        println("'$obj' string length is ${getStringLength(obj) ?: ".. err, not a string"}")
    }

    printLength("something")
    printLength(1000)
    printLength(listOf(Any()))
    printLength(listOf("test", "test"))

    loops()
    collections()

    val rect = Rectangle(8.0, 8.0)


    println()
}
