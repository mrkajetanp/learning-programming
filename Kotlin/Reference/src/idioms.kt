import java.io.File

data class Customer(val name: String, val email: String)

fun foo(a: Int = 0, b: String = "bbb") { }

object Resource {
    val name = "Name"
    var test = 8
}

fun idioms() {
    println("*** Idioms ***")

    val customerOne = Customer("John", "test@test.com")

    val list = listOf(1, 2, 3, 4, 5, 8)
    val positives = list.filter { x -> x > 3 }
    println(positives)
    println(list.filter { it > 2 })

    val map = mapOf(Pair("One", 1), Pair("Two", 2))
    val map2 = mapOf("One" to 1, "Two" to 2)

    for ((k, v) in map)
        println("$k -> $v")

    println(map2)
    println(map2["Two"])

    val inclusive = 1..10
    val exclusive = 1 until 10
    val reverse = 10 downTo 1
    val inThere = 7 in 1..10

    val p: String by lazy {
        "test" + " me"
    }

    println(p)

    fun String.spaceToCamelCase(): String = this.toUpperCase()

    println("Test me okay".spaceToCamelCase())
    Resource.test += 8

    val files = File("Test").listFiles()
    println(files?.size)
    println(files?.size ?: "empty")

    val values = mapOf("email" to "ooo")
    val email = values["email"] ?: throw IllegalStateException("Email is missing")

    val mainEmail = email.firstOrNull() ?: ""

    val value = 8
    value?.let {
        // execute if not null
    }

    val mapped = value?.let { "something" } ?: "null"

    fun transform(colour: String): Int {
        return when (colour) {
            "Red" -> 0
            "Green" -> 1
            else -> throw IllegalArgumentException("Invalid colour param value")
        }
    }

    fun test() {
        val result = try {
            transform("test")
        } catch (e: ArithmeticException) {
            throw IllegalStateException(e)
        }
    }

    fun foo(param: Int) {
        val result = if (param == 1) {
            "one"
        } else {
            "three"
        }
    }

    fun arrayOfMinusOnes(size: Int): IntArray {
        return IntArray(size).apply { fill(-1) }
    }

    println(arrayOfMinusOnes(3))

    class Test {
        fun one() = 1
        fun two() = 2
    }

    val test = Test()
    with(test) {
        one()
        two()
    }

    val b: Boolean? = true
    if (b == true) {
        // ...
    } else {
        // 'b' is false or null
    }
}

