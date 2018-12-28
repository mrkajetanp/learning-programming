import kotlinx.coroutines.*

suspend fun doTask(time: Long, message: String) {
    delay(time)
    println(message)
}

fun main(args: Array<String>) = runBlocking {
    launch { doTask(1000, "Hello")}

    coroutineScope {
        launch { doTask(500, "World") }

        delay(100L)
        println("Task from coroutine scope")
    }

    println("Coroutine scope is over")
}