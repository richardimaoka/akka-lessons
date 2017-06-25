package my.wrapper

object Wrapper {
  def apply(callerFunctionName: String = "f")(unitFunction: () => Unit): Unit = {
    val header = "----" + callerFunctionName + "------------------------------------------------------------------"
    println(header.substring(0, 60))

    unitFunction() // call the passed function

    Thread.sleep(50)
    println()
  }
}
