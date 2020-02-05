import java.lang.String
import java.lang.StringBuilder
import java.util.function.Consumer
import java.util.{Comparator, Random}

object Main {

  def generateInt: Int = {
    val count = (Math.random * 30).toInt
    count
  }

  def generateString: String = {
    val symbols = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdyfghijklmnopqrstuvwxyz1234567890 "
    val randString = new StringBuilder
    val count = (Math.random * 30).toInt
    for (i <- 0 until count) {
      randString.append(symbols.charAt((Math.random * symbols.length).toInt))
    }
    randString.toString
  }

  @throws[Exception]
  def main(args: Array[String]): Unit = {
    val mprint = new Consumer[String] {
      override def accept(t: String) = System.out.print(t + " ")
    }
    val comp = new Comparator[String] {
      override def compare(o1: String, o2: String) = o1.toString.compareToIgnoreCase(o2.toString)
    }
    var list = new ListArray[String]
    for(counter <- 0 to 30 ){
      var s: String=counter.toString();
      list.add(s)
    }
    list.print()
    System.out.println()

    for (counter <- 100 to 110 ) {
      var s: String=counter.toString();
      list.insert(s, 3)
    }
    list.print()
    System.out.println()

    list.sort((a,b)=>a.compare(b)>0);
    list.print()
    System.out.println()

    var s=list.get(3)
    System.out.println(s)
    System.out.println()

    for(counter <- 0 to 11 ){
      list.remove( 3)
    }
    list.print()
    System.out.println()

  }
}
