import java.io._
import java.nio.charset.Charset
import scala.io.Source

object PgnToCsv extends App {
  val pc = new PrintWriter(new BufferedWriter(new OutputStreamWriter(
    new FileOutputStream("pgn.csv"))))

  val csvHeader = "White,Black,Result,Date,ECO,Site,Round,FEN,Event,WhiteElo,BlackElo,Opening,Variation,moves\n"
                  //0     1       2     3    4   5   6     7    8      9       10       11         12     13

  pc.write(csvHeader)
  val filename = "millionbase-2.22.pgn"
  var moveStart = false
  var moves = new StringBuilder
  var sbn = new StringBuilder
  final val rowa : Array[String] = Array.fill(13)("""""""")

  for (line <- Source.fromFile(filename)(Charset.forName("ISO-8859-1")).getLines) {
    line match {
      case x if moveStart && !x.isEmpty =>
        sbn.append(x + " ")

      case x if moveStart && x.isEmpty =>
        moveStart = false
        if(sbn.isEmpty)
          pc.write(rowa.mkString(",") + "\n")
        else
          pc.write(rowa.mkString(",") + ",\"" + sbn.dropRight(1) + "\"\n")
        sbn = new StringBuilder
        rowa.indices.foreach(m => rowa(m) = """""""")

      case x if x.startsWith("[White ") =>
        rowa(0) = x.slice(7, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Black ") =>
        rowa(1) = x.slice(7, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Result ") =>
        rowa(2) = x.slice(8, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Date ") =>
        rowa(3) = x.slice(6, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[ECO ") =>
        rowa(4) = x.slice(5, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Site ") =>
        rowa(5) = x.slice(6, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Round ") =>
        rowa(6) = x.slice(7, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[FEN ") =>
        rowa(7) = x.slice(5, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Event ") =>
        rowa(8) = x.slice(7, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[WhiteElo ") =>
        rowa(9) = x.slice(10, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[BlackElo ") =>
        rowa(10) = x.slice(10, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Opening ") =>
        rowa(11) = x.slice(9, x.length - 1).replace("""\"""","")

      case x if x.startsWith("[Variation ") =>
        rowa(12) = x.slice(11, x.length - 1).replace("""\"""","")

      case x if x.matches("[0-9].+") =>
        sbn.append(x + " ")
        moveStart = true

      case _ =>
    }
  }


  pc.close()
}