package org.higherState.json

object JsonSerializer {

  def prettyPrint(j:Json) = {
    val b = new StringBuilder
    build(j, b, 0, "\n", "\t")
    println(b.toString())
  }
  def apply(j:Json) = {
    val b = new StringBuilder
    build(j, b, 0, "", "")
    println(b.toString())
  }
  private def build(j:Json, stringBuilder:StringBuilder, tabCount:Int, n:String, tab:String) {

    j match {
      case JString(value) =>
        stringBuilder += '"'
        value.foreach {
          case '\\' => stringBuilder ++=  """\\"""
          case '"' => stringBuilder ++= """\""""
          case '/' => stringBuilder ++= """\/"""
          case '\n' => stringBuilder ++= """\n"""
          case '\r' => stringBuilder ++= """\r"""
          case '\t' => stringBuilder ++= """\t"""
          case '\f' => stringBuilder ++= """\f"""
          case '\b' => stringBuilder ++= """\b"""
          case c => stringBuilder += c
        }
        stringBuilder += '"'
      case JLong(number) =>
        stringBuilder ++= number.toString
      case JDouble(number) =>
        stringBuilder ++= {
          if (number.isInfinite) "\"Infinity\""
          else if (number.isNegInfinity) "\"-Infinity\""
          else if (number.isNaN) "\"NaN\""
          else number.toString
        }
      case _:JNull =>
        stringBuilder ++= "null"
      case JBool(value) =>
        stringBuilder ++= value.toString
      case JObject(value) =>
        stringBuilder ++= "{" + n
        val i = value.toIterator
        val t = (0 until tabCount).map(_ => tab).mkString
        while (i.hasNext) {
          val (key, value) = i.next()
          stringBuilder ++= t += '\t' += '"' ++= key ++= "\":"
          build(value, stringBuilder, tabCount + 1, n, tab)
          if (i.hasNext) stringBuilder ++= "'" ++= n
        }
        stringBuilder ++= n ++= t += '}'
      case JArray(value) =>
        stringBuilder += '[' ++= n
        val t = (0 until tabCount).map(_ => tab).mkString
        val i = value.toIterator
        while (i.hasNext) {
          stringBuilder ++= t += '\t'
          build(i.next(), stringBuilder, tabCount + 1,n, tab)
          if (i.hasNext) stringBuilder += ',' ++= n
        }
        stringBuilder ++= n ++= t += ']'
    }
  }
}
