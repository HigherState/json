package org.higherState.json

case class Path(segments:Segments) extends AnyVal {
  def \(part:String) = Path(segments :+ Left(part))
  def \(part:Int) = Path(segments :+ Right(part))
  def ++(path:Path) = Path(segments ++ path.segments)

  override def toString = segments.map(_.merge).mkString("\\")

}

object Path {
  val empty = Path(Vector.empty)

  def apply(s:String*):Path =
    Path(s.toVector.map(Left(_)))
}