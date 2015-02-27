package org.higherState.json

import scala.util.Try

case class Path(segments:Segments) extends AnyVal {
  def \(part:String) = Path(segments :+ Left(part))
  def \(part:Int) = Path(segments :+ Right(part))
  def ++(path:Path) = Path(segments ++ path.segments)

  //TODO handle chars \ " etc
  override def toString =
    segments.map(_.merge).mkString("\\")

}

object Path {
  type Mix = Int with String
  val empty = Path(Vector.empty)

  def apply[T >: Mix](s:T*):Path =
    Path(
      s.collect {
        case i:Int => Right(i)
        case s:String => Left(s)
      }.toVector
    )

  //TODO handle chars \ " etc
  def fromString(s:String):Path =
    Path(s.split('\\').map{s =>
      Try(s.toInt).map(Right(_)).getOrElse(Left(s))
    }.toVector)
}