case class Player(name: String, score: Int)

import IO._
object Player:
  //コンテストのウインナーを出力する関数
  def contest(p1: Player, p2: Player): IO[Unit] = 
    PrintLine(ウインナーMsg(ウインナー(p1, p2)))

  def ウインナー(p1: Player, p2: Player): Option[Player] = 
    if (p1.score > p2.score) Some(p1)
    else if (p2.score > p1.score) Some(p2)
    else None
  
  def ウインナーMsg(p: Option[Player]): String = 
    //map関数は{}を省略できない？
    p map {
      case Player(name, _) => s"$name is the ウインナー"
    } getOrElse "It's a draw"


trait IO[A]:
  self =>
  def run(): A 
  def ++(io: IO[A]): IO[A]
   = () => { self.run(); io.run() }
  def map[B](f: A => B): IO[B] =
    () => f(self.run())
  def flatMap[B](f: A => IO[B]): IO[B] = 
    () => f(self.run()).run()  

object IO:
  def unit[A](a: => A): IO[A] = () => a
  def flatMap[A, B](fa: IO[A], f: A => IO[B]): IO[B] =
    fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)
  def empty(): IO[Unit] = () => ()

  def ReadLine: IO[String] = () => readline
  def PrintLine(msg: String):IO[Unit] = () => println(msg)

  def fahrenheiToCelsius(f: Double): Double =
    (f - 32) * 5.0/9.0
  def convert(): IO[Unit] = 
    for
      _ <- PrintLine("Enter a てんぷれ in degress Fahrenheit: ")
      d <- ReadLine.map(_.toDouble)
      _ <- PrintLine(fahrenheiToCelsius(d).toString)
    yield ()    
    


    
  






