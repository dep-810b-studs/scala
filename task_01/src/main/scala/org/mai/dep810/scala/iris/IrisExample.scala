package org.mai.dep810.scala.iris

import org.mai.dep810.scala.iris.PetalSize.PetalSize

import scala.io.Source
import scala.util.Try

object IrisExample
{
  def main(args: Array[String]): Unit =
  {
    var flowers: List[Iris] = loadFromFile("iris.data")
    //println(flowers)

    //get average sepal width
    val avgSepalLength = flowers.map(iris => iris.sepalWidth).sum/flowers.length
    println("Average sepal width: ")
    println(avgSepalLength);

    //get average petal square - petal width multiplied on petal length
    val avgPetalLength = flowers.map(iris => iris.petalWidth*iris.petalLength).sum/flowers.length
    println("Average petal square: ")
    println(avgPetalLength)



    //get average petal square for flowers with sepal width > 4
    val avgPetalSquare = flowers
        .filter(iris => iris.sepalWidth >4)
        .map(iris => iris.petalLength*iris.sepalWidth)
        .sum / flowers.count(iris => iris.sepalWidth >4) //fold left

    val query = flowers
        .filter(iris => iris.sepalWidth>4)
        .foldLeft((Tuple2[Double,Double])(0,0)){
          (acc,flower)=>(acc._1+flower.petalLength*flower.sepalWidth, acc._2+1)
        }

    println("Filter 4 for square")
    println(query._1/query._2)



    //get flowers grouped by Petal size (PetalSize.Small, etc.) with function getPetalSize
    val groupsByPetalSize = flowers.groupBy(iris=>getPetalSize(iris))
    println("Group by: ")
    println(groupsByPetalSize)

    //get max sepal width for flowers grouped by species
    val maxSepalWidthForGroupsBySpecies = flowers
      .groupBy(iris=> iris.species)
      .map{ case (key, v) => (key, v.maxBy(_.sepalWidth))}
      //.map((elem)=>(elem._1,elem._2.map(elem=>elem.sepalWidth).max))
    println("max sepal for group species: ")
    println(maxSepalWidthForGroupsBySpecies)
  }

  def loadFromFile(path: String): List[Iris] =
  {
    Source
      .fromFile(path)
      .getLines
      .map(line => line.toIris)
      .filter
      {
        case Some(iris) => true
        case None => false
      }
      .map
      {
        case Some(iris) => iris
      }
      .toList
  }

  implicit class StringToIris(str: String)
  {
    def toIris: Option[Iris] = str.split(",") match
    {
      case Array(a,b,c,d,e) if isDouble(a) && isDouble(b) && isDouble(c) && isDouble(d) =>
        Some(
          Iris(
            a.toDouble,
            b.toDouble,
            c.toDouble,
            d.toDouble,
            e))
      case others => None
    }

    def isDouble(str: String): Boolean = Try(str.toDouble).isSuccess
  }

  def getPetalSize(iris: Iris): PetalSize =
  {
    val petalSquare = iris.petalLength * iris.petalWidth
    if(petalSquare < 2.0)
      PetalSize.Small
    if(petalSquare < 5.0)
      PetalSize.Medium
    PetalSize.Large
  }

}

object PetalSize extends Enumeration {
  type PetalSize = Value
  val Large, Medium, Small = Value
}

case class Iris(sepalLength: Double, sepalWidth: Double, petalLength: Double, petalWidth: Double, species: String)
