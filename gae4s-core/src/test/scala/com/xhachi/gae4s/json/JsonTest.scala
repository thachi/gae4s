package com.xhachi.gae4s.json

import java.util.Date

import org.scalatest.FunSuite

import scala.util.Random

class JsonTest extends FunSuite {

  test("JSONで並列にDateを大量に処理してみる") {

    Json.addEnum(SampleEnum)
    val millis = System.currentTimeMillis()
    val r = new Random(millis)
    val samples = (0L until 2L).map(_ => r.nextLong() % millis).map(Math.abs).map(i => Sample(i.toString, new Date(i))).par

    val strings = samples.map(Json.stringify[Sample])
    val parses = strings.map(Json.parseAs[Sample])
    val asserts = parses.map { s => s.name.toLong == s.date.getTime }
    parses.filter(s => s.name.toLong != s.date.getTime).foreach(s => println(s"${s.name.toLong} == ${s.date.getTime}"))
    val contains = asserts.count(_ == false)
    assert(contains == 0)
  }


}

case class Sample(name: String, date: Date)

object SampleEnum extends Enumeration {
  val A = Value
}
