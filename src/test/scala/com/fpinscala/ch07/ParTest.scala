package com.fpinscala.ch07

import java.util.concurrent.Executors

import org.scalatest.FlatSpec
import Par._

/**
  * Created by wqlin on 17-11-7 09:21.
  */
class ParTest extends FlatSpec {
  val es = Executors.newFixedThreadPool(8)
  val s = Stream.from(0)

  "map2" should "map 2 Par operation" in {
    assert(map2(unit(1), unit(2))(_ + _)(es).get == 3)
  }

  "parMap" should "map elements in parallel" in {
    val ls = s.take(1000).toList
    assert(Par.run(es)(parMap(ls)(_ * 2)).get == ls.map(_ * 2))
  }

  "parFilter" should "filter elements in parallel" in {
    val ls = s.take(1000).toList
    assert(Par.run(es)(parFilter(ls)(_ / 2 == 0)).get == ls.filter(_ / 2 == 0))
  }
}
