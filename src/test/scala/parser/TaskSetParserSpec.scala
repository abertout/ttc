/*
 * Copyright (c) CNRS - CRIStAL Laboratory - Emeraude Team
 * contributor: Antoine Bertout (2012-2015)
 * Copyright (c) Antoine Bertout (2015-2016)
 *
 * bertout.antoine@gmail.com
 *
 * This software is a computer program whose purpose is to minimize the
 * number of tasks of a real-time system by clustering.
 *
 * This software is governed by the CeCILL license under French law and
 * abiding by the rules of distribution of free software.  You can  use,
 * modify and/ or redistribute the software under the terms of the CeCILL
 * license as circulated by CEA, CNRS and INRIA at the following URL
 * "http://www.cecill.info".
 *
 * As a counterpart to the access to the source code and  rights to copy,
 * modify and redistribute granted by the license, users are provided only
 * with a limited warranty  and the software's author,  the holder of the
 * economic rights,  and the successive licensors  have only  limited
 * liability.
 *
 * In this respect, the user's attention is drawn to the risks associated
 * with loading,  using,  modifying and/or developing or reproducing the
 * software by the user in light of its specific status of free software,
 * that may mean  that it is complicated to manipulate,  and  that  also
 * therefore means  that it is reserved for developers  and  experienced
 * professionals having in-depth computer knowledge. Users are therefore
 * encouraged to load and test the software's suitability as regards their
 * requirements in conditions enabling the security of their systems and/or
 * data to be ensured and,  more generally, to use and operate it in the
 * same conditions as regards security.
 *
 * The fact that you are presently reading this means that you have had
 * knowledge of the CeCILL license and that you accept its terms.
 *
 */

package test.scala.parser

import java.io.FileNotFoundException

import main.scala.parser.{PredDecl, TaskSetParser}
import main.scala.taskmodel.{Task, TaskSet}
import test.scala.UnitSpec


class TaskSetParserSpec extends UnitSpec{

  def fixture =
    new {

      val tauA = Task("a",485, 8613, 120000, 0)
      val tauB = Task("b",149, 15105, 120000)
      val tauC = Task("c",272, 16728, 120000, 5)
      val tauD = Task("d", 199, 18735, 120000, 0)
      val tauE = Task("e", 141, 22190, 120000, 0)
      val tauF = Task("f",141, 22190, 120000, 0)
      val taskSet = TaskSet(Seq(tauA, tauB, tauC, tauD, tauF, tauE), Some(Map(tauA -> Set(tauB, tauC, tauD), tauF -> Set(tauE))))
    }

  "A taskSet parser" should "parse task ident" in {

    val id1 = "tau1"
    val id2 = "1_b"
    val id3 = "tau 1"
    val id4 = "tau!"

    TaskSetParser.parse(TaskSetParser.taskId, id1).get shouldEqual id1
    TaskSetParser.parse(TaskSetParser.taskId, id2).get shouldEqual id2
    TaskSetParser.parse(TaskSetParser.taskId, id3).get should not equal id3
    TaskSetParser.parse(TaskSetParser.taskId, id4).get should not equal id4
    an [RuntimeException] should be thrownBy TaskSetParser.parse(TaskSetParser.taskId, "").get
  }

  it should "parse precedence constraint declaration" in {
    val f = fixture
    val l1 = "tau1"
    val l2 ="a, b"
    val l3 = ""
    val l4 = "a -> b"
    val l5 = "a -> b, c"
    val l6 = "a -> (b, c)"
    val l7 = "a -> (5taux)"

    List("tau1") shouldEqual TaskSetParser.parse(TaskSetParser.listTaskId, l1).getOrElse("")
    List("a", "b") shouldEqual TaskSetParser.parse(TaskSetParser.listTaskId, l2).getOrElse("")
    an [RuntimeException] should be thrownBy TaskSetParser.parse(TaskSetParser.listTaskId, l3).get
    PredDecl((f.tauA.name, List(f.tauB.name))) shouldEqual TaskSetParser.parse(TaskSetParser.precedDecl, l4).getOrElse(("", List.empty[String]))
    PredDecl((f.tauA.name, List(f.tauB.name, f.tauC.name))) should not equal TaskSetParser.parse(TaskSetParser.precedDecl, l5).getOrElse(("", List.empty[String]))
    PredDecl((f.tauA.name, List(f.tauB.name, f.tauC.name))) shouldEqual TaskSetParser.parse(TaskSetParser.precedDecl, l6).getOrElse(("", List.empty[String]))
    PredDecl((f.tauA.name, List("5taux"))) shouldEqual TaskSetParser.parse(TaskSetParser.precedDecl, l7).getOrElse(("", List.empty[String]))
  }

  it should "parse the tasks" in {
    val t1 = "a(485, 8613, 120000, 5)"
    val t2 = "b(44, 33, 111)"
    val tau1 = Task("a", 485, 8613, 120000, 5)
    val tau2 = Task("b", 44, 33, 111, 0, None)

    val eqC = tau1.equalContent(TaskSetParser.parse(TaskSetParser.taskDecl, t1).get.t)
    eqC shouldEqual  true
    val eqC2 = tau2.equalContent(TaskSetParser.parse(TaskSetParser.taskDecl, t2).get.t)
    eqC2 shouldEqual  true
  }


  it should "parse whole task set from a string" in {
    val input = "a(485, 8613, 120000, 0) \n b(149, 15105, 120000) c(272, 16728, 120000, 5),d(199, 18735, 120000, 0)," +
      "e(141, 22190, 120000, 0)," +
      "a -> (b,c), f(141, 22190, 120000, 0); a -> d" +
      "\n" +
      " f -> e"

    val parsedTaskSet = TaskSetParser.taskSetFromString(input)
    val expTaskSet = fixture.taskSet
    parsedTaskSet shouldEqual expTaskSet
  }

  it should "parse whole task set from a file" in {

    val expTaskSet = fixture.taskSet
    val validPath = getClass.getResource("/taskSet").getFile
    val taskSet = TaskSetParser.taskSetFromFile(validPath)
    taskSet shouldEqual expTaskSet
    val wrongPath = "../ressource/tasskSet"
    an [FileNotFoundException] should be thrownBy TaskSetParser.taskSetFromFile(wrongPath)
  }

  "Method taskByName" should "return the task if it exists" in {
    val f = fixture
    val taskByName = PrivateMethod[Option[Task]]('taskByName)
    val taskSet = f.taskSet.set
    val task: Option[Task] =  TaskSetParser invokePrivate taskByName(taskSet, "a")
    task shouldEqual Some(f.tauA)
    val task2: Option[Task] =  TaskSetParser invokePrivate taskByName(taskSet, "z")
    task2 shouldEqual None
  }




}

