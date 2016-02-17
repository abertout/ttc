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

package test.scala.clustering

import main.scala.clustering.{ClusterDeadline, ClusteringCompanion}
import main.scala.taskmodel.{Task, TaskSet}
import test.scala.UnitSpec

class ClusteringCompanionSpec extends UnitSpec{

  def fixture =
    new {
      val tauA = Task("a", 1, 5, 10, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 2, 5, 40, 0)
      val tauE = Task("e", 2, 4, 50, 0)



      val dep = Map(
        tauA -> Set(tauB),
        tauB -> Set(tauC, tauD),
        tauD -> Set(tauE)
      )

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauA, tauD, tauE), Some(dep))
    }
  def fixturePred =
    new {
      val tauA = Task("a", 1, 5, 10, 0)
      val tauBC = Task("b_c", 4, 7, 50, 0)
      val tauD = Task("d", 2, 5, 40, 0)
      val tauE = Task("e", 2, 5, 50, 0)


      val dep = Map(
        tauA -> Set(tauBC),
        tauBC -> Set(tauD),
        tauD -> Set(tauE)
      )

      val taskSet = TaskSet(set = Seq(tauBC, tauA, tauD, tauE), Some(dep))
    }

  def fixtureSucc =
    new {
      val tauA = Task("a", 1, 5, 10, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauED = Task("e_d", 4, 4, 50, 0)


      val dep = Map(
        tauA -> Set(tauB),
        tauB -> Set(tauC, tauED)
      )

      val taskSet = TaskSet(set = Seq(tauA, tauB, tauC, tauED), Some(dep))
    }

  def fixtureMax =
    new {
      val tauA = Task("a", 1, 5, 10, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauED = Task("d_e", 4, 5, 50, 0)


      val dep = Map(
        tauA -> Set(tauB),
        tauB -> Set(tauC, tauED)
      )

      val taskSet = TaskSet(set = Seq(tauA, tauB, tauC, tauED), Some(dep))
    }

  def fixtureMin =
    new {
      val tauA = Task("a", 1, 5, 10, 0)
      val tauCB = Task("c_b", 4, 5, 50, 0)
      val tauD = Task("d", 2, 5, 40, 0)
      val tauE = Task("e", 2, 5, 50, 0)

      val dep = Map(
        tauA -> Set(tauCB),
        tauCB -> Set(tauD),
        tauD -> Set(tauE)
      )

      val taskSet = TaskSet(set = Seq(tauA, tauCB, tauE, tauD), Some(dep))
    }

  "Two tasks" should "not be grouped if they have different periods" in {
    val f = fixture
    val ts = f.taskSet
    val companion = new ClusteringCompanion(ts)
    companion.regroupable(f.tauB, f.tauA) shouldEqual false
    companion.regroupable(f.tauB, f.tauC) shouldEqual true
  }

  it should "not be grouped if there is a path different than a direct precedence constraint between them" in {
    val f = fixture
    val ts = f.taskSet
    val companion = new ClusteringCompanion(ts)
    companion.regroupable(f.tauB, f.tauE) shouldEqual false
  }

  it should "be able to be grouped if they have equal periods and a correct link in the graph" in {
    val f = fixture
    val ts = f.taskSet
    val companion = new ClusteringCompanion(ts)
    companion.regroupable(f.tauB, f.tauC) shouldEqual true
  }

  it should "be well fused" in {
    val f = fixture
    val ts = f.taskSet
    val companion = new ClusteringCompanion(ts)
    val newTaskSetPred: TaskSet = companion.fusion(f.tauB, f.tauC, ClusterDeadline.DlPred)
    newTaskSetPred shouldEqual fixturePred.taskSet
    val newTaskSetSucc: TaskSet = companion.fusion(f.tauD, f.tauE, ClusterDeadline.DlSucc)
    newTaskSetSucc shouldEqual fixtureSucc.taskSet
    val newTaskSetMax: TaskSet = companion.fusion(f.tauD, f.tauE, ClusterDeadline.DlMax)
    newTaskSetMax shouldEqual fixtureMax.taskSet
    val newTaskSetMin: TaskSet = companion.fusion(f.tauB, f.tauC, ClusterDeadline.DlMin)
    newTaskSetMin shouldEqual fixtureMin.taskSet

  }

  "The transitive closure" should "be correct" in {

    val matrix = Vector(
      Vector(0, 1, 1, 0, 0),
      Vector(0, 0, 0, 1, 0),
      Vector(0, 0, 0, 1, 0),
      Vector(0, 0, 0, 0, 1),
      Vector(0, 0, 0, 0, 0)
    )

    val tcMatrix = Vector(
      Vector(0, 1, 1, 2, 2),
      Vector(0, 0, 0, 1, 2),
      Vector(0, 0, 0, 1, 2),
      Vector(0, 0, 0, 0, 1),
      Vector(0, 0, 0, 0, 0)
    )

    val tcMethod = PrivateMethod[Vector[Vector[Int]]]('transitiveClosure)
    val comp = new ClusteringCompanion(fixture.taskSet)
    val tc: Vector[Vector[Int]] =  comp invokePrivate tcMethod(matrix, ClusteringCompanion.ADDED_EDGE)

    tc shouldEqual tcMatrix
  }


  "The transitive reduction" should "be correct" in {

    val matrix = Vector(
      Vector(0, 1, 1, 1, 1),
      Vector(0, 0, 0, 1, 1),
      Vector(0, 0, 0, 1, 1),
      Vector(0, 0, 0, 0, 1),
      Vector(0, 0, 0, 0, 0)
    )

    val rcMatrix = Vector(
      Vector(0, 1, 1, 0, 0),
      Vector(0, 0, 0, 1, 0),
      Vector(0, 0, 0, 1, 0),
      Vector(0, 0, 0, 0, 1),
      Vector(0, 0, 0, 0, 0)
    )

    val trMethod = PrivateMethod[Vector[Vector[Int]]]('transitiveReduction)
    val comp = new ClusteringCompanion(fixture.taskSet)
    val tr: Vector[Vector[Int]] =  comp invokePrivate trMethod(matrix)

    tr shouldEqual rcMatrix
  }
}