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

package taskgeneration

import main.scala.taskmodel.{Task, TaskSet}
import test.scala.UnitSpec


class PrecedenceGenerationSpec extends UnitSpec{

  val fixture =
    new {
      val tauA = Task("a", 1, 5, 50, 0)
      val tauB = Task("b", 2, 7, 50, 5)
      val tauC = Task("c", 2, 5, 50, 0)
      val tauD = Task("d", 1, 10, 50, 0)
      val tauE = Task("e", 3, 12, 50, 0)
      val tauF = Task("f", 3, 9, 30, 0)
      val tauG = Task("g", 2, 16, 30, 5)
      val tauH = Task("h", 1, 13, 30, 0)
      val tauI = Task("i", 4, 20, 40, 0)

      val taskSet = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH))


      val tauJ = Task("j", 1, 5, 50, 0)
      val tauK = Task("k", 3, 34, 50, 0)
      val tauL = Task("l", 4, 30, 30, 0)

      val taskSet2 = TaskSet(set = Seq(tauB, tauC, tauI, tauA, tauD, tauF, tauE, tauG, tauH, tauJ, tauK, tauL))


    }


  "A simple precedence constraints generation method Erdös Rényi "  should
    "generate the precedence constraints randomly in order of increasing deadline" in {

    val f = fixture
    val withPredTaskSet = SimpleErdösRényi(f.taskSet, 0.5)
    f.taskSet.set.sortBy(_.d) shouldEqual withPredTaskSet.set

  }

  it should "generate precedence constraints only between tasks of equal periods" in {
    val f = fixture
    val withPredTaskSet = SimpleErdösRényi(f.taskSet, 0.5)
    val diffPeriods  = for(m <- withPredTaskSet.tasksAndSuccs.getOrElse(Map.empty); succ <- m._2 )
      yield m._1.t != succ.t
    diffPeriods.exists(_ == true) shouldEqual false

  }

  it should "detect error probability parameter" in {
    val f = fixture
    a [IllegalArgumentException] should be thrownBy SimpleErdösRényi(f.taskSet, -0.5)
    a [IllegalArgumentException] should be thrownBy SimpleErdösRényi(f.taskSet, 1.1)
  }

  "A by-level generation method" should "" in {

    val f = fixture
    val taskSet = f.taskSet2
    val withPredTaskSet = ByLevel(taskSet, 0.65, 0.50)
     ???

  }




}
