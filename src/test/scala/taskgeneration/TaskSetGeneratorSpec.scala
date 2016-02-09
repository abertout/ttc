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

package test.scala.taskgeneration

import main.scala.taskgeneration.{RandFixedSum, TaskSetGenerator}
import taskgeneration.DistinctPeriods
import test.scala.UnitSpec


class TaskSetGeneratorSpec extends UnitSpec{

  "The genName method" should "give the next char seq of the one given in parameter" in {
    val genName = PrivateMethod[String]('genName)
    val aa = TaskSetGenerator invokePrivate genName(26)
    aa shouldEqual "ba"
    val z = TaskSetGenerator invokePrivate genName(25)
    z shouldEqual "z"
    val a = TaskSetGenerator invokePrivate genName(0)
    a shouldEqual "a"
  }


  "A task set generator" should "generate the task set correctly" in {
    val taskSet = TaskSetGenerator.genTaskSet(20, 3.0, dMin = 0.0d, dMax = 1.0d,
      asynchronous = false, DistinctPeriods, 10,RandFixedSum )
    taskSet.set.size shouldEqual 20
    taskSet.tasksAndSuccs shouldEqual None
    taskSet.set exists(_.o != 0) shouldEqual false
    taskSet.set.map(_.t).distinct.size shouldEqual 10
  }




}
