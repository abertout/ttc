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

package test.scala.export

import java.io.FileNotFoundException

import main.scala.export.Export
import main.scala.taskgeneration.{RandFixedSum, TaskSetGenerator}
import main.scala.taskmodel.{Task, TaskSet}
import taskgeneration.{ByLevel, LimitedHPDistinctPeriods}
import test.scala.UnitSpec


class ExportSpec extends UnitSpec{

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
    val tauIJ = Task("i_j", 4, 20, 40, 0)

    val dep = Map(
      tauA -> Set(tauB, tauE),
      tauG -> Set(tauE),
      tauD -> Set(tauF, tauIJ)
    )

    val taskSet = TaskSet(set = Seq(tauB, tauC, tauIJ, tauA, tauD, tauF, tauE, tauG, tauH), Some(dep))

  }

  "A export to Cheddar" should "generate a correct xml file" in {
    val f = fixture
    Export.toCheddarXml(f.taskSet,"EDF", "/tmp/cheddar.xml")
    a [Exception] should be thrownBy Export.toCheddarXml(f.taskSet,"LLF", "/tmp/cheddar.xml")
    a [FileNotFoundException] should be thrownBy Export.toCheddarXml(f.taskSet,"EDF", "/tmpss/cheddar.xml")
  }

  "A export to SimSo" should "generate a correct xml file" in {
    val f = fixture
    Export.toSimSoXml(f.taskSet,"EDF", "/tmp/simso.xml", None)
    a [Exception] should be thrownBy Export.toSimSoXml(f.taskSet,"DM", "/tmp/simso.xml", None)
    a [FileNotFoundException] should be thrownBy Export.toSimSoXml(f.taskSet,"EDF", "/tmpsd/simso.xml", None)

  }

  "A export to dot" should "generate a correct dot representation of a graph in a string" in {
    val f = fixture
    val s = Export.toDot(f.taskSet, "/tmp/taskSet.dot")
    ???
  }

  "The display graph method" should "correctly display a dot description" in {
    val f = fixture
    Export.displayGraph(f.taskSet)
    ???
  }


  "the genScalaTaskSetDecl method" should "generate a string with all the scala declaration to form the task set" in {
    val taskSetMulti = TaskSetGenerator.genTaskSet(50,3.4d,0.0,1.0,asynchronous = true,LimitedHPDistinctPeriods,10,RandFixedSum)
    val withPredTaskSet = ByLevel(taskSetMulti, 0.65, 0.50)
    val decl = Export.genScalaTaskSetDecl(withPredTaskSet, "taskSet")
    ???
  }

}
