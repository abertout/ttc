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


import clustering.MultiClustering
import main.scala.clustering.{MinDensity, MonoClustering}
import main.scala.parser.TaskSetParser
import main.scala.partitionning.GlobalHeuristic
import main.scala.scheduling.{DMresponseTimeAnalysis, EDFqPA}
import main.scala.taskgeneration.{RandFixedSum, TaskSetGenerator}
import main.scala.taskmodel.TaskSet
import taskgeneration.{ByLevel, LimitedHPDistinctPeriods}

object Main extends App {


  val path = getClass.getResource("/taskSet").getFile
  val taskSet: TaskSet = TaskSetParser.taskSetFromFile(path)
  val minTaskSet = MonoClustering.cluster(taskSet, DMresponseTimeAnalysis, MinDensity, Some(DMresponseTimeAnalysis))

  val taskSetMulti = TaskSetGenerator.genTaskSet(200,2.4d,0.0,1.0,asynchronous = false,LimitedHPDistinctPeriods,10,RandFixedSum)
  val taskSetMultiDep = ByLevel(taskSetMulti, 0.65, 0.50)

  val minFlows = MultiClustering.cluster(taskSetMultiDep, GlobalHeuristic, EDFqPA, MinDensity, EDFqPA, None)


}
