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

package ttc.partitionning

import ttc.scheduling.{Encoding, SchedTest}
import ttc.taskmodel.{Task, TaskSet}

import scala.annotation.tailrec
import scala.collection.immutable.::
import scala.collection.mutable.ArrayBuffer


trait PartitionningAlgorithm {


  def partitionning(taskSet: TaskSet, schedTest: SchedTest): Option[Vector[Seq[Task]]]

  def methodName: String

  /**
    * compute utilization factor of a set of tasks
    * @return
    */
  def uFactor = (seq: Seq[Task]) => seq.map(task => task.c / task.t.toDouble).sum



}




object PartitionningAlgorithm {


  /**
    * Return the critical path of the task set
    *
    * @param taskSet taskSet
    * @return the sequence of the critical path
    */
  def criticalPath(taskSet: TaskSet): Seq[Task] = {
    //TODO Rendre plus efficace car génère inutilement tous les chemins intermérdiaires  (A -> B-> C) produit (A -> B) et (A ->B -> C)
    val pathsAndU = new scala.collection.mutable.ArrayBuffer[(ArrayBuffer[Task], Float)]() //ArrayBuffer of tuples (Tasks in topologic order forming a path, C_i / D_i)

    val topoSortedSet = Encoding.topologicSort(taskSet)

    for (task <- topoSortedSet) {
      //for each task in the topologic order
      val preds = taskSet.directPreds(task)
      if (preds.nonEmpty) {
        //if the task has predecessors
        for (pred <- preds) {
          val tmpPath = new ArrayBuffer[(ArrayBuffer[Task], Float)]() //The buffer of the new path we will create
          for (path <- pathsAndU) {
            //for each of the existing paths in the graph of dependent tasks (at the start, only form by tasks without preds)
            if (path._1.last == pred) {
              //if the current path contains the current pred at the end of the path
              val newPath = path._1 :+ task //create a new path with path found plus current task
              tmpPath += ((newPath, path._2 + (task.c / task.t.toFloat))) //add the new path with the updated length
            }
          }
          tmpPath.foreach(pathsAndU += _) //add new path to the original buffer of paths
        }
      } else
        pathsAndU += ((ArrayBuffer[Task](task), task.c / task.t.toFloat)) //add the "first" tasks in chains without preds as path of length equal to the execution time
    }
    pathsAndU.maxBy(path => path._2)._1 //The critical path is the longest path (max sum of the execution times)
  }




}

object ButtazoHeuristicH1 extends PartitionningAlgorithm{

  /**
    * Compute density on deadline for end-of-chains tasks and on period for other tasks and sum both
    *
    * @param taskSet taskSet
    * @return particular density
    */
  private def particularDensity(taskSet: TaskSet): Double = {
    val (endTasks,otherTasks)  = taskSet.set partition (task => taskSet.directPreds(task).nonEmpty && taskSet.directSuccs(task).isEmpty)
    endTasks.map(t => t.c / t.d.toDouble).sum + otherTasks.map(t => t.c / t.t.toDouble).sum
  }

  /**
    *
    * @param taskSet taskSet
    * @return lower bound on the number of flows
    */
  def lowerBoundOnFlows(taskSet: TaskSet): Int = {
    val nHeavyTasks = taskSet.set.view.count(task => task.c / task.t.toDouble > heavyTasksUfactor)
    val density: Double = particularDensity(taskSet)
    math.max(nHeavyTasks, math.ceil(density)).toInt  //lower bound on number of flows
  }

  val heavyTasksUfactor = 0.50f

  /**
    * Partitionning heuristic method adapted from Hn heuristic method of
    * Buttazzo, G., Bini, E., & Wu, Y. (2010).
    * Heuristics for Partitioning Parallel Applications on Virtual Multiprocessors.
    * In Workshop on Adaptive Resource Management. SWE.
    * u factor instead of computation times. Computation of mLow adapted to the end deadline constraint
    *
    * @param taskSet taskSet
    * @return Seq of flows
    */


  def heuristicPartitionning(taskSet: TaskSet, initialNbOfFlows: Int, schedTest: SchedTest): Option[Vector[Seq[Task]]] = {
    val (indepSet, depSet) = taskSet.set.partition(task => taskSet.directPreds(task).isEmpty && taskSet.directSuccs(task).isEmpty)
    val indepTaskSet = if(indepSet.nonEmpty) Some(taskSet.restrictedTo(indepSet:_*)) else None
    val depTaskSet = if(depSet.nonEmpty) Some(taskSet.restrictedTo(depSet:_*)) else None

    @tailrec
    def depTasksStep(taskSet: TaskSet, flows: Vector[Seq[Task]], remainingTaskSet: TaskSet, initialNbOfFlows: Int, schedTest: SchedTest): (Vector[Seq[Task]],List[Task]) = {
      if (initialNbOfFlows == flows.length) return (flows, remainingTaskSet.set.toList)
      val cp = PartitionningAlgorithm.criticalPath(remainingTaskSet)
      val updatedFlows = fitElseNew(taskSet, flows, cp, schedTest)
      if(cp.toSet == remainingTaskSet.set.toSet)
        return (updatedFlows, List.empty[Task]) //Avoid to make a recursive call one a forbidden empty task set
      val updatedSet = remainingTaskSet.remove(cp: _*)
      depTasksStep(taskSet, updatedFlows, updatedSet, initialNbOfFlows, schedTest)
    }

    //@tailrec
    def remainingTasksStep(flows: Vector[Seq[Task]], setOfTasks: List[Task], schedTest: SchedTest): Vector[Seq[Task]] = setOfTasks match {
      case task :: rest =>
        val updatedFlows = fitElseNew(taskSet, flows, Seq(task), schedTest)
        remainingTasksStep(updatedFlows, rest, schedTest)
      case _ => flows
    }


    def sortByDecExecTime(tau1: Task, tau2: Task): Boolean = tau1.c / tau1.t.toDouble >= tau2.c / tau2.t.toDouble

    val (firstFlows, remainingDepTasks) = depTaskSet match{
      case Some(depTs) => depTasksStep(depTs, Vector.empty[Seq[Task]], depTs, initialNbOfFlows, schedTest)
      case None => (Vector.empty[Seq[Task]], List.empty)
    }

    val sortedRemainingTasks: List[Task] = indepTaskSet match { //List mandatory for pattern matching
      case Some(indepTs) => (remainingDepTasks ++ indepTs.set).sortWith(sortByDecExecTime)
      case None => remainingDepTasks.sortWith(sortByDecExecTime)
    }

    val finalFlows = remainingTasksStep(firstFlows, sortedRemainingTasks, schedTest)


    if(finalFlows.exists(uFactor(_) > 1.00d))
      return None
    Some(finalFlows)
  }

  def fitElseNew(taskSet: TaskSet, flows: Vector[Seq[Task]], cp: Seq[Task], schedTest: SchedTest): Vector[Seq[Task]] = {

    val flowsByDecUFactor = flows.sortWith(uFactor(_) > uFactor(_))

    for(i <- flowsByDecUFactor.indices) {
      val expFlow: Seq[Task] = flowsByDecUFactor(i) ++ cp
      val flowTaskSet: TaskSet = taskSet.restrictedTo(expFlow:_*) //create a new task set in adding the task to the current flow
      val encflowTaskSet = Encoding.predsEncoding(flowTaskSet)
      if(schedTest(encflowTaskSet)._1){
        return flowsByDecUFactor.updated(i, expFlow)
      }
    }
    flowsByDecUFactor.+:(cp) //If no flow fit, create a new one
  }


  override def partitionning(taskSet: TaskSet, schedTest: SchedTest): Option[Vector[Seq[Task]]] = {
    val mLow = ButtazoHeuristicH1.lowerBoundOnFlows(taskSet)
    heuristicPartitionning(taskSet, mLow, schedTest)
  }
  override def methodName = "H1"

}

object ButtazoHeuristicH2 extends PartitionningAlgorithm {
  override def partitionning(taskSet: TaskSet, schedTest: SchedTest): Option[Vector[Seq[Task]]] =
    ButtazoHeuristicH1.heuristicPartitionning(taskSet, 1, schedTest)

  override def methodName = "H2"


}

object GlobalHeuristic extends PartitionningAlgorithm {

  /**
    * Minimize the number of flows in assigning decreasing execution time critical path to flows successively (Best Fit). Critical path can be made of a single task
    * @param taskSet taskSet
    * @param schedTest schedulability test
    * @return vector of flows
    */
  override def partitionning(taskSet: TaskSet, schedTest: SchedTest): Option[Vector[Seq[Task]]] = {

    @tailrec
    def minNbFlows(taskSet: TaskSet, flows: Vector[Seq[Task]], remainingTaskSet: TaskSet, schedTest: SchedTest): Vector[Seq[Task]] = {
      if(remainingTaskSet.set.isEmpty) return flows
      val cp = PartitionningAlgorithm.criticalPath(remainingTaskSet)
      val updatedFlows = ButtazoHeuristicH1.fitElseNew(taskSet, flows, cp, schedTest)
      if(cp.toSet == remainingTaskSet.set.toSet)
        return updatedFlows //Avoid to make a recursive call one a forbidden empty task set
      minNbFlows(taskSet, updatedFlows, remainingTaskSet.remove(cp:_*), schedTest)
    }

    val flows = minNbFlows(taskSet, Vector.empty[Seq[Task]], taskSet, schedTest)
    if(flows.exists(uFactor(_) > 1.00d))
      throw new Exception("Partitionning infeasible")
    Some(flows)

  }

  override def methodName = "GlobalHeuristic"


}

