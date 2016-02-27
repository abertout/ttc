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

package ttc.taskgeneration

import ttc.taskmodel.{Task, TaskSet}
import ttc.utils.Numbers
import scala.util.Random


trait PrecedenceGeneration

object SimpleErdösRényi extends PrecedenceGeneration {

  def apply(taskSet: TaskSet, prob: Double): TaskSet = {
    require(prob >= 0 && prob <= 1, "probability must be in [0,1] interval")

    val sortedTaskSet = TaskSet(set = taskSet.set.sortBy(_.d), taskSet.tasksAndSuccs)
    val listDep: IndexedSeq[(Task, Task)] = for (i <- 0 until sortedTaskSet.size; j <- i + 1 until sortedTaskSet.size
                                                 if sortedTaskSet.set(i).t == sortedTaskSet.set(j).t && Random.nextDouble <= prob)
      yield (sortedTaskSet.set(i), sortedTaskSet.set(j))

    val depMap = listDep.groupBy(kv => kv._1).map{
      case (k,v) => (k,v.flatMap{
        case(k2,v2) => List(k2,v2)
      }.toSet - k)
    }

    TaskSet(sortedTaskSet.set, Some(depMap))
  }
}

object ByLevel extends PrecedenceGeneration {

  def apply(taskSet: TaskSet, prob: Double, levels: Double): TaskSet = {
    require(prob >= 0 && prob <= 1, "probability must be in [0,1] interval")
    require(levels >=0 && levels <= 1,"percentage of levels should be between 0 and 1")

    val tasksByPeriod = taskSet.set.groupBy(_.t)

    def allComb[A](z:Seq[Seq[A]]): Seq[(A,A)] = if(z.size > 1)for(xs <- z.head; ys <- z(1)) yield(xs,ys) else Seq.empty[(A,A)]

    val depMapByPeriod =
      for{
        taskOfOnePeriod <- tasksByPeriod
        tasks = taskOfOnePeriod._2.sortBy(_.d)
        maxHeight = (levels * tasks.size).toInt
        nbTasksByLevel = Numbers.partitionsWithoutZero(maxHeight, tasks.size).filter(_ != 0)
        nLevel = nbTasksByLevel.size
        tasksByLevel = nbTasksByLevel.foldLeft((tasks,Seq.empty[Seq[Task]])) {
          case ((oldTasks, newTasks), occ) => (oldTasks.drop(occ),newTasks.:+(oldTasks.take(occ)))
        }._2
        communicatingLevels = tasksByLevel.sliding(2).toList
        toTuple = communicatingLevels map(x => allComb(x))
        dependentTasks: Seq[(Task,Task)] = (for (t <- toTuple if scala.util.Random.nextDouble <= prob)  yield t).flatten
        dep: Map[Task, Set[Task]] = dependentTasks.groupBy(_._1) map {case (k,v) => (k,v.map(_._2).toSet)} /* */
      }yield dep

    val depMap = depMapByPeriod.reduce(_.++(_))

    TaskSet(set = taskSet.set, Some(depMap))
  }







}


