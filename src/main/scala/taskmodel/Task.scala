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

package ttc.taskmodel

/**
 * A task with real-time constraints
 * @param id task id
 * @param c worst-case execution time (WCET)
 * @param d relative deadline
 * @param t period of activation
 * @param o offset
 * @param r worst-case response time
 */
case class Task(id: String, c: Int, d:Int, t:Int, o:Int = 0, var r: Option[Int] = None) {

  override def toString: String = s"$id($c, $d, $t, $o, ${r.getOrElse("-1")})"

  /* Task equality is based on its id */
  override def hashCode = id.hashCode

  override def equals(other: Any) = other match {
    case that: Task => this.id == that.id
    case _ => false
  }

  def equalContent(other: Any) = other match {
    case that: Task => this.id == that.id && this.c == that.c && this.d == that.d && this.t == that.t && this.o == that.o
    case _ => false
  }

  /**
   * Returns the next absolute deadline from a time (excluded)
   * @param time specified date (time)
   * @return
   */
  def getNextAbsoluteDeadline(time: Int): Int = {
    if(d <= t) return math.floor(time / t.toDouble).toInt * t + d + o
    else
      if(time <= d) return d
      (math.floor(time / t.toDouble).toInt + 1) * t + (d - t + o)
  }

}
