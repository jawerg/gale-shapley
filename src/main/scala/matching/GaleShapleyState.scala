package matching

import TypeDef._

case class GaleShapleyState( prefM: Pref, prefW: Pref, alloc: Alloc ) {

  // collect all Boys that are allow to make an offer at the current State of the game.
  val hunters: List[ Boy ] = {
    prefM
      .toList.filter( x => x._2.nonEmpty ).toMap
      .keys.toSet.diff( alloc.values.toSet ).toList
  }

  // Translate Boys' Preferences into a Queue for each Girl.
  val queue: Queue = {
    prefM.toList
      .filter( x => hunters.contains( x._1 ) ) // require: Boy is on the hunt.
      .map( x => (x._2.head, x._1) ).groupBy( _._1 ).toList // collect tuples at each girl.
      .map( x => (x._1, x._2.map( y => y._2 )) ) // extract boys from tuple to generate queue
      .toMap
  }

  val choices: Queue = {
    queue.toList
      .map( x => (x._1, x._2 ++ alloc.get( x._1 )) ) // Add current allocation to the choice set
      .map( x => x._1 -> x._2.toSet.intersect( prefW( x._1 ).toSet ).toList ) // restrict queue valid proponents
      .toMap
  }

  val matches: Alloc = {
    choices.toList
      .filter( x => x._2.nonEmpty )
      .map( x => x._1 -> prefW( x._1 ).filter( y => x._2.contains( y ) ).head ) // choose favorite proponent
      .filter( x => !alloc.toList.contains( x ) ) // Filter current allocations.
      .toMap
  }

  val prefMp: Pref = {
    prefM ++ // override preferences for newly matched boys.
      prefM
        .filter( x => hunters.contains( x._1 ) ).toList
        .map( x => x._1 -> x._2.tail ).toMap
  }

  val is_stable: Boolean = prefM == prefMp && matches == Map( )
}
