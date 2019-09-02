package matching

import matching.TypeDef._

trait Player {
  val id: PID
}

case class Boy( id: PID, plan: Plan ) extends Player {
  def is_searching( alloc: Alloc ): Boolean = {
    plan.nonEmpty &&
      !alloc.values.toList.contains( id )
  }
  def proposes_to( alloc: Alloc ): PID = {
    if ( is_searching( alloc ) ) plan.head // propose to next candidate.
    else { // or propose to current match for implementational reasons.
      val alloc_inv = alloc.toList.map( _.swap ).toMap
      alloc_inv( id )
    }
  }
  def future_plan( alloc: Alloc ): Plan = {
    if ( is_searching( alloc ) ) plan.tail
    else plan
  }
}

case class Girl( id: PID, pref: Pref, queue: Proposals = Nil ) extends Player {
  def valid_queue: List[ PID ] = pref.filter( queue.contains( _ ) )
  def has_valid_proposal: Boolean = valid_queue.nonEmpty
  def deferred_acceptance_of: PID = valid_queue.head
}

case class Proposal( boy: PID, girl: PID )

// The tail-recursive implementation requires a Game State.
case class GaleShapleyState( prefM: PMap, prefW: PMap, alloc: Alloc ) {

  // Generate objects from inputs.
  lazy val boys: List[ Boy ] = prefM.toList.map( x => Boy( x._1, x._2 ) )
  lazy val girls: List[ Girl ] = prefW.toList.map( x => Girl( x._1, x._2 ) )

  def hunters: List[ Boy ] = boys.filter( _.is_searching( alloc ) )

  // In order to append to queue, we need to represent the current allocations
  // as a List that matches None to an empty List.
  def alloc_listmap: Map[ PID, List[ PID ] ] = {
    prefW
      .keySet
      .map( x => (x, alloc.get( x ) match {
        case Some( y ) => List( y )
        case _ => Nil
      }) )
      .toMap
  }

  def matches: Alloc = {
    hunters
      .map( boy => Proposal( boy.id, boy.proposes_to( alloc ) ) )
      .groupBy( _.girl )
      .view
      .mapValues( _.map( _.boy ) )
      .toList
      .map( x => Girl( x._1, prefW( x._1 ), x._2 ++ alloc_listmap( x._1 ) ) )
      .filter( _.has_valid_proposal )
      .map( girl => (girl.id, girl.deferred_acceptance_of) )
      .filter( !alloc.toList.contains( _ ) ) // filter for pre-existing matches.
      .toMap
  }

  def prefMp: PMap = {
    prefM ++ // override preferences for newly matched boys.
      prefM
        .toList
        .filter( x => hunters.map( _.id ).contains( x._1 ) )
        .map( x => (x._1, x._2.tail) )
        .toMap
  }

  def allocp: Alloc = ( alloc ++ matches ).toList.sortBy( _._1 ).toMap

  def is_stable: Boolean = {
    prefM == prefMp &&
      alloc == allocp
  }
}
