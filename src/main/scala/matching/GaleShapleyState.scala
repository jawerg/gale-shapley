package matching

import matching.TypeDef._

// The tail-recursive implementation requires a Game State.
case class GaleShapleyState( prefM: PMap, prefW: PMap, alloc: Alloc = Map( ) ) {

  /////////////////////////////////////////////////////////////////////////////
  // Define Boys and Girls to encapsulate their primitive actions in the game.
  case class Boy( id: PID, plan: Plan ) {
    def is_searching: Boolean = {
      plan.nonEmpty &
        !alloc.values.toList.contains( id )
    }
    def proposes_to: PID = plan.head
  }

  case class Girl( id: PID, pref: Pref, queue: Proposals = Nil ) {
    def valid_queue: List[ PID ] = pref.filter( queue.contains( _ ) )
    def has_valid_proposal: Boolean = valid_queue.nonEmpty
    def deferred_acceptance_of: PID = valid_queue.head
  }

  case class Proposal( boy: PID, girl: PID )

  /////////////////////////////////////////////////////////////////////////////
  // Translate problem description into internal objects.
  lazy val boys: List[ Boy ] = prefM.toList.map( x => Boy( x._1, x._2 ) )
  lazy val girls: List[ Girl ] = prefW.toList.map( x => Girl( x._1, x._2 ) )

  /////////////////////////////////////////////////////////////////////////////
  // Method definitions
  def hunters: List[ Boy ] = boys.filter( _.is_searching )
  def optional_match( pid: PID ): List[ PID ] = {
    alloc.get( pid ) match {
      case Some( y ) => List( y )
      case _ => Nil
    }
  }
  def alloc_listmap: Map[ PID, List[ PID ] ] = {
    prefW
      .keySet
      .map( gid => (gid, optional_match( gid )) )
      .toMap
  }

  def matches: Alloc = {
    hunters
      .map( boy => Proposal( boy.id, boy.proposes_to ) )
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

  def is_converged: Boolean = {
    prefM == prefMp &&
      alloc == allocp
  }

  // Brute Force Search for possible improvements
  // Note: Only use on plain setup.
  def improvements: List[ (PID, PID) ] = {
    val alloc_b = alloc.toList.map( _.swap ).toMap // allocations from boy POV
    for {
      g <- girls
      b <- boys
      if g.pref.indexOf( b.id ) < g.pref.indexOf( alloc( g.id ) ) &
        b.plan.indexOf( g.id ) < b.plan.indexOf( alloc_b( b.id ) )
    } yield (g.id, b.id)
  }
  def is_stable: Boolean = {
    if ( improvements.isEmpty ) true
    else false
  }
}
