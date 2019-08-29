type Boy = Int
type Girl = Int
type Pref = Map[ Int, List[ Int ] ] // Preferences
type Queue = Map[ Girl, List[ Boy ] ]
type Alloc = Map[ Girl, Boy ] // Allocation

// Example.
val prefM: Pref = Map[ Boy, List[ Girl ] ](
  1 -> List( 1, 2, 3, 4 ),
  2 -> List( 1, 4, 3, 2 ),
  3 -> List( 2, 1, 3, 4 ),
  4 -> List( 4, 2, 3, 1 )
)
val prefW: Pref = Map[ Girl, List[ Boy ] ](
  1 -> List( 4, 3, 1, 2 ),
  2 -> List( 2, 4, 1, 3 ),
  3 -> List( 4, 1, 2, 3 ),
  4 -> List( 3, 2, 1, 4 )
)
val alloc: Alloc = Map( )

//
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

  val choices: Queue = queue.toList.map( x => (x._1, x._2 ++ alloc.get( x._1 )) ).toMap

  val matches: Alloc = {
    choices.toList
      .map( x => x._1 -> x._2.toSet.intersect( prefW( x._1 ).toSet ).toList ) // restrict queue valid proponents
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

@scala.annotation.tailrec
def GaleShapleyIter( gss: GaleShapleyState ): Alloc = {
  if ( gss.is_stable ) gss.alloc
  else GaleShapleyIter( GaleShapleyState( gss.prefMp, gss.prefW, gss.alloc ++ gss.matches ) )
}

def GaleShapleySolver( prefM: Pref, prefW: Pref ): Alloc = {
  val alloc: Alloc = Map( )
  val GSS0 = GaleShapleyState( prefM, prefW, alloc )
  GaleShapleyIter( GSS0 )
}

GaleShapleySolver( prefM, prefW )