package matching

object TypeDef {
  type Boy = Int
  type Girl = Int
  type Pref = Map[ Int, List[ Int ] ] // Preferences
  type Queue = Map[ Girl, List[ Boy ] ]
  type Alloc = Map[ Girl, Boy ] // Allocation
}
