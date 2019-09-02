package matching

object TypeDef {
  type PID = Int

  type Pref = List[ PID ] // Preferences
  type Plan = List[ PID ] // Order of future proposals.
  type Proposals = List[ PID ]

  // Define a Preference / Plan / Proposal Mapping to describe actions.
  type PMap = Map[ PID, Pref ]

  // Matches are allocations of Girls to Boys.
  type Alloc = Map[ PID, PID ] // Allocation
}
