type movement = Position.t list

type action = 
  Attack_unit of (Unit.t * Unit.t) |
  Attack_building of (Unit.t * Building.t) |
  Wait
  

