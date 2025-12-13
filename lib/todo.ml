type location = { file : string; line : int; column : int }
type t = { location : location; marker : Marker.t; urgency : int; msg : string }
