sealed trait Mark {
  val value: Int
}

case object EMPTY extends Mark {
  val value: Int = 0
}

case object O extends Mark {
  val value: Int = -1
}

case object X extends Mark {
  val value: Int = 1
}
