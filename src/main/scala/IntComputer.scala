trait ParamMode
  case object PositionMode extends ParamMode
  case object ValueMode extends ParamMode

case class Param(value: Int, mode: ParamMode)

trait Op
case class Add(p1: Param, p2: Param) extends Op
case class Mul(p1: Param, p2: Param) extends Op
case class Input(to: Int) extends Op
case class Output(from: Int) extends Op


