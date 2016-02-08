sealed abstract class IsPossible

case object NoTooEarly extends IsPossible
case object NoTooLate extends IsPossible
case object NoTooLittleSpace extends IsPossible

case object NoNotForThis extends IsPossible
case object NoInvalid extends IsPossible

case object YesPossible extends IsPossible
case object YesCertain extends IsPossible
