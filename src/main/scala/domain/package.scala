import scala.util.{Failure, Success, Try}

package object domain {
  type OutputNum = Int
  type Chip      = Int
  type BotId     = Int

  sealed trait Destination
  case class Output(ind: Int)       extends Destination
  case class NextBot(botNum: BotId) extends Destination
  object Destination {
    def from(dest: String, id: Int): Try[Destination] = {
      if (dest.equalsIgnoreCase("bot")) {
        Success(NextBot(id))
      } else if (dest.equalsIgnoreCase("output")) {
        Success(Output(id))
      } else Failure(new IllegalArgumentException("Unknown destination"))
    }
  }

  sealed trait Command
  case class MoveChipTo(c: Chip, dest: Destination)                            extends Command
  case class PassChipsToNext(from: BotId, low: Destination, high: Destination) extends Command

  trait BotError
  case class CannotAccept(msg: String)        extends BotError
  case class UnsupportedBotEvent(msg: String) extends BotError
}
