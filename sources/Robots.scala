package myfunlaby

import com.funlabyrinthe.core.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object Robots extends Module:
  override protected def startGame()(using universe: Universe): Unit =
    val player = universe.players.head
    player.enqueueUnderControl { () =>
      val level = player.showSelectNumberMessage(
        "Choisis un niveau (1-5) :",
        min = 1,
        max = 5,
        default = 1,
      )
      val mapID = s"map$level"
      val map = universe.findTopComponentByID[Map](AdditionalComponents, mapID)
      val initialPos = level match
        case 1 | 2     => Position(2, 18, 0)
        case 3 | 4 | 5 => Position(2, 10, 0)
      player.reified[Player].position = Some(map.ref(initialPos))
    }
  end startGame
end Robots

@definition def robotHandlerPlugin(using Universe) = new RobotHandlerPlugin

@definition def robot5(using Universe) = new RobotXThenY
@definition def robot4(using Universe) = new RobotXThenY
@definition def robot3(using Universe) = new RobotXThenY

@definition def robot26(using Universe) = new RobotXThenYWithPatrol
@definition def robot25(using Universe) = new RobotXThenY
@definition def robot24(using Universe) = new RobotXThenY
@definition def robot23(using Universe) = new RobotXThenYWithRandomPatrol

@definition def robot37(using Universe) = new RobotXThenYWithRandomPatrol
@definition def robot36(using Universe) = new RobotXThenYWithPatrol
@definition def robot35(using Universe) = new RobotXThenY
@definition def robot34(using Universe) = new RobotXThenYWithSmartPatrol
@definition def robot33(using Universe) = new RobotXThenYWithPatrol

@definition def robot47(using Universe) = new RobotXThenYWithRandomPatrol
@definition def robot46(using Universe) = new RobotXThenY
@definition def robot45(using Universe) = new RobotXThenY
@definition def robot44(using Universe) = new RobotXThenY
@definition def robot43(using Universe) = new RobotXThenYWithGuardPatrol

@definition def robot58(using Universe) = new RobotXThenYWithGuardPatrol
@definition def robot57(using Universe) = new RobotXThenYWithRandomPatrol
@definition def robot56(using Universe) = new RobotXThenY
@definition def robot55(using Universe) = new RobotXThenY
@definition def robot54(using Universe) = new RobotXThenY
@definition def robot53(using Universe) = new RobotXThenYWithGuardPatrol

@definition def randomTransporter(using Universe) = new RandomTransporter

@definition def rockCreator(using Universe) = new RockCreator

class RobotHandlerPlugin(using ComponentInit) extends PlayerPlugin:
  override def moved(context: MoveContext): Unit =
    import context.*
    if pos() == (grass: Square) then
      for robot <- universe.components[Robot] do
        if robot.position.exists(_.map == pos.map) && player.isPlaying then
          robot.move(player)
  end moved
end RobotHandlerPlugin

abstract class Robot(using ComponentInit) extends PosComponent:
  category = ComponentCategory("robots", "Robots")

  painter += "Characters/Robot"
  zIndex = 512 // below the player
  editVisualTag = id.reverse.takeWhile(c => c >= '0' && c <= '9').reverse

  var useUnblock: Boolean = false
  var avoidCenter: Boolean = false

  var minX = 0
  var maxX = 21
  var minY = 0
  var maxY = 21

  override protected def hookExecute(context: MoveContext): Unit =
    import context.*
    if player.isPlaying then // TODO This shouldn't be needed, should it?
      player.lose()
      player.showMessage("Tu t'es jeté dans la gueule du loup !")
  end hookExecute

  def move(player: Player): Unit =
    val map = position.get.map
    val pos = position.get.pos
    val playerPos = player.position.get.pos

    val newPos =
      if useUnblock then
        tryUnblock(map, pos).getOrElse(pickMove(map, pos, playerPos))
      else
        pickMove(map, pos, playerPos)

    if newPos != pos && isFree(newPos) && (minX to maxX).contains(newPos.x) && (minY to maxY).contains(newPos.y) then
      if !(avoidCenter && newPos.x >= 4 && newPos.x <= 16 && newPos.y >= 4 && newPos.y <= 16) then
        position = Some(map.ref(newPos))
        if newPos == playerPos then
          player.lose()
          player.showMessage("Tu t'es fait avoir par le robot !")
  end move

  private def tryUnblock(map: Map, pos: Position): Option[Position] =
    val possibleMoves = Direction.values.toList.map(pos +> _).filter(isFree(_))
    if possibleMoves.sizeIs == 1 then
      Some(possibleMoves.head)
    else
      None
  end tryUnblock

  protected def pickMove(map: Map, pos: Position, playerPos: Position): Position

  protected def isFree(pos: Position): Boolean =
    val map = position.get.map
    val square = map(pos)
    square == (grass: Square)
      && !map.posComponentsBottomUp(pos).exists(!_.isInstanceOf[Player])
  end isFree
end Robot

class RobotXThenY(using ComponentInit) extends Robot:
  var yThenX: Boolean = false

  protected def pickMove(map: Map, pos: Position, playerPos: Position): Position =
    tryFollowPlayer(map, pos, playerPos)
  end pickMove

  protected def tryFollowPlayer(map: Map, pos: Position, playerPos: Position): Position =
    if yThenX then
      tryMoveY(map, pos, playerPos)
        .orElse(tryMoveX(map, pos, playerPos))
        .getOrElse(pos)
    else
      tryMoveX(map, pos, playerPos)
        .orElse(tryMoveY(map, pos, playerPos))
        .getOrElse(pos)
  end tryFollowPlayer

  private def tryMoveX(map: Map, pos: Position, playerPos: Position): Option[Position] =
    if pos.x != playerPos.x then
      val newPos =
        if playerPos.x < pos.x then pos +> Direction.West
        else pos +> Direction.East
      if isFree(newPos) then
        Some(newPos)
      else
        None
    else
      None
  end tryMoveX

  private def tryMoveY(map: Map, pos: Position, playerPos: Position): Option[Position] =
    if pos.y != playerPos.y then
      val newPos =
        if playerPos.y < pos.y then pos +> Direction.North
        else pos +> Direction.South
      if isFree(newPos) then
        Some(newPos)
      else
        None
    else
      None
  end tryMoveY
end RobotXThenY

def manhattanDist(p1: Position, p2: Position): Int =
  val diff = p1 - p2
  diff.x.abs + diff.y.abs + diff.z.abs

class RobotXThenYWithPatrol(using ComponentInit) extends RobotXThenY:
  var maxPursuitDistance: Int = 4
  var direction: Direction = Direction.North
  var directions: List[Direction] = Nil

  override protected def pickMove(map: Map, pos: Position, playerPos: Position): Position =
    if manhattanDist(pos, playerPos) <= maxPursuitDistance then
      tryFollowPlayer(map, pos, playerPos)
    else
      patrol(map, pos, playerPos)
  end pickMove

  protected def patrol(map: Map, pos: Position, playerPos: Position): Position =
    val newPos = pos +> direction
    if isFree(newPos) then
      newPos
    else
      // Choose new direction
      direction = directions.indexOf(direction) match
        case -1  => direction.opposite // turn around
        case idx => directions((idx + 1) % directions.size) // cycle
      pos +> direction
  end patrol
end RobotXThenYWithPatrol

class RobotXThenYWithRandomPatrol(using ComponentInit) extends RobotXThenYWithPatrol:
  override protected def patrol(map: Map, pos: Position, playerPos: Position): Position =
    pos +> Direction.values(scala.util.Random.nextInt(Direction.values.length))
end RobotXThenYWithRandomPatrol

class RobotXThenYWithSmartPatrol(using ComponentInit) extends RobotXThenYWithPatrol:
  override protected def patrol(map: Map, pos: Position, playerPos: Position): Position =
    if pos.x > 3 || pos.y < 4 || pos.y > 16 then
      tryFollowPlayer(map, pos, playerPos)
    else
      if pos.y < 10 then
        pos +> Direction.North
      else
        pos +> Direction.South
  end patrol
end RobotXThenYWithSmartPatrol

class RobotXThenYWithGuardPatrol(using ComponentInit) extends RobotXThenYWithPatrol:
  override protected def patrol(map: Map, pos: Position, playerPos: Position): Position =
    if pos.x >= 17 then
      // On the right; go to vertical center
      if pos.y < 10 then
        pos +> Direction.South
      else if pos.y > 10 then
        pos +> Direction.North
      else
        pos
    else
      if pos.x < 4 && pos.y >= 4 && pos.y <= 16 then
        // On the inner left; go to vertical exterior
        if pos.y > 10 then
          pos +> Direction.South
        else
          pos +> Direction.North
      else
        // On top or bottom; go right
        pos +> Direction.East
  end patrol
end RobotXThenYWithGuardPatrol

class RandomTransporter(using ComponentInit) extends Effect:
  category = ComponentCategory("transporters", "Transporters")

  painter += "Transporters/Transporter"
  editVisualTag = "✥"

  override def execute(context: MoveContext): Unit =
    import context.*

    player.direction = Some(Direction.values(scala.util.Random.nextInt(4)))
    goOnMoving = true
  end execute
end RandomTransporter

final class RockCreator(using ComponentInit) extends ComponentCreator[Rock]:
  category = ComponentCategory("rocks", "Rocks")

  icon += "Rocks/BigRock"
  icon += "Creators/Creator"
end RockCreator

class Rock(using ComponentInit) extends PosComponent:
  category = ComponentCategory("rocks", "Rocks")

  painter += "Rocks/BigRock"

  override protected def hookPushing(context: MoveContext): Unit =
    import context.*

    val behind = pos +> player.direction.get
    if keyEvent.isEmpty || behind() != grass.toSquare || behind.map.posComponentsBottomUp(behind.pos).nonEmpty then
      cancel()
    else
      position = Some(behind)
  end hookPushing
end Rock
