package myfunlaby

import com.funlabyrinthe.core.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object Robots extends Module:
  override protected def startGame()(using universe: Universe): Unit =
    val player = universe.players.head
    player.enqueueUnderControl { () =>
      val level = player.showSelectNumberMessage(
        "Choisis un niveau (1-2) :",
        min = 1,
        max = 2,
        default = 1,
      )
      val mapID = s"map$level"
      val map = universe.findTopComponentByID[Map](AdditionalComponents, mapID)
      player.reified[Player].position = Some(map.ref(Position(2, 18, 0)))
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

@definition def randomTransporter(using Universe) = new RandomTransporter

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

    if newPos != pos && isFree(newPos) then
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

  /*protected def attempts(newPosAttempts: (() => Position)*): Position =
    newPosAttempts.iterator
      .map(_())
      .find(isFree(_))
      .getOrElse(position.get.pos)*/
end Robot

class RobotXThenY(using ComponentInit) extends Robot:
  var yThenX: Boolean = false

  protected def pickMove(map: Map, pos: Position, playerPos: Position): Position =
    if yThenX then
      tryMoveY(map, pos, playerPos)
        .orElse(tryMoveX(map, pos, playerPos))
        .getOrElse(pos)
    else
      tryMoveX(map, pos, playerPos)
        .orElse(tryMoveY(map, pos, playerPos))
        .getOrElse(pos)
  end pickMove

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

  override protected def pickMove(map: Map, pos: Position, playerPos: Position): Position =
    if manhattanDist(pos, playerPos) <= maxPursuitDistance then
      super.pickMove(map, pos, playerPos)
    else
      // Patrol
      val newPos = pos +> direction
      if isFree(newPos) then
        newPos
      else
        // Turn around
        direction = direction.opposite
        pos +> direction
  end pickMove
end RobotXThenYWithPatrol

class RobotXThenYWithRandomPatrol(using ComponentInit) extends RobotXThenYWithPatrol:
  override protected def pickMove(map: Map, pos: Position, playerPos: Position): Position =
    direction = Direction.values(scala.util.Random.nextInt(Direction.values.length))
    super.pickMove(map, pos, playerPos)
  end pickMove
end RobotXThenYWithRandomPatrol

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
