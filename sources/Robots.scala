package myfunlaby

import com.funlabyrinthe.core.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

object Robots extends Module

@definition def robotHandlerPlugin(using Universe) = new RobotHandlerPlugin
@definition def robot5(using Universe) = new RobotXThenY
@definition def robot4(using Universe) = new RobotXThenY
@definition def robot3(using Universe) = new RobotXThenY

@definition def randomTransporter(using Universe) = new RandomTransporter

class RobotHandlerPlugin(using ComponentInit) extends PlayerPlugin:
  override def moved(context: MoveContext): Unit =
    import context.*
    if pos() == (grass: Square) then
      for robot <- universe.components[Robot] do
        if robot.position.exists(_.map == pos.map) && player.playState == CorePlayer.PlayState.Playing then
          robot.move(player)
  end moved
end RobotHandlerPlugin

abstract class Robot(using ComponentInit) extends PosComponent:
  category = ComponentCategory("robots", "Robots")

  painter += "Characters/Robot"
  zIndex = 512 // below the player
  editVisualTag = id.reverse.takeWhile(c => c >= '0' && c <= '9').reverse

  override protected def hookExecute(context: MoveContext): Unit =
    import context.*
    if player.playState == CorePlayer.PlayState.Playing then // TODO This shouldn't be needed, should it?
      player.lose()
      player.showMessage("Tu t'es jeté dans la gueule du loup !")
  end hookExecute

  def move(player: Player): Unit =
    val map = position.get.map
    val pos = position.get.pos
    val playerPos = player.position.get.pos
    val newPos = pickMove(map, pos, playerPos)
    if newPos != pos && isFree(newPos) then
      position = Some(map.ref(newPos))
      if newPos == playerPos then
        player.lose()
        player.showMessage("Tu t'es fait avoir par le robot !")
  end move

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
  /* #Robot5
   * Remplacer &Variable_1 Variable_5
   * Remplacer &Variable_2 Variable_15
   * Si Variable_1 = X Alors [Saute BonX5]
   * Si Variable_1 > X Alors [Decrementer &Variable_1] Sinon [Incrementer &Variable_1]
   * Si [Case Variable_1 Variable_2 1] = [#] Alors [Saute BonY5]
   * #BonX5
   * Remplacer &Variable_1 Variable_5
   * Si Variable_2 = Y Alors [Saute BonY5]
   * Si Variable_2 < Y Alors [Incrementer &Variable_2] Sinon [Decrementer &Variable_2]
   * #BonY5
   * Si [Case Variable_1 Variable_2 1] <> [#] Alors [Saute Robot4]
   * Remplacer Case Variable_5 Variable_15 1 #
   * Remplacer Case Variable_1 Variable_2 1 !
   * Remplacer &Variable_5 Variable_1
   * Remplacer &Variable_15 Variable_2
   * Si Variable_5 <> X Ou Variable_15 <> Y Alors [Saute Robot4]
   * Echec {Tu t'es fait avoir par le robot !}
   * Gagner
   * Stop
   */
  protected def pickMove(map: Map, pos: Position, playerPos: Position): Position =
    if pos.x != playerPos.x then
      val newPos =
        if playerPos.x < pos.x then pos +> Direction.West
        else pos +> Direction.East
      if isFree(newPos) then
        return newPos
    end if

    if pos.y != playerPos.y then
      val newPos =
        if playerPos.y < pos.y then pos +> Direction.North
        else pos +> Direction.South
      if isFree(newPos) then
        return newPos
    end if

    pos
  end pickMove
end RobotXThenY

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
