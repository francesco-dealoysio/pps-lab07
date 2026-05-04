package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)


class RobotWithBattery(val robot: Robot, var battery: Int, val cost: Int) extends Robot:
  export robot.{position, direction, turn}

  override def act(): Unit =
    if battery >= cost then
      robot.act()
      battery -= cost
    else
      println("Battery empty!")

  override def toString: String = s"${robot.toString} (battery=$battery)"
  
  
class RobotCanFail(val robot: Robot, val failProb: Double) extends Robot:
    export robot.{position, direction, turn}

    override def act(): Unit =
      if Random.nextDouble() >= failProb then
        robot.act()
      else
        println("Action failed!")

    override def toString: String = s"${robot.toString} (can fail)"
  
class RobotRepeated(val robot: Robot, val times: Int) extends Robot:
    export robot.{position, direction, turn}

    override def act(): Unit =
      for _ <- 1 to times do
        robot.act()

    override def toString: String = s"${robot.toString} (repeated $times times)"



@main def testRobot(): Unit =
  import ex2.*
  
  val robot =
    LoggingRobot(
      RobotRepeated(
        RobotWithBattery(
          RobotCanFail(
            SimpleRobot((0, 0), Direction.North),
            0.2
          ),
          battery = 5,
          cost = 1
        ),
        times = 2
      )
    )

  robot.act()
  robot.turn(robot.direction.turnRight)
  robot.act()
  robot.act()
/*
@main def testRobot(): Unit =
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
*/