package ex3

  object Solitaire extends App:
    type Pos = (Int, Int)
    type Solution = Seq[Pos]
    type SolutionFactory = Solution => Iterable[Solution]
    val width = 5
    val height = 5
    val total = width * height
    val start: Pos = (width / 2, height / 2)
    val MAX_PRINT_SOLUTIONS = Int.MaxValue //10
    given SolutionFactory = LazyList(_)

    def render(solution: Seq[Pos]): String =
      val reversed = solution.reverse
      val rows =
        for
          y <- 0 until height
          row =
            for
              x <- 0 until width
              number = reversed.indexOf((x, y)) + 1
            yield if number > 0 then "%-2d ".format(number) else "X  "
        yield row.mkString
      rows.mkString("\n")

    def placeMarks(n: Int = total)(using factory: SolutionFactory): Iterable[Solution] = n match
      case 1 =>  factory(Seq(start))
      case _ =>
        for
          path <- placeMarks(n - 1)
          next <- nextPositions(path) // y
          //next <- nextPositions(path.head)
          //if isSafe(next, path)
        yield
          next +: path
/*
      def isInside(pos: Pos): Boolean =
        val (x, y) = pos
        x >= 0 && x < width && y >= 0 && y < height

      def nextPositions(pos: Pos): Iterable[Pos] =
        val (x, y) = pos
        List(
          (x + 3, y),
          (x - 3, y),
          (x, y + 3),
          (x, y - 3),
          (x + 2, y + 2),
          (x + 2, y - 2),
          (x - 2, y + 2),
          (x - 2, y - 2)
        ).filter(isInside)

      def isSafe(pos: Pos, path: Iterable[Pos]): Boolean =
        !path.exists(_ == pos)
*/
      // y
      def nextPositions(path: Iterable[Pos]): Iterable[Pos] =
        val (x, y) = path.head
        List(
          (x + 3, y),
          (x - 3, y),
          (x, y + 3),
          (x, y - 3),
          (x + 2, y + 2),
          (x + 2, y - 2),
          (x - 2, y + 2),
          (x - 2, y - 2)
        ).filter(isAcceptable(_, path))

      // y
      def isAcceptable(pos: Pos, path: Iterable[Pos]): Boolean =
        pos._1 >= 0 && pos._1 < width && pos._2 >= 0 && pos._2 < height && !path.exists(_ == pos)

    val solutions = placeMarks()

    println("Prime soluzioni trovate:")

    solutions.take(MAX_PRINT_SOLUTIONS).zipWithIndex.foreach { case (solution, index) =>
      println()
      println(s"Soluzione ${index + 1}:")
      println(render(solution))
    }